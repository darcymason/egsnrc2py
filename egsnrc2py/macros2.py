import re
from typing import Tuple, List
from pprint import pprint
from textwrap import dedent

from egsnrc2py.util import nested_brace_value, fix_identifiers
from egsnrc2py.config import (
    default_float, AUTO_TRANSPILE_PATH, MORTRAN_SOURCE_PATH,
    INTEGER, REAL, LOGICAL
)

import logging

logger = logging.getLogger('egsnrc2py')


warning_line = '***************** Warning: '
error_line = '***************** Error: '
quit_line = '***************** Quitting now.'

logging_replace = {
    '$egs_debug(#,#);': 'logger.debug({P2})',
    '$egs_info(#,#);': 'logger.info({P2})',
    '$egs_warning(#,#);': f"logger.warning('{warning_line}'\n{{P2}}",
    '$egs_fatal(#,#,#);': dedent(
        f"""logging.critical('{error_line}')\n
        logging.critical('{{P2}}')\n
        logging.critical('{quit_line}')\n
        sys.exit({{P3}})
        """
    ),
    '$egs_fatal(#,#);': dedent(
        f"""logging.critical('{error_line}')\n
        logging.critical('{{P2}}')\n
        logging.critical('{quit_line}')\n
        sys.exit(1)
        """
    ),
}

# Function signatures: func_name: (input args names, outputs),
# for outputs that are a number, the corresponding arg name (0-based) is returned
#  If a name, then that name is returned
func_signatures = {
    # $COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,range)
    # $COMPUTE_DRANGE(ekei,ekef,lelkef,elkei,elkef,tstep)
    # $COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,tuss)
    'compute_drange': ("eke1,eke2,lelke1,elke1,elke2".split(","), (5,))

}
class MacrosAndCode:
    """Hold macro processing code and ensure called in proper sequence"""
    max_lines_for_inlining = 10  # max lines in a macro for putting inline

    def __init__(self, macros_code, source_code, recurse=True):
        """Parse macros code and classify different types for later use"""
        # First, take dashes out of identifiers
        self.macros_code = self.orig_macros_code = fix_identifiers(macros_code)
        self.source_code = fix_identifiers(source_code)
        self.recurse = recurse

        self.imports = set()
        self.callbacks = set()
        self.empty_callbacks = set()

        # Change all PARAMETERS x = y macros back to REPLACE..WITH
        self._replace_PARAMETERs()

        # Find all REPLACE ... WITH -> store in self.all_from_to
        self._map_replace_from_to()

        # Change error, warning, info, debug macros to Python logging
        self._replace_logging()

        # Classify macros to called, constant, defined_block, other
        #   This uses the macros code and the source code - latter to see
        #   how they are used
        self._macro_types()

        # For simple constants and parameters
        #   find, replace with own name without the "$" sign
        self._process_parameters()

        # Replace $LOGICAL, $REAL etc.
        self._replace_var_decl()

        # Inline functions, check vs None if empty, or large ones go to top
        # Creates self.callbacks_code
        self._replace_macro_callables()

        self._replace_eval_using()  # needs self.callback_code above

        # self._replace_others()

        # Now recurse so replace any macros that were in previous replacements
        #   but only if this is top level, not already recursed into
        # Note this is really inefficient, would be better to recurse only on
        # the macro replacement value, but this is more complete and since
        # only run 'once', it is not worth trying to over-optimize
        if self.recurse:
            while True:
                sub_macros = MacrosAndCode(
                    self.macros_code, self.source_code, recurse=False
                )
                # Get out if no changes were made
                if sub_macros.source_code == self.source_code:
                    break
                self.source_code = sub_macros.source_code
                self.imports.update(sub_macros.imports)
                self.callbacks.update(sub_macros.callbacks)
                self.empty_callbacks.update(sub_macros.empty_callbacks)

    def _replace_logging(self):
        """Replace egs_info, egs_warning etc with Python-style logging"""
        for m_from, m_to in logging_replace.items():
            if m_from not in self.all_from_to and self.recurse: # top level only
                logger.error(
                    f"`logging_replace` macro '{m_from}' not found in list"
                )
            self.all_from_to[m_from] = m_to


    def _replace_PARAMETERs(self) -> str:
        """Replace PARAMETER macros with REPLACE..WITH

        This just makes it easier to treat them along with other replaces.
        """
        logger.info("Replacing PARAMETER macros with REPLACE..WITH")
        pattern = r"^( *)PARAMETER\s*(.*?)\s*?=\s*?(.*?);(.*)$"
        repl_with = r"\1REPLACE {\2} WITH {\3}\4"
        self.macros_code = re.sub(
            pattern, repl_with, self.macros_code, flags=re.MULTILINE
        )

    @property
    def all_from_to(self):
        return self._all_from_to

    def _map_replace_from_to(self) -> dict:
        """Goes through the .macros `code` and determines all REPLACE .. WITH"""
        logger.info("Mapping all macros from->to")
        all_di = {}
        # Note WITH can be followed by a comment before the opening {
        pattern = r"^ *REPLACE\s*\{(.*?)\}\s*?WITH\s*?(\".*?\"\s*)?\{"
        i = 0
        re_pattern = re.compile(pattern, re.MULTILINE)
        # subcode = code  # need to update search string to exclude REPLACE in val
        while True:
            match = re_pattern.search(self.macros_code, i)
            if not match:
                break
            replace_from = match.group(1)
            replace_to = nested_brace_value(self.macros_code, match.end())
            if replace_from in all_di:
                orig_to = all_di[replace_from]
                if len(orig_to) < 70 and len(replace_to) < 70:
                    details =  f" from '{orig_to}' to '{replace_to}'"
                else:
                    details = " (definitions too long to print)"
                logger.warning(
                    f"Repeat definition of macro '{replace_from}'{details}"
                )
            all_di[replace_from] = replace_to
            # print(replace_from, " -> ", replace_to)
            i = match.end() + len(replace_to) + 1  # one extra for }

        # Now create regular expression versions of all of the macros

        self._all_from_to = all_di

    def _macro_types(self):
        """Scan through user code (not macro code) to check if macros are
        ever assigned, or called

        Note: search with r"\$\w*?\s*?=" in full egsnrc.mortran
        found no assigned <$var =>'s, except in string printouts,
        so if not called, then are constant

        """
        self.called = []
        self.constant = []
        self.defined_block = []
        self.complex = []
        self.other = []

        for m_from, m_to in self.all_from_to.items():
            # See if called - if alone on a line (except comments):
            if "[IF]" in m_to:
                self.complex.append((m_from, m_to))
                continue
            if m_from.startswith(("$COMIN", ";COMIN", "$DEFINE", "$DECLARE")):
                self.defined_block.append((m_from, m_to))
                continue

            if m_from.startswith(("$COMPUTE", "$CALC", "$UPDATE")):
                self.called.append((m_from, m_to))
                continue

            macro_str = escape(m_from)
            # pattern <indent>[;]$macro then ; or (args);
            alone_pattern = rf'^[ ;]*{macro_str}\s*?(["#].*?$)?;?'
            if re.search(alone_pattern, self.source_code, flags=re.MULTILINE):
                self.called.append((m_from, m_to))
            # See if has a open bracket right after it
            # pattern = rf"\W{macro}\s*?\("
            # if re.search(pattern, code, flags=re.MULTILINE):
            #     self.called.append(macro)
            else:
                if get_type(m_to) is None:
                    self.other.append((m_from, m_to))
                else:
                    self.constant.append((m_from, m_to))


    def _replace_var_decl(self):
        mapping = {
            "$INTEGER": INTEGER,
            "$REAL": REAL,
            "$LOGICAL": LOGICAL,
            "LOGICAL": LOGICAL,
        }

        self.imports.add("import numpy as np\n")
        out_lines = []
        for line in self.source_code.splitlines():
            matched = False
            for typ in ["$INTEGER", "$REAL", "$LOGICAL", "LOGICAL"]:
                if line.startswith(typ):
                    vars = line.replace(typ, "").split(",")
                    for var in vars:
                        out_lines.append(
                            f"{var.strip().replace(';', '')}: {mapping[typ]}"
                        )
                    matched = True
                    break # out of inner loop
            if not matched:
                out_lines.append(line)

        self.source_code = "\n".join(out_lines)

    def _process_parameters(self):
        """Determine macros with our custom (partial) replacements for "constant"
        or "parameter" style macros

        Sets
        -------
        self.new_macros_code
            The original .macros code modified with some changes
        self.parameters
            A list of (varname, type, value) for the output Python parameters file

        """

        # Go through the macros code, modifying the "REPLACE ... WITH" macro "with"
        # part with the macro name minus the $.  This becomes a named "constant"
        # (really a parameter) in a python parameters file
        # We do this in the macros themselves in case we don't fully consume
        #   them, and use Mortran to do substitutions later for us.
        # We also do in the source code.
        # generate sorted list - largest first in case some names are subset of others
        new_macros = self.macros_code
        new_source = self.source_code
        parameters = []
        # sorted_from = sorted(self.all_from_to, key=lambda x: len(x), reverse=True)
        constant_set = set(mac[0] for mac in self.constant)  # faster lookup than a list
        for m_from, m_to in self.all_from_to.items():
            if m_from in constant_set:
                _type = get_type(m_to)

                # doubled {{ and }} needed for f-string to give a single
                escaped_from = m_from.replace('$',r'\$')
                pattern = r"^( *)REPLACE\s*?\{{{_from}\}}\s*?WITH\s*?\{{{to}\}}"
                to_pattern = r"\1REPLACE {{{_from}}} WITH {{{to}}}"
                from_pattern = pattern.format(_from=escaped_from, to = m_to)
                bare_name = m_from.replace("$", "")  # 'from' name without $
                to_pattern = to_pattern.format(_from=m_from, to=bare_name)

                # Replace macro "WITH" part with new name
                new_macros = re.sub(from_pattern, to_pattern, new_macros, flags=re.MULTILINE)

                # Replace the macro in the source code
                new_source = re.sub(escaped_from, bare_name, new_source, flags=re.MULTILINE)
                # Store parameters for later reference, and to produce output file
                # This is for Python, so convert bool types to Python
                if m_to == ".false.":
                    m_to = "False"
                elif m_to == ".true.":
                    m_to = "True"
                parameters.append((bare_name, _type, m_to)) # original constant val for params
            # try:
            #     int(m_to)
            #     macros_code = re.sub()
            # except TypeError:
            #     try:
            #         float(m_to)
            #     except TypeError:
            #         continue
        self.macros_code = new_macros
        self.source_code = new_source
        self.parameters = parameters

    def write_params_file(self, filename) -> None:
        with open(filename, "w") as f:
            f.write("import numpy as np\n\n")
            for name, _type, value in self.parameters:
                f.write(f"{name}: {_type} = {value}\n")

    def write_callbacks_file(self, filename) -> None:
        with open(filename, "w") as f:
            f.write(self.callbacks_code)

    def _generate_callbacks_code(self):
        cb_list = []
        for name, args, repl in sorted(self.callbacks):
            func_args, return_vars = func_details(name, args)
            cb_list.append(
                f"def {name}({func_args}):\n" + "\n    ".join(repl.splitlines())
            )
        callbacks = "\n# CALLBACKS ---- \n" + "\n".join(cb_list) + "\n\n"

        empty_callbacks = "\n# EMPTY CALLBACKS ----\n" + "\n".join(
            f"{cb} = None"
            for cb in sorted(self.empty_callbacks)
        ) + "\n\n"

        self.callbacks_code = empty_callbacks + callbacks
        self.imports.add("\nfrom egsnrc.callbacks import *")

    def macro_replaced_source(self) -> str:
        """The final source code to write out"""
        imports = "# IMPORTS -------\n" + "\n".join(sorted(self.imports))
        return imports + self.callbacks_code + self.source_code

    def _replace_macro_callables(self):
        """Macros that are callable replaced with (may be optional) call"""
        def replace_empty_fn(match):
            """Make lower-case names"""
            indent = match.group(1)
            func_name = match.group(2).lower()
            args = match.group(3) or ""
            pre_comment = f"{indent}# --- Inline empty replace: {match.group(0).strip()} -----\n"
            post_comment = f"\n{indent}# " + "-" * len(pre_comment.strip()) + "\n"
            fn_check = f"{indent}if {func_name}:\n{indent}    {func_name}({args})"
            return pre_comment + fn_check + post_comment

        def inline_replace_fn(match):
            indent = match.group(1)
            func_name = match.group(2).lower()
            args = match.group(3) or ""

            # repl is replacement value in outer function
            lines = [f"\n{indent}else:"] + [
                f"{indent}    {line}"
                for line in dedent(repl).splitlines()
            ]
            # Visually bracket the replacement with comments
            # Still use `if` check against none, so user can replace if desired
            # the `else:` clause is the "inlined" code
            # remove the '$' and ';' if there so doesn't match in recursive replaces
            macro = match.group(0).strip().replace(";","").replace("$", "")
            pre_comment = f"{indent}# --- Inline replace: {macro} -----\n"
            fn_check = f"{indent}if {func_name}:\n{indent}    {func_name}({args})"
            post_comment = f"\n{indent}# " + "-" * len(pre_comment.strip()) + "\n"
            return pre_comment + fn_check + "\n".join(lines) + post_comment

        def non_inline_replace_fn(match):
            nonlocal args, re_from, re_to, repl
            indent = match.group(1)
            func_name = match.group(2).lower()
            args = match.group(4)
            if args:
                args = args.split(",")
            else:
                args = ""
            func_args, return_vars = func_details(func_name, args)
            if args and func_args:
                args = args[:len(func_args)] # cut to number of input args
            ret_str = ""
            if return_vars:
                ret_str = ",".join(return_vars) + " = "
            macro = match.group(0).strip().replace(";","").replace("$", "")
            pre_comment = f"{indent}# --- Replace: {macro} -----\n"
            args_str = ", ".join(args)
            return pre_comment + f"{indent}{ret_str}{func_name}({args_str})"

        # Replace macros, but largest first, because can share same start
        args = ""
        sorted_called = sorted(self.called, key=lambda x:len(x[0]), reverse=True)
        for macro, repl in sorted_called:
            # Note, next line assumes `fix_identifiers` has already been run
            re_from, re_to = re_from_to(macro, repl)
            macro_str = macro.replace("-", "_").replace("$", "").replace(";", "")
            pattern = rf'^( *)\$({macro_str})\s*?(\((.*)\))?;?' # \s*(\".*?)$ comment
            # match = re.search(pattern, code, flags=re.MULTILINE)
            # if match:
            #     print(f"Matched {pattern}")

            # XXX need to replace macros with repl too.  Many point to a ; replacement
            if repl.strip() == ";":  # blank call-back
                self.source_code = re.sub(
                    pattern, replace_empty_fn, self.source_code, flags=re.MULTILINE
                )
                self.empty_callbacks.add(macro_str.lower())
            elif len(repl.splitlines()) <= self.max_lines_for_inlining:
                self.source_code = re.sub(
                    pattern, inline_replace_fn, self.source_code, flags=re.MULTILINE
                )
                self.empty_callbacks.add(macro_str.lower())
            else:
                # Replace to call the function
                subst = r"\1\2(\3)"
                self.source_code, num_subs = re.subn(
                    pattern, non_inline_replace_fn, self.source_code, flags=re.MULTILINE
                )
                # repl updated in re.sub called function above to replace args
                if num_subs:
                    self.callbacks.add((macro_str.lower(), args, repl))
        self._generate_callbacks_code()

    def _replace_others(self):
        sorted_others = sorted(self.other, key=lambda x:len(x[0]), reverse=True)
        for m_from, m_to in sorted_others:
            re_from, re_to = re_from_to(m_from, m_to)
            eval1pattern = re.compile(re_from, flags=re.MULTILINE)
            self.source_code = eval1pattern.sub(re_to, self.source_code)


    def _replace_eval_using(self):
        """Custom code for common macro"""
        def repl1arg(match):
            # $EVALUATE#USING#(#);
            P1, P2, P3 = match.groups()
            if P2.lower().strip() == "sin":
                eval_expr = f"{P1}=sin({P2})"
            elif P2.lower() in 'sinc blc rthr rthri'.split():
                eval_expr = f"{P1}={P2}1[L{P3}]*{P3}+{P2}0[L{P3}]"
            else:
                eval_expr = f"{P1}={P2}1[L{P3},MEDIUM]*{P3}+{P2}0[L{P3},MEDIUM]"
            return eval_expr + f"  # EVALUATE{P1}USING{P2}({P3})"  # match.group(0) caused inf recursion

        eval1arg, _ = re_from_to("$EVALUATE#USING#(#);", "")
        eval1pattern = re.compile(eval1arg, flags=re.MULTILINE)
        self.source_code = eval1pattern.sub(repl1arg, self.source_code)
        self.callbacks_code = eval1pattern.sub(repl1arg, self.callbacks_code)
        # REPLACE {} WITH {
        # [IF] '{P2}'=SNAME1
        # [{P1}={P2}1(L{P3})*{P3}+{P2}0(L{P3});] [ELSE]
        # [{P1}={P2}1(L{P3},MEDIUM)*{P3}+{P2}0(L{P3},MEDIUM);]}

        # REPLACE {$EVALUATE#USING#(#,#);} WITH {
        # {P1}={P2}0(L{P3},L{P4})+{P2}1(L{P3},L{P4})*{P3}+
        # {P2}2(L{P3},L{P4})*
        # {P4};}"2-D APPROXIMATION INDEPENDENT OF MEDIUM"
        # SPECIFY SNAME AS ['sinc'|'blc'|'rthr'|'rthri'|'SINC'|'BLC'|'RTHR'|'RTHRI'];
        # SPECIFY SNAME1 AS ['sin'|'SIN'];

    def _replace_set_interval(self):
        # REPLACE {$SET INTERVAL#,SINC;} WITH {;}
        # REPLACE {$SETINTERVAL#,#;} WITH {     -- no blank between words!?
        # [IF] '{P2}'=SNAME  [L{P1}={P2}1*{P1}+{P2}0;]
        # [ELSE]  [L{P1}={P2}1(MEDIUM)*{P1}+{P2}0(MEDIUM);]}
        pass


def find_all_macros_used(code):
    """Return all identifiers starting with $ in the code"""
    pattern = r" *?(\$[\w-]+)" #r"^ *?(\$[-\w]*)"
    matches = re.findall(pattern, code)
    return set(matches)


def escape(s) -> str:
    """To clean up the "from side" for regex searching"""
    return re.escape(s)
    # for c in ["$", "[", "+"]:
    #     s = s.replace(c, r"\c")
    # return s


def generate_macros_py(filename:str, code: str) -> None:
    """Generate py code with info used by the transpile code

    e.g. lists of macros that are const vs callables
    code should be full egsnrc.mortran to capture full information
    """
    constant_macros, called_macros, defined_block_macros = macro_types(code)

    with open(filename, 'w') as f:
        f.write("# autogenerated by _util.generate_macros_py")
        for name, _list in [
            ("constant_macros", constant_macros),
            ("called_macros", called_macros),
            ("defined_blocks_macros", defined_block_macros)
        ]:
            f.write("\n\n")
            f.write(f"{name} = [\n")
            f.write(
                "".join(f"    '{name}',\n" for name in sorted(_list))
            )
            f.write("]")


def get_type(m_to):
    try:
        int(m_to)
        return INTEGER
    except ValueError:
        try:
            float(m_to)
            return REAL
        except ValueError:
            if m_to in [".false.", ".true."]:
                return LOGICAL
    return None


def re_from_to(m_from, m_to) -> Tuple[str, str]:
    """Make a particular from->to macro a `re` reg expr search pattern

    E.g. Replace '#' with '.*?' and in "to", {P1} etc with the group match num
    """
    # XXX usefulness is limited due to not replacing indents
    re_from = re.escape(m_from).replace(r"\#", "(.*?)")

    # Set up replace of {P1} etc if "from" has arguments
    # Some "to"'s (e.g. $CALLTRACE) have REPLACEs inside them,
    # and should not update those until next time macros run.
    # These are bracketed like "{WAIT {P1}}"
    re_to = m_to
    # ?<! is a negative look-behind.  Don't match if that text precedes
    re_to = re.sub(r"(?<!WAIT )\{P(\d*)\}", r"\\g<\1>", re_to)  # {Px} -> \x group replace
    re_to = re.sub(r"\{WAIT \{ARB\}\}", "#", re_to)
    re_to = re.sub(r"\{WAIT \{P(\d*)\}\}", r"{P\1}", re_to)
    return re_from, re_to


def func_details(name, args:list) -> Tuple[list, list]:
    # Return the args used in the function definition (not the call)
    # and the name of vars to be returned
    bare_name = name.replace("$", "").replace(";", "")
    if bare_name in func_signatures:
        func_args, func_outputs = func_signatures[bare_name]
        return_vars = [
            args[out] if isinstance(out, int) else out
            for out in func_outputs
        ]
    else:
        func_args = [f"arg{i}" for i in range(len(args))]
        if func_args:
            return_vars = ["<XXX>"]  # will be syntax error, forcing hand-edit
        else:
            return_vars = []
    return func_args, return_vars


# Note WITH can be followed by a comment before the opening {
replace_with_pattern_str = r"REPLACE\s*\{(.*?)\}\s*?WITH\s*?(\".*?\"\s*)?\{"
replace_with_pattern = re.compile(replace_with_pattern_str, re.MULTILINE)


def parse_and_apply_macros(code: str, macros: dict) -> str:
    """Parse code, collecting and applying macros in order

    Parameters
    ----------
    code: str
        The macros AND the source code they operate on

    macros: dict
        List of re-compiled previously parsed macros: replacement pairs
    """

    def sub_func(match):
        nonlocal re_with, macros
        # Act on macros and replaces inside out replacement value
        expanded = match.expand(re_with)
        expanded = parse_and_apply_macros(expanded, macros)
        return expanded

    while True:
        # Find either a REPLACE...WITH macro
        # or the first match to existing macros, whichever is first

        rev_macros = sorted(macros, key=lambda x: len(x), reverse=True)

        i_first = 1e9  # stays there if no match
        repl_match = replace_with_pattern.search(code)
        if repl_match:
            i_first = repl_match.start()

        i_match = None
        for i, m_from in enumerate(rev_macros):
            compiled_from = macros[m_from][0]
            match = compiled_from.search(code)
            if match and match.start() < i_first:
                i_first = match.start()
                i_match = i

        # Now see what we found
        if i_first == 1e9:  # no match, we're done
            break

        if i_match is not None:  # then found a match to a macro we need to expand
            logger.debug(f"Found match to {rev_macros[i_match]} at pos {i_first} ")
            macro_from = rev_macros[i_match]
            re_replace, re_with = macros[macro_from]
            code = re_replace.sub(sub_func, code, count=1)
        else:  # found a REPLACE macro to add to our list
            logger.debug(f"Found REPLACE macro def'n at pos {i_first} ")
            match = repl_match
            m_replace = match.group(1)
            m_with = nested_brace_value(code, match.end())
            i_start = match.start()
            i_end = match.end() + len(m_with) + 1  # one extra for }
            code = code[:i_start] + code[i_end:]
            if m_replace in macros:
                orig_with = macros[m_replace]
                if len(orig_with) < 70 and len(m_with) < 70:
                    details =  f" from '{orig_with}' to '{m_with}'"
                else:
                    details = " (definitions too long to print)"
                logger.warning(
                    f"Repeat definition of macro '{m_replace}'{details}"
                )
            re_replace, re_with = re_from_to(m_replace, m_with)
            macros[m_replace] = (re.compile(re_replace), re_with)
        # print(m_replace, " -> ", m_with)

    return code





if __name__ == "__main__":

    in_filename = MORTRAN_SOURCE_PATH / "electr.mortran"
    with open(in_filename, 'r') as f:
        code = f.read()

    with open(MORTRAN_SOURCE_PATH / "egsnrc.macros", 'r') as f:
        macros_code = f.read()

    macros_code = dedent("""REPLACE {PARAMETER #=#;} WITH
   { REPLACE {{P1}} WITH {{P2}}}

PARAMETER $MXXXX=400;     "GAMMA SMALL ENERGY INTERVALS"
x = $MXXXX;

PARAMETER $MXXXX=1;
y = $MXXXX;
z = $MXSGE;
    """
    )

    macros = {}
    code = parse_and_apply_macros(macros_code, macros)
    print("Code\n----\n", code)
    print("# macros:", len(macros))
    # pprint(macros)
    # pprint(code)