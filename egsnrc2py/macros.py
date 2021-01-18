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


commons = {}
empty_callbacks = set()
imports = set()
macros = {}
parameters = {}


NUMBER_TYPES = (INTEGER, REAL, LOGICAL)
# Macro Types
FUNCTION, COMMON, PARAMETER, COMPLEX = list(range(4))


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

def write_params_file(self, filename) -> None:
    with open(filename, "w") as f:
        f.write("import numpy as np\n\n")
        for name, _type, value in parameters:
            f.write(f"{name}: {_type} = {value}\n")

    def write_callbacks_file(self, filename) -> None:
        with open(filename, "w") as f:
            f.write(self.callbacks_code)

def _generate_callbacks_code():
    cb_list = []
    for name, args, repl in sorted(callbacks):
        func_args, return_vars = func_details(name, args)
        cb_list.append(
            f"def {name}({func_args}):\n" + "\n    ".join(repl.splitlines())
        )
    callbacks = "\n# CALLBACKS ---- \n" + "\n".join(cb_list) + "\n\n"

    empty_callbacks = "\n# EMPTY CALLBACKS ----\n" + "\n".join(
        f"{cb} = None"
        for cb in sorted(empty_callbacks)
    ) + "\n\n"

    self.callbacks_code = empty_callbacks + callbacks
    imports.add("\nfrom egsnrc.callbacks import *")


def get_expansion_type(m_to):
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


def eval_using(match):
    # $EVALUATE#USING#(#);
    # Simply use Python f-string replacement to handle the known fixed #s
    P1, P2, P3 = match.groups()
    if P2.lower().strip() == "sin":
        eval_expr = f"{P1}=sin({P2})"
    elif P2.lower() in 'sinc blc rthr rthri'.split():
        eval_expr = f"{P1}={P2}1[L{P3}]*{P3}+{P2}0[L{P3}]"
    else:
        eval_expr = f"{P1}={P2}1[L{P3},MEDIUM]*{P3}+{P2}0[L{P3},MEDIUM]"
    return eval_expr + f"  # EVALUATE{P1}USING{P2}({P3})"  # match.group(0) caused inf recursion


def expand_function_call(match, macro_from, re_replace, re_with):
    # XXX for now, do all as inline
    # indent = match.group(1)
    func_match = re.search(r"(?:\$)?([\w_]+);?(\(.*?#.*?\))?;?", macro_from)
    func_name = func_match.group(1).lower()
    empty_callbacks.add(func_name)
    args_list = ", ".join(match.groups()) if match else ""
    args_str = f"({args_list})"
    indent = "   "  # XXX need proper indent later

    repl = match.expand(re_with)
    fn_check = f"{indent}if {func_name}:\n{indent}    {func_name}{args_str}"
    lines = []
    if repl.strip().replace(";",""):  # non-empty replace
        lines = [f"\n{indent}else:"] + [
            f"{indent}    {line}"
            for line in dedent(repl).splitlines()
        ]

    # Visually bracket the replacement with comments
    # Still use `if` check against none, so user can replace if desired
    # the `else:` clause is the "inlined" code
    # remove the '$' and ';' if there so doesn't match in recursive replaces
    match_name = match.group(0).strip().replace("$", "$ ") # so later macros don't replace in the comment
    pre_comment = f"{indent}# --- Inline replace: {match_name} -----\n"
    post_comment = f"\n{indent}# End inline replace: {match_name} ----\n"
    return pre_comment + fn_check + "\n".join(lines) + post_comment


    post_comment = f"\n{indent}# " + "-" * len(pre_comment.strip()) + "\n"

    return pre_comment + match.expand(re_with) + post_comment
    # XXX do replace for non-inline
    # func_args, return_vars = func_details(macro_from)


def handle_macro_expansion(macro_from, match, re_replace, re_with) -> str:
    if macro_from.startswith(("$COMIN", ";COMIN", "$DEFINE", "$DECLARE")):
        commons[macro_from] = re_with  # keep in case needed later
        return "# " + macro_from

    # Logging/warning/error functions
    if macro_from in logging_replace:
        return match.expand(logging_replace(macro_from))

    if macro_from.startswith('$EVALUATE#USING'):
        return eval_using(match)

    # Function calls - any parentheses with an argument are one for sure
    if (
        re.search(r"\(.*?#.*?\)", macro_from)
        or macro_from.endswith(";")
        or macro_from.lower().startswith("$call_")
    ): # XXX not all that end in ; are calls
        expansion = match.expand(re_with)
        expansion = parse_and_apply_macros(expansion)
        if "call " in expansion or " = " in expansion or macro_from.lower().startswith("$call_"):
            return expand_function_call(match, macro_from, re_replace, re_with)
    return match.expand(re_with)


# Note WITH can be followed by a comment before the opening {
replace_with_pattern_str = r"REPLACE\s*\{(.*?)\}\s*?WITH\s*?(\".*?\"\s*)?\{"
replace_with_pattern = re.compile(replace_with_pattern_str, re.MULTILINE)


def parse_and_apply_macros(code: str) -> str:
    """Parse code, collecting and applying macros in order

    Parameters
    ----------
    code: str
        The macros AND the source code they operate on

    Sets
    ----
    macros: dict
        List of re-compiled previously parsed macros: replacement pairs

    parameters: dict
        List of constants that are replaced with capitalized name,
        and will be put into a Python parameters file
    """
    global macros, parameters

    def sub_func(match):
        nonlocal re_with, macro_from, re_replace
        global macros, parameters
        # Act on macros and replaces inside out replacement value
        expansion_type = get_expansion_type(re_with)
        if expansion_type in NUMBER_TYPES:
            bare_name = match.group(0).replace("$", "").replace(";", "")  # 'from' name without $ or ;
            to_ = re_with.replace(".true.", "True").replace(".false.", "False")
            parameters[bare_name] = (expansion_type, to_)
            return bare_name  # no need to recurse - know it is a simple #
        else:
            expand1 = handle_macro_expansion(macro_from, match, re_replace, re_with)
        expanded = parse_and_apply_macros(expand1)
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

    # in_filename = MORTRAN_SOURCE_PATH / "electr.mortran"
    # with open(in_filename, 'r') as f:
    #     electr_code = f.read()

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
    code = parse_and_apply_macros(macros_code)
    print("Code\n----\n", code)
    print("# macros:", macros)
    # pprint(macros)
    # pprint(code)