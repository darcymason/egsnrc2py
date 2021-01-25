import re
from typing import Tuple, List
from pprint import pprint
import textwrap
from egsnrc2py import edited_callbacks

from egsnrc2py.util import nested_brace_value, fix_identifiers, block_subs, replace_subs
from egsnrc2py.config import (
    default_float, AUTO_TRANSPILE_PATH, MORTRAN_SOURCE_PATH,
    INTEGER, REAL, LOGICAL
)

import logging
from pathlib import Path

logger = logging.getLogger('egsnrc2py')


commons = {}
callbacks = {}
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
    '$egs_debug(#,#);': "logger.debug(\\g<3>)",
    '$egs_info(#,#);': "logger.info(\\g<3>)",
    '$egs_warning(#,#);': f"logger.warning('{warning_line}\\n\\g<3>')",
    '$egs_fatal(#,#,#);': textwrap.dedent(
        f"""logging.critical('{error_line}')\n
        logging.critical(\\g<3>)\n
        logging.critical('{quit_line}')\n
        sys.exit(\\g<4>)
        """
    ),
    '$egs_fatal(#,#);': textwrap.dedent(
        f"""logging.critical('{error_line}')\n
        logging.critical('\\g<3>')\n
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
    'compute_drange': ("eke1,eke2,lelke1,elke1,elke2".split(","), (5,)),
    'randomset': ([], (0,)),  # the one argument is output only

}

dont_inline = ["randomset"]


# Macros to leave alone or deal with in special functions
# Set value to None if meant to ignore - match will be put in a Python comment
complex_macros = {
    '$DUMP#,#;': None,
    '$IMPLICIT_NONE': None,
    '$SET INTERVAL#,#;': None,  # is SET INTERVAL#,SINC in .macros, but nothing in Fortran
}


def write_parameters_file(filename) -> None:
    with open(filename, "w") as f:
        f.write("import numpy as np\n\n")
        for name, (_type, value) in parameters.items():
            f.write(f"{name}: {_type} = {value}\n")

def write_callbacks_file(filename) -> None:
    with open(filename, "w") as f:
        f.write(_generate_callbacks_code(filename))

def _generate_callbacks_code():
    cb_list = []
    for name in sorted(callbacks):
        if f"{name}_def" in dir(edited_callbacks):
            cb_list.append(getattr(edited_callbacks, f"{name}_def"))
        else:
            args, repl, return_vars = callbacks[name]
            # func_args, return_vars = func_details(name, args)
            func_body = '\n    '.join(textwrap.dedent(repl).splitlines())
            return_str = f"\n   return {', '.join(return_vars)}" if return_vars else ""
            cb_list.append(
                f"def {name}({', '.join(args)}):\n{func_body}{return_str}"
            )

    callbacks_str = ""
    if cb_list:
        callbacks_str = "\n# CALLBACKS ---- \n" + "\n".join(cb_list) + "\n\n"

    empty_callbacks_str = "\n# EMPTY CALLBACKS ----\n" + "\n".join(
        f"{cb} = None"
        for cb in sorted(empty_callbacks)
    ) + "\n\n"

    return empty_callbacks_str + callbacks_str


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
    def sub_P_plus_1(match):
        r"""Because indent is first in match {P1} -> \g<2>, {P2} -> \g<3> etc"""
        P_num = int(match.group(1))
        return f"\\g<{P_num + 1}>"

    re_from = re.escape(m_from).replace(r"\#", "(.*?)")
    indent_pattern = r"(?P<indent> *?)"
    re_from = indent_pattern + re_from

    # Set up replace of {P1} etc if "from" has arguments
    # Some "to"'s (e.g. $CALLTRACE) have REPLACEs inside them,
    # and should not update those until next time macros run.
    # These are bracketed like "{WAIT {P1}}"
    re_to = m_to
    # ?<! is a negative look-behind.  Don't match if that text precedes
    re_to = re.sub(r"(?<!WAIT )\{P(\d*)\}", sub_P_plus_1, re_to)  # {Px} -> \x group replace
    re_to = re.sub(r"\{WAIT \{ARB\}\}", "#", re_to)
    re_to = re.sub(r"\{WAIT \{P(\d*)\}\}", r"{P\1}", re_to)

    # Note re_to does not have the indent in it - added when replaced
    return re_from, re_to


def func_details(name, args:list) -> Tuple[list, list]:
    # Return the args used in the function definition (not the call)
    # and the name of vars to be returned
    bare_name = name.replace("$", "").replace(";", "")
    if bare_name in func_signatures:
        func_args, func_outputs = func_signatures[bare_name]
        return_vars = [
            args[out].strip() if isinstance(out, int) else out
            for out in func_outputs
        ]
    else:
        func_args = args
        if func_args:
            return_vars = ["<XXX>"]  # will be syntax error, forcing hand-edit
        else:
            return_vars = []
    return func_args, return_vars


def eval_using(match):
    # $EVALUATE#USING#(#);
    # Simply use Python f-string replacement to handle the known fixed #s
    _, P1, P2, P3 = match.groups()  # first match is indent
    if P2.lower().strip() == "sin":
        eval_expr = f"{P1}=sin({P2})"
    elif P2.lower() in 'sinc blc rthr rthri'.split():
        eval_expr = f"{P1}={P2}1[L{P3}]*{P3}+{P2}0[L{P3}]"
    else:
        eval_expr = f"{P1}={P2}1[L{P3},MEDIUM]*{P3}+{P2}0[L{P3},MEDIUM]"
    return add_indent(
        eval_expr + f"  # EVALUATE{P1}USING{P2}({P3})",  # match.group(0) caused inf recursion
        match
    )

def add_indent(text, match):
    # May also later add, e.g. reformatting if/else blocks not indented yet
    return textwrap.indent(text, match.group("indent"))

def bracket_function_call(match, macro_from, expansion):
    match_name = match.group(0).strip().replace("$", "$ ") # so later macros don't replace in the comment
    pre_comment = f"# --- Inline replace: {match_name} -----\n"
    post_comment = f"\n# End inline replace: {match_name} ----"
    func_match = re.search(r"(?:\$)?([\w_]+);?(\(.*?#.*?\))?;?", macro_from)
    func_name = func_match.group(1).lower()

    args = [x.strip() for x in match.groups()[1:]]

    func_args, return_vars = func_details(func_name, args)
    args_str = f"({', '.join(func_args)})"
    logger.debug(f"Handling function '{func_name}'")
    return_str = f"{', '.join(return_vars)} = " if return_vars else ""
    if func_name in dont_inline:
        callbacks[func_name] = (func_args, expansion, return_vars)
    else:
        empty_callbacks.add(func_name)

    func_call = f"{return_str}{func_name}{args_str}"
    if func_name in dont_inline:
        return func_call
    else:
        fn_check = f"if {func_name}:\n    {func_call}"
        lines = []
        if expansion.strip().replace(";",""):  # non-empty replace
            lines = [f"\nelse:"] + [
                f"    {line}"
                for line in expansion.splitlines()
            ]
        return pre_comment + fn_check + "\n".join(lines) + post_comment


def handle_macro_expansion(
    macro_from, match, re_replace, re_with
) -> str:
    if macro_from.startswith(("$COMIN", ";COMIN", "$DEFINE", "$DECLARE")):
        commons[macro_from] = re_with  # keep in case needed later
        # Mangle the name a little so don't replace it in the comment
        # Mangle the name a little so don't replace it in the comment
        return "# " + macro_from.replace("$", "$ ").capitalize()

    # Logging/warning/error functions
    if macro_from in logging_replace:
        return add_indent(
            match.expand(logging_replace[macro_from]), match
        )

    if macro_from.startswith('$EVALUATE#USING'):
        return eval_using(match)

    if macro_from in complex_macros:
        handler = complex_macros[macro_from]
        if handler:
            raise NotImplementedError("Need to implement complex macro handlers")
        return add_indent(
            # add space after $ to not replace again
            f"# Unhandled macro '{textwrap.dedent(match.group(0).replace('$', '$ '))}'",
            match
        )

    if "[IF]" in re_with or "{EMIT }" in re_with:
        # is a "complex macro" - we leave alone for now
        msg = (
            f"# Complex macro {macro_from} - not replacing "
            "within its replacement"
        )
        return add_indent(
            msg + match.expand(re_with), match
        )

    # Something else, maybe function calls
    expansion = textwrap.dedent(match.expand(re_with))

    # Separate out lines in IF or loop blocks
    expansion = replace_subs(expansion, block_subs)

    # recurse into it and apply any other replacements
    expansion = _parse_and_apply_macros(expansion)
    if (
        re.search(r"\(.*?#.*?\)", macro_from)
        or macro_from.endswith(";")
        or macro_from.lower().startswith("$call_")
    ): # XXX not all that end in ; are calls

        if "call " in expansion or " = " in expansion or macro_from.lower().startswith("$call_"):
            return add_indent(
                bracket_function_call(
                    match, macro_from, expansion),
                match
            )
        else:
            return expansion
    return add_indent(expansion, match)


# Note WITH can be followed by a comment before the opening
# Use engaive look-behind (?<!") to not take a replace immediately after
#  a comment marker ("). Note if a closed comment before the replace,
#  we will miss that one.
replace_with_pattern_str = r'(?<!")REPLACE\s*\{(.*?)\}\s*?WITH\s*?(\".*?\"\s*)?\{'
replace_with_pattern = re.compile(replace_with_pattern_str, re.MULTILINE)


def _parse_and_apply_macros(code: str) -> str:
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
            parameters[bare_name.strip()] = (expansion_type, to_)
            return bare_name  # no need to recurse - know it is a simple #
        else:
            return handle_macro_expansion(
                macro_from, match, re_replace, re_with
            )

    i_first = 0
    no_matches = set()
    while True:
        # Find either a REPLACE...WITH macro
        # or the first match to existing macros, whichever is first

        live_macros = (m for m in macros if m not in no_matches)
        rev_macros = sorted(live_macros, key=lambda x: len(x), reverse=True)

        i_last = i_first  # i_last will be start of last thing we replaced
        i_first = 1e9  # stays there if no match
        repl_match = replace_with_pattern.search(code, pos=i_last)
        if repl_match:
            i_first = repl_match.start()

        i_match = None
        if code.strip():  # Don't go through everything if nested replace empty
            for i, m_from in enumerate(rev_macros):
                compiled_from = macros[m_from][0]
                match = compiled_from.search(code, pos=i_last)
                if not match:  # never appears in code, don't search it again
                    no_matches.add(m_from)
                    continue
                if match.start() < i_first:
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
            match = repl_match
            m_replace = match.group(1)
            logger.debug(f"Found REPLACE macro '{m_replace}' at pos {i_first} ")
            m_with = nested_brace_value(code, match.end())
            i_start = match.start()
            i_end = match.end() + len(m_with) + 1  # one extra for }
            code = code[:i_start] + code[i_end:]
            if m_replace in macros:
                orig_with = macros[m_replace][1]
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


def init_macros():
    macros.clear()
    parameters.clear()
    callbacks.clear()
    empty_callbacks.clear()
    imports.clear()

    # $SET INTERVAL defined without space in egsnrc.macros, here add to
    #   be able to comment those lines out
    m_from = r'$SET INTERVAL#,#;'
    re_from, _ = re_from_to(m_from, "")
    macros[m_from] = (re.compile(re_from), r"# $ SET INTERVAL \g<2>,\g<3>")


def apply_macros(macros_code, egs_code):
    # First process macros file; macros will stay in global variable
    init_macros()

    logger.info("Fixing identifiers (no dash or leading numbers)")
    macros_code = fix_identifiers(macros_code)
    egs_code = fix_identifiers(egs_code)

    logger.info("Parsing macros code")
    macros_code = _parse_and_apply_macros(macros_code)

    logger.info("Parsing egs code")
    egs_code = _parse_and_apply_macros(egs_code)

    return macros_code, egs_code

if __name__ == "__main__":

    # in_filename = MORTRAN_SOURCE_PATH / "electr.mortran"
    # with open(in_filename, 'r') as f:
    #     electr_code = f.read()

    with open(MORTRAN_SOURCE_PATH / "egsnrc.macros", 'r') as f:
        macros_code = f.read()

    macros_code = textwrap.dedent("""REPLACE {PARAMETER #=#;} WITH
        { REPLACE {{P1}} WITH {{P2}}}
        PARAMETER $MXXXX=400;     "GAMMA SMALL ENERGY INTERVALS"
        x = $MXXXX;

        PARAMETER $MXXXX=1;
        y = $MXXXX;
        z = $MXSGE;
    """
    )

    code = _parse_and_apply_macros(macros_code)
    print("Code\n----\n", code)
    print("# macros:", macros)
    # pprint(macros)
    # pprint(code)