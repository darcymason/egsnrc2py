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


class MacrosAndCode:
    """Hold macro processing code and ensure called in proper sequence"""
    max_lines_for_inlining = 15  # max lines in a macro for putting inline

    def __init__(self, macros_code, source_code):
        """Parse macros code and classify different types for later use"""
        # First, take dashes out of identifiers
        self.macros_code = self.orig_macros_code = fix_identifiers(macros_code)
        self.source_code = fix_identifiers(source_code)

        # Change all PARAMETERS x = y macros back to REPLACE..WITH
        self._replace_PARAMETERs()

        # Find all REPLACE ... WITH -> store in self.all_from_to
        self._map_replace_from_to()

        self._replace_var_decl()
        # Classify macros to called, constant, defined_block, other
        #   This uses the macros code and the source code - latter to see
        #   how they are used
        self._macro_types()

        # For simple constants and parameters
        #   find, replace with own name without the "$" sign
        self._process_parameters()
        self._replace_macro_callables()

    def _replace_PARAMETERs(self) -> str:
        """Replace PARAMETER macros with REPLACE..WITH

        This just makes it easier to treat them along with other replaces.
        """
        pattern = r"^( *)PARAMETER\s*(.*?)\s*?=\s*?(.*?);(.*)$"
        repl_with = r"\1REPLACE {\2} WITH {\3}\4"
        self.macros_code = re.sub(
            pattern, repl_with, self.macros_code, flags=re.MULTILINE
        )

    @property
    def all_from_to(self):
        return self._all_from_to

    def _map_replace_from_to(self):
        """Goes through the .macros `code` and determines all REPLACE .. WITH"""
        all_from_to = []
        all_di = {}
        pattern = r"^ *REPLACE\s*\{(.*?)\}\s*?WITH\s*?\{"
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
                logger.warning(
                    f"Repeat definition of macro '{replace_from}' from "
                    f"'{all_di[replace_from]}' to {replace_to}"
                )
            all_from_to.append((replace_from, replace_to))
            all_di[replace_from] = replace_to
            # print(replace_from, " -> ", replace_to)
            i = match.end() + len(replace_to) + 1  # one extra for }

        self._all_from_to = all_from_to

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
        self.other = []

        for m_from, m_to in self.all_from_to:
            # See if called - if alone on a line (except comments):
            if m_from.startswith(("$COMIN", ";COMIN", "$DEFINE", "$DECLARE")):
                self.defined_block.append((m_from, m_to))
                continue
            macro_str = escape(m_from)
            alone_pattern = rf'^ *{macro_str}\s*?(["#].*?$)?;?'
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

        out_lines = ["import numpy as np\n"]
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
        for m_from, m_to in self.all_from_to:
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

    def write_new_macros_file(self, filename) -> None:
        with open(filename, "w") as f:
            f.write(self.macros_code)

    def write_params_file(self, filename) -> None:
        with open(filename, "w") as f:
            f.write("import numpy as np\n\n")
            for name, _type, value in self.parameters:
                f.write(f"{name}: {_type} = {value}\n")

    def macro_replaced_source(self) -> str:
        return self.source_code

    def _replace_macro_callables(self):
        """Macros that are callable replaced with (may be optional) call"""


        def replace_fn(match):
            """Make lower-case names"""
            indent = match.group(1)
            func_name = match.group(2).lower()
            args = match.group(3) or ""
            return f"{indent}if {func_name}:\n{indent}    {func_name}({args})"

        for macro, repl in self.called:
            # Note, next line assumes `fix_identifiers` has already been run
            macro_str = macro.replace("-", "_").replace("$", "").replace(";", "")
            pattern = rf'^( *)\$({macro_str})\s*?(?:\((.*)\))?;?' # \s*(\".*?)$ comment
            # match = re.search(pattern, code, flags=re.MULTILINE)
            # if match:
            #     print(f"Matched {pattern}")
            self.source_code = re.sub(pattern, replace_fn, self.source_code, flags=re.MULTILINE)


def test_eval_subst(code):
    pattern = r"\$EVALUATE (\w*) USING (\w*)\((\w*)\);?"
    subst = """
    [IF] '\g<2>'=SNAME1
    [\g<1>=\g<2>1(L\g<3>)*\g<3>+\g<2>0(L\g<3>);] [ELSE]
    [\g<1>=\g<2>1(L\g<3>,MEDIUM)*\g<3>+\g<2>0(L\g<3>,MEDIUM);]}
    """
    # subst = r"\1"
    # m = re.search(pattern, code)
    # print(m.groups())

    code = re.sub(pattern, subst, code, re.MULTILINE)
    return code


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




if __name__ == "__main__":

    in_filename = MORTRAN_SOURCE_PATH / "electr.mortran"
    with open(in_filename, 'r') as f:
        code = f.read()

    with open(MORTRAN_SOURCE_PATH / "egsnrc.macros", 'r') as f:
        macros_code = f.read()

    macros_code = dedent("""REPLACE {$MAXL_MS}    WITH {63}
        REPLACE {$SELECT-ELECTRON-MFP;} WITH {
        $RANDOMSET RNNE1; IF(RNNE1.EQ.0.0) [RNNE1=1.E-30;]
        DEMFP=MAX(-LOG(RNNE1),$EPSEMFP);}

    """
    )
    code = dedent("""# Not vacuum. Must sample
        $SELECT_ELECTRON_MFP;
        """
    )

    # macros_code = "PARAMETER $MXSGE=1;\n    PARAMETER $MXSEKE=1;"

    # with open(MORTRAN_SOURCE_PATH / "egsnrc.mortran", 'r') as f:
    #     egsnrc_code = f.read()

    macros = MacrosAndCode(macros_code, code)
    print("Output code")
    print(macros.macro_replaced_source())