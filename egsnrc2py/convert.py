import re

import sys

from typing import List
from textwrap import dedent

from .macros import constant_macros, called_macros
from .config import MORTRAN_SOURCE_PATH, AUTO_TRANSPILE_PATH, TEMPLATES_PATH


# Type conversions
REAL = "float"
ENERGY_PRECISION = "float"
INTEGER = "int"
LOGICAL = "bool"


if sys.version_info < (3, 8):
    raise ImportError("Need Python 3.8 or later")

# XXX needs to be OrderedDict!!
main_subs = {
    # Comments / semicolons
    r'"(.*?)"': r'# \1',  # comments in quotes
    r'"(.*)': r'# \1',  # comment without end quote
    r";(\s*)$": r"\1",      # semi-colon at end of line
    r";(\s*)(?P<comment>#(.*?))?$": r" \g<comment>", # still a semicolon before #

    # IF/ELSE
    r"^(\s*)IF\((.*)\)\s*?\[(.*?)[;]?\](.*?)$": r"\1if \2:\n\1    \3\4", # basic IF
    r"^(\s*)(?:]\s*)?ELSE(.*?)?\[(.*)\](.*?)$": r"\1else:\n\1    \3\4", # basic ELSE [ ]
    r"^(\s*)(?:]\s*)?ELSE(\s*?)?\[?.*?$": r"\1else:",  # bare ELSE line or ELSE [
    r"^(\s*)IF(\s*)?\((.*)\)(.*)$": r"\1if \3:\n\1    \4", # IF on one line

    # LOOPs
    r"^(\s*):(\w*):LOOP": r"\1while True:  # :\2: LOOP",
    r"^(\s*)\]\s*UNTIL\s*\((.*?)\)(\s*?# .*$)?": r"\1if \2:\n\1    break \3",


    # Math operators
    r"if(.*?)~=": r"if\1!=", # not equals
    r"if(.*?) = ": r"if\1 == ", # = to ==
    r"if(.*?) = ": r"if\1 == ", # = to == again if there multiple times
    r"if(.*?) = ": r"if\1 == ", # = to == again

    # Booleans
    r" \| ": r" or ",
    r" \& ": r" and ",

    # Leftover brackets
    r"^\s*\[\s*$": r"",  # line with only [
    r"^\s*\]\s*$": r"",  # line with only ]


    # r"\$electron_region_change|\$photon_region_change": r"ir(np) = irnew; irl = irnew; medium = med(irl)",
    # r"\$declare_max_medium": r"",
    r"\$default_nmed": "1",
    # r"\$INIT-PEGS4-VARIABLES": "",
    # r"\$DECLARE-PEGS4-COMMON-BLOCKS": "",
    r"SUBROUTINE\s*(.*)$": r"def \1:",
    r"\.true\.": "True",
    r"\.false\.": "False",
    r"[iI]f(.*?)(?:=)?=\s*True": r"if\1 is True",
    r"[iI]f(.*?)(?:=)?=\s*False": r"if\1 is False",
    r"\$IMPLICIT_NONE": r"",
    r"\$DEFINE_LOCAL_VARIABLES_ELECTR": r"# $DEFINE_LOCAL_VARIABLES_ELECTR XXX do we need to type these?",


}

call_subs= {
    # paired with added funcs
    # r"\$start_new_particle": r"start_new_particle()",
    # r"\$CALL_USER_ELECTRON": r"call_user_electron()",
    # r"\$SELECT_ELECTRON_MFP": r"select_electron_mfp()",
    r"   \$RANDOMSET (\w*)": r"\1 = randomset()",
}


def add_new_funcs(code: str) -> str:
    fakes = [
        "def start_new_particle():\n    medium = med[irl]\n\n",
        "def call_user_electron():\n    pass\n\n",
        "def select_electron_mfp():\n    RNNE1 = randomset()\n    if RNNE1 == 0.0):\n        RNNE1 = 1.E-30\n    DEMFP = max([-log(RNNE1), EPSEMFP])",
    ]
    return "\n".join(fakes) + code

commenting_lines = [
    "/******* trying to save evaluation of range.",
    "*/",
    "data ierust/0/ # To count negative ustep's",
    "save ierust",

]

def replace_subs(code: str, subs: dict) -> str:
    for pattern, sub in subs.items():
        code = re.sub(pattern, sub, code, flags=re.MULTILINE)
    return code


def replace_var_decl(code: str) -> str:
    # XXX maybe go to numpy types here, e.g.
    #    floatvar: np.float32 = np.dtype('float32').type(4.5)
    mapping = {
        "$INTEGER": INTEGER,
        "$REAL": REAL,
        "$LOGICAL": LOGICAL,
        "LOGICAL": LOGICAL,
    }

    out_lines = []
    for line in code.splitlines():
        matched = False
        for typ in ["$INTEGER", "$REAL", "$LOGICAL", "LOGICAL"]:
            if line.startswith(typ):
                vars = line.replace(typ, "").split(",")
                for var in vars:
                    out_lines.append(f"{var.strip()}: {mapping[typ]}")
                matched = True
                break # out of inner loop
        if not matched:
            out_lines.append(line)

    return "\n".join(out_lines)

def comment_out_lines(code: str, lines_to_comment: list) -> str:
    all_lines = code.splitlines()
    for i, line in enumerate(all_lines):
        if line in lines_to_comment:
            all_lines[i] = "# " + line
    return "\n".join(all_lines)


def fix_identifiers(code) -> str:
    """Take invalid (for Python) var names and make them valid"""
    # Fix leading number, often used for ranges
    code = re.sub(r"^[^#].*\$(\d)", r"\$from\1", code)

    # Fix dashes to underscore
    # First, some comment have "---", keep those by enclosing in spaces
    code = re.sub(r'"(.*)\$(\w*)---', r'"\1$\2 --- ', code)
    for i in range(8, 1, -1):  # up to 7 dashes
        pattern = r"\$" + "-".join([r"(\w*)"]*i)
        subst = r"$" + "_".join([rf"\{j}" for j in range(1, i+1)])
        code = re.sub(pattern, subst, code)
    return code


def transpile_macros(code: str) -> str:
    """Transpile statements in macros file"""
    macro_subs = {
        # PARAMETER with comments
        r'PARAMETER\s*\$(\w*)\s*=\s*(\d*);\s*"(.*?)"': r"\1: int = \2  # \3",
        r'PARAMETER\s*\$(\w*)\s*=\s*(\d*\.\d*);\s*"(.*?)"': r"\1: float = \2  # \3",

        # PARAMETER without comments
        r'PARAMETER\s*\$(\w*)\s*=\s*(\d*);': r"\1: int = \2",
        r'PARAMETER\s*\$(\w*)\s*=\s*(\d*\.\d*);': r"\1: float = \2",

        # REPLACE
        r'REPLACE\s*\{\$(\w*)\}\s*WITH\s*\{(\d*)\}': r"\1: int = \2",  # simple int replacement
        r'REPLACE\s*\{\$(\w*)\}\s*WITH\s*\{(\d\.?\d*)\}': r"\1: float = \2",  # simple float replace

        # Comments, semicolon
        r'"(.*?)"': r'# \1',  # comments in quotes
        r'"(.*)': r'# \1',  # comment without end quote
        r";(\s*)$": r"\1",      # semi-colon at end of line
        r";\s*(# .*$)?": r"\1", # '; # comment' -> just comment

        # Compiler directives
        r"^%(.*?)$": r"# %\1",  # Any line starting with %

    }
    for pattern, sub in macro_subs.items():
        code = re.sub(pattern, sub, code, flags=re.MULTILINE)

    return code


def replace_auscall(code: str) -> str:
    """Return list of strings, indented properly

    XXX ASSUMES
    - AUSGAB is first on line, other than whitespace
    - no comments are anything else on line

    """
    # pattern = r"^(?P<indent>\s*)\$AUSCALL\((?P<arg>.*?)\)\s*?;?.*?$"
    pattern = r"^( *?)\$AUSCALL\((.*)\)"
    subst = (
        r"\1IARG = \2\n"
        r"\1if IAUSFL[IARG + 1] != 0:\n"
        r"\1    AUSGAB(IARG)"
    )
    # subst = "XXXXYYYY"
    code = re.sub(pattern, subst, code, flags=re.MULTILINE)
    fake_ausgab = """\n\ndef AUSGAB(IARG):\n    pass\n\n\n"""

    return fake_ausgab + code


def particle_vars_and_types():
    """All the variables relating to a specific particle

    From:
    COMMON/STACK/
       $LGN(E,X,Y,Z,U,V,W,DNEAR,WT,IQ,IR,LATCH($MXSTACK)),
       LATCHI,NP,NPold;
   $ENERGY PRECISION
       E;     "total particle energy"
   $REAL
       X,Y,Z, "particle co-ordinates"
       U,V,W, "particle direction cosines"
       DNEAR, "perpendicular distance to nearest boundary"
       WT;    "particle weight"
   $INTEGER
       IQ,    "charge, -1 for electrons, 0 for photons, 1 for positrons"
       IR,    "current region"
       LATCH, "extra phase space variable"

       # Note these are not array-based, just single value; XXX ignore for now
       LATCHI,"needed because shower does not pass latch-BLOCK DATA sets 0"
       NP,    "stack pointer"
       NPold; "stack pointer before an interaction"
    """

    # Just a fixed list now, but may later generate from parsing code
    vars = "e x y z u v w dnear wt iq ir latch".split()
    var_types = [ENERGY_PRECISION] + [REAL]*8 + [INTEGER]*3
    return vars, var_types


def replace_particle_vars(code: str) -> str:
    """Replace arrays with <var>(np) in Mortran to p.<var> in Python"""
    vars, _ = particle_vars_and_types()
    particle_var = "p"
    for var in vars:
        # XXX note below assumes np not changed in code
        pattern = rf"([^\w]){var}\(np\)"  # e.g. "wt(np)" or "WT(np)"
        subst = rf"\1{particle_var}.{var}"
        code = re.sub(pattern, subst, code, flags=re.IGNORECASE)

    # Also indicate when particle has been cut:
    pattern = r"np\s*?=\s*?np\s*?-\s*?1\s*?;"  # np = np - 1;
    subst = r"p.exists = False"
    code = re.sub(pattern, subst, code, flags=re.IGNORECASE)

    return code


def build_particle_class(filename) -> None:
    """Build the Particle class and save to Python file"""
    vars, var_types = particle_vars_and_types()
    imports = ""
    variables = "\n    ".join(
        f"{var}: {var_type}"
        for var, var_type in zip(vars, var_types)
    )

    with open(TEMPLATES_PATH / "particle_tmpl.py", 'r') as f:
        str_out = f.read().format(imports=imports, variables=variables)

    with open(filename, 'w') as f:
        f.write(str_out)


def replace_macro_callables(code: str) -> str:
    """Macros that are callable replaced with (may be optional) call"""

    subst = dedent(r"""
        \1if \2:
        \1    \2(\3)"""
    )

    for macro in called_macros:
        # Note, next line assumes `fix_identifiers` has already been run
        macro_str = macro.replace("-", "_").replace("$", "")
        pattern = rf'^( *)\$({macro_str})\s*?(?:\((.*)\))?;' # \s*(\".*?)$ comment
        # match = re.search(pattern, code, flags=re.MULTILINE)
        # if match:
        #     print(f"Matched {pattern}")
        code = re.sub(pattern, subst, code, flags=re.MULTILINE)
    return code


def replace_constants(code, ):
    pass


if __name__ == "__main__":
    out_filename = AUTO_TRANSPILE_PATH / "electr.py"

    # in_filename = MORTRAN_SOURCE_PATH / "egsnrc.macros"
    # out_filename = AUTO_TRANSPILE_PATH / "common.py"

    with open(MORTRAN_SOURCE_PATH / "electr.mortran", 'r') as f:
        code = f.read()

    code = fix_identifiers(code)
    code = replace_macro_callables(code)
    code = replace_subs(code, main_subs)
    # code = transpile_macros(code)
    code = replace_auscall(code)
    code = add_new_funcs(code)
    code = replace_subs(code, call_subs)
    code = replace_var_decl(code)
    code = comment_out_lines(code, commenting_lines)
    code = replace_particle_vars(code)
    build_particle_class(AUTO_TRANSPILE_PATH / "particle.py")

    # code = "$AUSCALL($SPHOTONA);"
    # code = replace_macro_callables(code)
    # print(code)
    with open(out_filename, "w") as f:
        f.write(code)

