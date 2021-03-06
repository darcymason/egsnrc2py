import re

import sys
from pathlib import Path

from typing import List
from textwrap import dedent

from egsnrc2py import macros

from egsnrc2py.config import (
    MORTRAN_SOURCE_PATH, AUTO_TRANSPILE_PATH, TEMPLATES_PATH,
    REAL, ENERGY_PRECISION, INTEGER, LOGICAL
)
from egsnrc2py.util import fix_identifiers, replace_subs, main_subs


if sys.version_info < (3, 9):
    raise ImportError("Need Python 3.9 or later")  # for dict union symbol


usage = """
    python convert.py <input_filename>
"""

call_subs= {
    # paired with added funcs
    # r"\$start_new_particle": r"start_new_particle()",
    # r"\$CALL_USER_ELECTRON": r"call_user_electron()",
    # r"\$SELECT_ELECTRON_MFP": r"select_electron_mfp()",
    r"   \$RANDOMSET (\w*)": r"\1 = randomset()",
}

# Replace variable names that are Python keywords
keyword_subs = {
    r'\blambda\b': r"lambda_",
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




def comment_out_lines(code: str, lines_to_comment: list) -> str:
    all_lines = code.splitlines()
    for i, line in enumerate(all_lines):
        if line in lines_to_comment:
            all_lines[i] = "# " + line
    return "\n".join(all_lines)


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
    """Replace arrays with <var>(np) in Mortran to <var>[np] in Python"""
    vars, _ = particle_vars_and_types()
    # particle_var = "p"
    for var in vars:
        # XXX note below assumes np not changed in code
        pattern = rf"([^\w]){var}\(np\)"  # e.g. "wt(np)" or "WT(np)"
        subst = rf"\1{var}[np]"
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


if __name__ == "__main__":
    from egsnrc2py.config import EGS_HOME_PATH


    if len(sys.argv) != 2:
        print(usage)
        sys.exit(-1)

    input_path = Path(sys.argv[1])
    if input_path.suffix != "mortran":
        input_path = Path(input_path.name + ".mortran")

    with open(MORTRAN_SOURCE_PATH / input_path, 'r') as f:
        egs_code = f.read()


    with open(MORTRAN_SOURCE_PATH / "egsnrc.macros", 'r') as f:
        macros_code = f.read()
    with open(MORTRAN_SOURCE_PATH / "ranlux.macros", 'r') as f:
        random_macros = f.read()

    out_filename = AUTO_TRANSPILE_PATH / (input_path.stem + ".py")

    params_py_filename = AUTO_TRANSPILE_PATH / "params.py"
    callbacks_filename = AUTO_TRANSPILE_PATH / "callbacks.py"


    # run_str = "macros_code, egs_code = macros.apply_macros(macros_code, egs_code)"
    # Modify the source code according to what we can do with macros
    #import cProfile, pstats
    #cProfile.run(run_str, "macro_parse_stats")
    #p = pstats.Stats('macro_parse_stats')
    # p.strip_dirs().sort_stats("cumulative").print_stats(15)

    macros_code = random_macros + macros_code
    macros_code, egs_code = macros.apply_macros(macros_code, egs_code)
    print(f"Found {len(macros.macros)} macros")

    egs_code = replace_subs(egs_code, main_subs)
    egs_code = replace_particle_vars(egs_code)
    egs_code = replace_subs(egs_code, call_subs)
    egs_code = replace_subs(egs_code, keyword_subs)

    macros.write_parameters_file(params_py_filename)
    # macros.write_callbacks_file() - for now will place in same output file
    callbacks_code = macros._generate_callbacks_code()

    print("Writing", str(out_filename))
    with open(out_filename, "w") as f:
        f.write(callbacks_code)
        f.write(egs_code)

