"""Various routines for analyzing what are calls, what are constants, etc."""
import re
from typing import Tuple
from pprint import pprint
from textwrap import dedent


block_subs = {
    # IF/ELSE
    r"^(\s*)IF\((.*)\)\s*?\[(.*?)[;]?\](.*?)$": r"\1if \2:\n\1    \3\4", # basic IF
    r"^(\s*)(?:]\s*)?ELSE(.*?)?\[(.*)\](.*?)$": r"\1else:\n\1    \3\4", # basic ELSE [ ]
    r"^(\s*)(?:]\s*)?ELSE(\s*?)?\[?.*?$": r"\1else:",  # bare ELSE line or ELSE [
    r"^(\s*)IF(\s*)?\((.*)\)(.*)$": r"\1if \3:\n\1    \4", # IF on one line

    # LOOPs
    r"^(\s*):(\w*):LOOP": r"\1while True:  # :\2: LOOP",
    r"^(\s*)\]\s*UNTIL\s*\((.*?)\)(\s*?# .*$)?": r"\1if \2:\n\1    break \3",

    # Goto's and labels `go to :label:` -> goto_label = True \n break
    r"(?i)( *)go ?to\s*:(.*?):" : r"\1goto_\2 = True\n\1break # XXX",
    r"^:(.*):": r"if goto_\1:  XXX"  # syntax error so hand-edit flagged

}

main_subs = (
    {
        # Comments / semicolons
        r'"(.*?)"': r'# \1',  # comments in quotes
        r'"(.*)': r'# \1',  # comment without end quote
        r";(\s*)$": r"\1",      # semi-colon at end of line
        r";(\s*)(?P<comment>#(.*?))?$": r" \g<comment>", # still a semicolon before #

        # Compiler directives
        r"^%(.*?)$": r"# %\1",  # Any line starting with %
        r"^!(.*?)$": r"# !\1",  # Any line starting with !

        # Common arrays needing square brackets
        r"(?i)\(irl\)": r"[irl]",  # (irl) region number local variable
        r"(?i)\(medium\)": r"[medium]",  # medium number, often set to med[irl]
        r"\(lelke,medium\)": "[lelke,medium]",  # common in ELECTR

        # Max, min, log sometimes have mixed case - Python needs lowercase
        r"(?i)\bMAX\((.*?)\)": r"max(\1)",
        r"(?i)\bMIN\((.*?)\)": r"min(\1)",
        r"(?i)\bLOG\((.*?)\)": r"log(\1)",

    }
    | block_subs |  # merge operators for dicts Python 3.9+
    {
        # Math operators - (?i) makes whole pattern case-insensitive
        r"(?i)if(.*?)~=": r"if\1!=", # not equals
        r"(?i)if(.*?)\.NE\.": r"if\1 != ", # old-style fortran not equals
        r"(?i)if(.*?)\.EQ\.": r"if\1 == ", # old-style fortran not equals
        r"(?i)if(.*?) = ": r"if\1 == ", # = to ==
        r"(?i)if(.*?) = ": r"if\1 == ", # = to == again if there multiple times
        r"(?i)if(.*?) = ": r"if\1 == ", # = to == again

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
        r"(?i)\$IMPLICIT_NONE": r"",
        r"(?i)\$DEFINE_LOCAL_VARIABLES_ELECTR": r"# $DEFINE_LOCAL_VARIABLES_ELECTR XXX do we need to type these?",

    }
)


def replace_subs(code: str, subs: dict) -> str:
    for pattern, sub in subs.items():
        code = re.sub(pattern, sub, code, flags=re.MULTILINE)
    return code

# def evaluate_using(P1, P2, P3):
#     if P2 == SNAME1:
#         P1=P2}1(L{P3})*{P3}+{P2}0(L{P3})
#     else:
#         {P1}={P2}1(L{P3},MEDIUM)*{P3}+{P2}0(L{P3},MEDIUM)

def test_eval_subst(code):
    pattern = r"\$EVALUATE (\w*) USING (\w*)\((\w*)\);?"
    subst = r"""
    [IF] '\g<2>'=SNAME1
    [\g<1>=\g<2>1(L\g<3>)*\g<3>+\g<2>0(L\g<3>);] [ELSE]
    [\g<1>=\g<2>1(L\g<3>,MEDIUM)*\g<3>+\g<2>0(L\g<3>,MEDIUM);]}
    """
    # subst = r"\1"
    # m = re.search(pattern, code)
    # print(m.groups())

    code = re.sub(pattern, subst, code, re.MULTILINE)
    return code


def fix_identifiers(code) -> str:
    """Take invalid (for Python) var names and make them valid"""

    # Fix dashes to underscore
    # First, some comment have "---", keep those by enclosing in spaces
    code = re.sub(r'"(.*)\$(\w*)---', r'"\1$\2 --- ', code)
    # 'Break' macros in comments with a space - at least the ones with $ start
    # Check for up to 2 of them.
    code = re.sub(
        r'"(.*?)\$([\w-]*)([^"\n\$]*)(?:(\$)([\w-]*))?([^"\n]*)',
        r'"\1$ \2\3\4 \5\6', code)
    for i in range(8, 1, -1):  # up to 7 dashes
        pattern = r"\$" + "-".join([r"(\w*)"]*i)
        subst = r"$" + "_".join([rf"\{j}" for j in range(1, i+1)])
        code = re.sub(pattern, subst, code)

    # Change dashes in goto labels to underscore - leave colons for now
    for i in range(8, 1, -1):  # up to 7 dashes
        pattern = r":" + "-".join([r"(\w*)"]*i) + ":"
        subst = r":" + "_".join([rf"\{j}" for j in range(1, i+1)]) + ":"
        code = re.sub(pattern, subst, code)


    # Fix leading number, often used for ranges
    code = re.sub(r"\$(\d)(\w*?)", r"$from\1\2", code)
    return code



def nested_brace_value(s: str, start: int, open="{", close="}") -> str:
    depth = 1
    orig_start = start
    while depth > 0:
        i_close = s.find("}", start)
        i_open = s.find("{", start)
        if i_open >= 0 and i_open < i_close:
            depth += 1
            start = i_open + 1
        elif i_close < 0:
            raise ValueError(f"Closing brace after position {start} not found")
        else:
            depth -= 1
            start = i_close + 1
    return s[orig_start:i_close]


def map_replace_from_to(code: str) -> str:
    all_from_to = {}
    pattern = r"^ *REPLACE\s*\{(.*)\}\s*WITH\s*\{"
    i = 0
    subcode = code  # need to update search string to exclude REPLACE in val
    while True:
        match = re.search(pattern, subcode, flags=re.MULTILINE)
        if not match:
            break
        replace_from = match.group(1)
        replace_to = nested_brace_value(subcode, match.end())
        all_from_to[replace_from] = replace_to
        # print(replace_from, " -> ", replace_to)
        i += len(match.group(0)) + len(replace_to) + 1  # one extra for }
        subcode = code[i:]

    return all_from_to


def generate_replaces_py(filename:str, code: str) -> None:
    from_to = replace_from_to(code)
    with open(filename, 'w') as f:
        f.write("# autogenerated by _util.generate_macros_py")


if __name__ == "__main__":
    # test_code = "$EVALUATE dedx0 USING ededx(elke);"
    # print("Subst for ", test_code)
    # print(test_eval_subst(test_code))

    in_filename = MORTRAN_SOURCE_PATH / "electr.mortran"
    with open(in_filename, 'r') as f:
        code = f.read()

    with open(MORTRAN_SOURCE_PATH / "egsnrc.macros", 'r') as f:
        macros_code = f.read()

    with open(MORTRAN_SOURCE_PATH / "egsnrc.mortran", 'r') as f:
        full_egs_code = f.read()


    s = dedent(r"""
        REPLACE {$SHORT_INT} WITH {;integer*2} "change this to integer*4 for compilers"

        REPLACE {$TRACE#,#;} WITH {$TRACE{P1};$TRACE{P2};}
        REPLACE {$S1TRACE#,#;} WITH {$S1TRACE{P1};$S1TRACE{P2};}

        REPLACE {$DUMP#,#;} WITH
        {;{SETR A=NEWLABEL}
            V{COPY A}={P1};OUTPUT V{COPY A};(' {P1}=',1PG15.7);
            [IF] {EXIST 2} [$DUMP{P2};] ;}
    "NOTICE: THE LIST OF VARIABLES MUST BE FOLLOWED BY A COMMA"
    "FOR EXAMPLE $DUMP S,T(U,V),W,; OR $DUMP A,;"
    ;  "---------- BUFFER FLUSH SEMICOLON ----------"
    """
    )

    # generate_macros_py(AUTO_TRANSPILE_PATH / "macros.py", full_egs_code)
    map_replaces = map_replace_from_to(macros_code)
    print(map_replaces)
    print("Nulls:")
    for k,v in map_replaces.items():
        if v.strip() == ";":
            print(f"{k} -> {v}")

    print("\nConstants:")
    for k,v in map_replaces.items():
        try:
            x = float(v.replace(";", " ").strip())  # shouldn't have ; though
        except:
            continue
        print(f"{k} -> {v}")
