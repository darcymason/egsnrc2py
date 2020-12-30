import re

import sys


if sys.version_info < (3, 8):
    raise ImportError("Need Python 3.8 or later")

# XXX needs to be OrderedDict!!
subs = {
    r'"(.*?)"': r'# \1',  # comments in quotes
    r'"(.*)': r'# \1',  # comment without end quote
    r";(\s*)$": r"\1",      # semi-colon at end of line
    r";(\s*)(?P<comment>#(.*?))?$": r" \g<comment>",
    r"^(\s*)IF\((.*)\)\s*\[(.*?)[;]?\](.*)$": r"\1if \2:\n\1    \3\4", # basic IF
    r"^(\s*)ELSE(.*)\[(.*)\](.*)$": r"\1else:\n\1    \3\4", # basic ELSE
    r"^(\s*)IF(\s*)?\((.*)\)(.*)$": r"\1if \3:\n\1    \4", # IF on one line
    r"if(.*)?~=": r"if\1!=", # not equals
}

if __name__ == "__main__":
    in_filename = "electr.mor"
    out_filename = "electr.py"
    with open(in_filename, 'r') as f:
        code = f.read()

    for pattern, sub in subs.items():
        code = re.sub(pattern, sub, code, flags=re.MULTILINE)

    with open(out_filename, "w") as f:
        f.write(code)
