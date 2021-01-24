
from egsnrc2py.commons_definitions import *

# REPLACE {$COMIN-ELECTR;} WITH {;COMIN/DEBUG,BOUNDS,ELECIN,EPCONT,MEDIA,MISC,STACK,THRESH,
# UPHIIN,UPHIOT,USEFUL,USER,RANDOM,ET-Control,CH-Steps,EGS-IO,
#          EGS-VARIANCE-REDUCTION,EMF-INPUTS/;

def make_imports() -> str:
    lines = [
        "import numpy as np",
        "from egsnrc import params",
        "",
    ]
    return lines

def make_arrays(
    arr_names: list,
    arr_types: list,
    arr_comments: list,
    max_dim: str
) -> str:
    lines = []
    in_comment_block = False
    for arr_name, arr_type, arr_comment in zip(arr_names, arr_types, arr_comments):
        starting_comment_block = arr_comment.startswith("*")
        if in_comment_block and arr_comment:
            in_comment_block = False
            if not starting_comment_block:
                lines.append("")
        if isinstance(arr_comment, list):
            lines.extend([f"    # {line}" for line in arr_comment])
        if starting_comment_block:
            in_comment_block = True
            lines.append("")
            lines.append(f"    # {arr_comment[1:]}")
            arr_comment = ""
        comment = f"  # {arr_comment}" if arr_comment else ""
        if arr_type == STRING:
            var_decl = f'[""] * ({max_dim} + 1)'
        else:
            var_decl = f"np.zeros({max_dim} + 1, {arr_type})"
        lines.append(
            f'    "{arr_name}": {var_decl},{comment}'
        )
    return lines


def make_vars(var_names: list, var_types: list, var_comments: list) -> str:
    lines = []
    for var_name, var_type, var_comment in zip(var_names, var_types, var_comments):
        if isinstance(var_comment, list):  # will post after the variable
            comment = ""
        else:
            comment = f"  # {var_comment}" if var_comment else ""
        var_assign = f"{var_type}(0)" if var_type != STRING else '""'
        lines.append(f'    "{var_name}": {var_assign},{comment}')
        if isinstance(var_comment, list):
            lines.extend([f"    # {line}" for line in var_comment])
            lines.append("")
    return lines


def call_signature(com) -> str:
    arr_sigs = []
    for arr_name, arr_type in zip(com.arr_names, com.arr_types):
        arr_type_str = f": np.ndarray[{arr_type}]" if arr_type and arr_type.startswith("np.") else ""
        arr_sigs.append(f"{arr_name}{arr_type_str}")
    var_sigs = [
        f"{var_name}: {var_type}"
        for var_name, var_type in zip(com.var_names, com.var_types)
    ]
    return ", ".join(arr_sigs + var_sigs)


def make_common_section(com) -> str:
    """Generate common section for one of the original COMMON blocks"""
    lines = [f"# {com.common_name} ========== "]
    lines.append(f"# Call signature:\n# {call_signature(com)}")
    lines.append(f"{com.common_name} = {{")
    # update array types
    lines.append("    # Arrays -------")
    lines.extend(
        make_arrays(
            com.arr_names, com.arr_types, com.arr_comments, com.arr_max
        )
    )
    lines.append("")
    lines.append("    # Scalars -----")
    lines.extend(make_vars(com.var_names, com.var_types, com.var_comments))
    lines.append("}")
    return lines


def make_common_file(filename: str):
    """Generate the common.py file which contains the Python equivalent
    of the "COMMON" blocks from the original Mortran code"""
    lines = make_imports()
    for com in [Stack, Bounds, Media]:
        lines.extend(make_common_section(com))

    with open(filename, "w") as f:
        f.write("\n".join(lines))


if __name__ == "__main__":
    make_common_file(AUTO_TRANSPILE_PATH / "common.py")