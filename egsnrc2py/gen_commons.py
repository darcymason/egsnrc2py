
from egsnrc2py.config import REAL, ENERGY_PRECISION, INTEGER, LOGICAL, AUTO_TRANSPILE_PATH
from dataclasses import dataclass

STRING = "str"


# REPLACE {$COMIN-ELECTR;} WITH {;COMIN/DEBUG,BOUNDS,ELECIN,EPCONT,MEDIA,MISC,STACK,THRESH,
# UPHIIN,UPHIOT,USEFUL,USER,RANDOM,ET-Control,CH-Steps,EGS-IO,
#          EGS-VARIANCE-REDUCTION,EMF-INPUTS/;
@dataclass
class Stack:
    common_name = "stack"
    arr_max = "params.MXSTACK"
    arr_names = "e x y z u v w dnear wt iq ir latch".split()
    arr_types = [ENERGY_PRECISION] + [REAL]*8 + [INTEGER]*3
    var_names = "latchi np npold".split()
    var_types = [INTEGER]*3
    arr_comments = [
        "total particle energy",
        "*particle co-ordinates",
        "",
        "",
        "*particle direction cosines",
        "",
        "",
        "perpendicular distance to nearest boundary",
        "particle weight",
        "charge, -1 for electrons, 0 for photons, 1 for positrons",
        "current region",
        "extra phase space variable",
    ]
    var_comments = [
        "needed because shower does not pass latch-BLOCK DATA sets 0",
        "stack pointer",
        "stack pointer before an interaction",
    ]

@dataclass
class Bounds:
    """CUTOFF ENERGIES & VACUUM TRANSPORT DISTANCE"""
    common_name = "bounds"
    arr_max = "params.MXREG"
    arr_names = "ecut pcut".split()
    arr_types = [REAL] * 2
    arr_comments = [
        "Minimum electron transport energy",
        "Minimum photon transport energy",
    ]

    var_names = ["vacdst"]
    var_types = [REAL]
    var_comments = ["Infinity (1E8)"]


class Media:
    """NAMES OF MEDIA CURRENTLY BEING USED"""
    common_name = "media"
    arr_max = "params.MXMED"
    arr_names = (
        "rlc,rldu,rho,msge,mge,mseke,meke,mleke,mcmfp,mrange,"
        "iraylm,iphotonucm,media"
    ).split(",")
    arr_types = [REAL]*3 + [INTEGER]*9 + [STRING]
    arr_comments = [
        "radiation length in centimeters for a given medium",
        "radiation length after user scaling over-ride",
        "mass density of a given medium",
        #  apx, upx;"new photon xsection data thresholds"
        "??? ",
        "number of photon mapped energy intervals for a given medium",
        "??? ",
        "number of e mapped energy intervals for a given medium",
        "??? ",
        "??? ",
        "??? ",
        "Rayleigh switch for a given medium",
        "photonuclear switch for a given medium", # IPHOTONUCM
        "media names", # MEDIA
    ]
    var_names = (
        "iphotonuc nmed eii_xfile photon_xsections comp_xsections "
        "photonuc_xsections"
    ).split()
    var_types = [INTEGER]*2 + [STRING]*4
    var_comments = [
        "set to 1 if any IPHOTONUCM is set to 1",  # IPHOTONUC
        "number of media",  # NMED
        [
            "Defaults to eii_ik.data if On or Off options selected ",
            "which is the EII implemented by Iwan for EGSnrc ",
            "else, following options available: ",
            "  eii_'casnati'.data    ",
            "  eii_'kolbenstvedt'.data ",
            "  eii_'gryzinski'.data ",
            "these must be in $HEN_HOUSE/data",
        ],  # eii_xfile
        [
            "If photon_xsections is not empty, photon cross sections will be",
            "re-initialized using data files  ",
            "  'photon_xsection'_photo.data   ",
            "  'photon_xsection'_pair.data    ",
            "  'photon_xsection'_triplet.data ",
            "  'photon_xsection'_rayleigh.data",
            "that must be placed in $HEN_HOUSE/data",
        ], #  photon_xsections
        [
            "If comp_xsections is not empty or not set to 'default' and ",
            "bound Compton scattering is On, then total Compton cross sections ",
            "will be taken from 'comp_xsections'_compton.data ",
            "instead of being computed from the theoretical expressions",
        ], # comp_xsections
        [
            "If photonuc_xsections is not empty or not set to 'default',",
            "the photonuclear cross sections will be taken from",
            "'photonuc_xsections'_photonuc.data instead of using the data",
            "in the default file iaea_photonuc.data.",
        ],  # photonuc_xsections;
    ]


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