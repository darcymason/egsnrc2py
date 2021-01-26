
from egsnrc2py.config import (
    REAL, ENERGY_PRECISION, INTEGER, LOGICAL, AUTO_TRANSPILE_PATH, STRING
)
from dataclasses import dataclass
from textwrap import wrap
import re


class Common:
    def __init__(self):
        self.all_names = self.arr_names + self.var_names

    def python_common_block(self, egsfortran_name="egsfortran"):
        common_lines = []
        common = self.common_name
        common_lines.append(f"\n#  COMMON block {common} --------")
        common_lines.append(f"{common} = {egsfortran_name}.{common}")
        for var in self.all_names:
            common_lines.append(f"{var} = {common}.{var}")

        return "\n".join(common_lines)

    def python_global_line(self):
        name_list = ", ".join(self.all_names)

        if len(name_list) < 70:
            global_str = f"global {name_list}"

        else:
            global_str = "\n".join(
                f"global {line}"
                for line in wrap(name_list)
            )
        return f"# {self.common_name}\n{global_str}"

class ComplexCommon(Common):
    """Base class for generating from close-to-Mortran/Fortran style input

    Typically, capture the `all_vars` from a Fortran version of common block,
    replacing array dimensions with the `params.py` constants;
    and the 'all_types_and_comments` from the Mortran macros definition blocks
    separated by variable type.
    """
    def __init__(self):
        # Change the "TYPE": {var1: comment, var2:comment}
        #  to var1: (type, comment), var2: (type, comment)
        types_comments = {}
        for _type, names_comments in self.all_types_and_comments.items():
            for names, comment in names_comments.items():
                for i, name in enumerate([n.strip() for n in names.split(",")]):
                    types_comments[name.lower()] = (_type, comment if i==0 else "")
        self.types_comments = types_comments

        self.vars = []
        opt_args = r"(\w*,)*\w*"
        for match in re.finditer(rf"[\w_]*(\({opt_args}\))?,?", self.all_vars, flags=re.MULTILINE):
            name = match.group(0)
            if name.endswith(","):
                name = name[:-1]
            if name:
                self.vars.append(name)

        self.var_names = [name for name in self.vars if "(" not in name]
        self.arr_names = [x[0] for x in self.arr_names_types_comments()]
        self.all_names = self.var_names + self.arr_names

    def arr_names_types_comments(self):
        for name in self.vars:
            if "(" not in name:  # simple scalar
                continue  # this loop only for arrays
            name, args = name.split("(")
            shape_str = f"({args.upper()}"
            arr_type, comment = self.types_comments[name]
            yield name, shape_str, arr_type, comment


class Randomm(ComplexCommon):
    common_name = "randomm"
    all_vars = "rng_array(24),seeds(25),rng_seed"

    # RANDOM from ranlux
    all_types_and_comments = {
        REAL:{
            "rng_array":    "contains 24 random numbers",
        },
        INTEGER: {
            "rng_seed":     "current pointer for rng_array",
            "seeds":        "for storing the rng state",
        },
    }
randomm = Randomm()


# REPLACE {$COMIN-ELECTR;} WITH {;COMIN/DEBUG,BOUNDS,ELECIN,EPCONT,MEDIA,MISC,STACK,THRESH,
# UPHIIN,UPHIOT,USEFUL,USER,RANDOM,ET-Control,CH-Steps,EGS-IO,
#          EGS-VARIANCE-REDUCTION,EMF-INPUTS/;

class Stack(Common):
    common_name = "stack"
    arr_max = "MXSTACK"
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
stack = Stack()


class Bounds(Common):
    """CUTOFF ENERGIES & VACUUM TRANSPORT DISTANCE"""
    common_name = "bounds"
    arr_max = "MXREG"
    arr_names = "ecut pcut".split()
    arr_types = [REAL] * 2
    arr_comments = [
        "Minimum electron transport energy",
        "Minimum photon transport energy",
    ]

    var_names = ["vacdst"]
    var_types = [REAL]
    var_comments = ["Infinity (1E8)"]
bounds = Bounds()


class Media:
    """NAMES OF MEDIA CURRENTLY BEING USED"""
    common_name = "media"
    arr_max = "MXMED"
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
media = Media()
class ElecIn(ComplexCommon):
    """ELECTRON TRANSPORT INPUT"""
    common_name = "elecin"
    all_vars = (
        # Array dimensions will be capitalized in processing
        "esig_e(mxmed),psig_e(mxmed),esige_max,psige_max,"
        "range_ep(1,mxeke,mxmed),e_array(mxeke,mxmed),etae_ms0(mxeke,mxmed),"
        "etae_ms1(mxeke,mxmed),etap_ms0(mxeke,mxmed),etap_ms1(mxeke,mxmed),"
        "q1ce_ms0(mxeke,mxmed),q1ce_ms1(mxeke,mxmed),q1cp_ms0(mxeke,mxmed),"
        "q1cp_ms1(mxeke,mxmed),q2ce_ms0(mxeke,mxmed),q2ce_ms1(mxeke,mxmed),"
        "q2cp_ms0(mxeke,mxmed),q2cp_ms1(mxeke,mxmed),blcce0(mxeke,mxmed),"
        "blcce1(mxeke,mxmed),eke0(mxmed),eke1(1),xr0(mxmed),teff0(mxmed),"
        "blcc(mxmed),xcc(mxmed),esig0(mxeke,mxmed),esig1(mxeke,mxmed),"
        "psig0(mxeke,mxmed),psig1(mxeke,mxmed),ededx0(mxeke,mxmed),"
        "ededx1(mxeke,mxmed),pdedx0(mxeke,mxmed),pdedx1(mxeke,mxmed),"
        "ebr10(mxeke,mxmed),ebr11(mxeke,mxmed),pbr10(mxeke,mxmed),"
        "pbr11(mxeke,mxmed),pbr20(mxeke,mxmed),pbr21(mxeke,mxmed),"
        "tmxs0(mxeke,mxmed),tmxs1(mxeke,mxmed),expeke1(mxmed),iunrst(mxmed),"
        "epstfl(mxmed),iaprim(mxmed),sig_ismonotone(1,mxmed)"
    )

    all_types_and_comments = {
        REAL: {
            "esig_e":      ("maximum electron cross section per energy loss"
                            "for each medium"),
            "psig_e":      ("maximum positron cross section per energy loss"
                            "for each medium"),
            "esige_max":   "maximum electron cross section per energy loss",
            "psige_max":   "maximum electron cross section per energy loss",
            "range_ep":    "electron (0) or positron (1) range",
            "E_array":     "table energies",
            "etae_ms0,etae_ms1": "for interpolation of screening parameter (e-)",
            "etap_ms0,etap_ms1": "for interpolation of screening parameter (e+)",
            "q1ce_ms0,q1ce_ms1": "for interpolation of q1 correction due to spin (e-)",
            "q1cp_ms0,q1cp_ms1": "for interpolation of q1 correction due to spin (e+)",
            "q2ce_ms0,q2ce_ms1": "for interpolation of q2 correction due to spin (e-)",
            "q2cp_ms0,q2cp_ms1": "for interpolation of q2 correction due to spin (e+)",
            "blcce0,blcce1": ["for interpolation of scattering power correction ",
                            "necessary to account for scattering already taken  ",
                            "into account in discrete Moller/Bhabha             ",
                            ],
            "":       "",
            "expeke1":     "Exp(1/eke1)-1",
            "EKE0,EKE1": "table for kinetic energy indexing",
            "XR0":         "unused, but read in HATCH",
            "TEFF0":       "unused, but read in HATCH",
            "BLCC":        "b lower case sub c",
            "XCC":         "chi sub-c-c",
            "ESIG0,ESIG1": "used for electron cross section interpolation",
            "PSIG0,PSIG1": "used for positron cross section interpolation",
            "EDEDX0,EDEDX1": "used for electron dE/dx interpolation",
            "PDEDX0,PDEDX1": "used for positron dE/dx interpolation",
            "EBR10,EBR11": "used for e- branching into brems interpolation",
            "PBR10,PBR11": "used for e+ branching into brems interpolation",
            "PBR20,PBR21": "used for e+ branching into Bhabha interpolation",
            "TMXS0,TMXS1":   "used for maximum step-size interpolation",
        },
        INTEGER: {
            "IUNRST":      "flag for type of stopping power (see PEGS4)",
            "EPSTFL":      "flag for ICRU37 collision stopping powers",
            "IAPRIM":      "flag for ICRU37 radiative stopping powers",
        },
        LOGICAL: {
            "sig_ismonotone": ("true, if cross section is an increasing function"
                               "of energy, false otherwise"),
        }
    }

elecin = ElecIn()


class Uphiot(ComplexCommon):
    common_name = "uphiot"
    all_vars = "THETA,SINTHE,COSTHE,SINPHI,COSPHI,PI,TWOPI,PI5D2".lower()
    all_types_and_comments = {
        REAL: {
            'theta':  "polar scattering angle",
            'sinthe': "sin(THETA)",
            'costhe': "cos(THETA)",
            'sinphi': "sine of the azimuthal scattering angle",
            'cosphi': "cosine of the azimuthal scattering angle",
            'pi': "",
            'twopi': "",
            'pi5d2': "",
        }
    }
uphiot = Uphiot()


if __name__ == "__main__":
    # elec = ElecIn()
    # for info in elec.arr_names_types_comments():
    #     print(info)
    # # print(elec.types_comments)

    for obj in list(globals().values()):
        if isinstance(obj, Common):
            print(obj.python_common_block())