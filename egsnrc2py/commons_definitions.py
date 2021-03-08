
from egsnrc2py.config import (
    REAL, ENERGY_PRECISION, LOGICAL, AUTO_TRANSPILE_PATH, STRING,
    INTEGER, SHORT_INT
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


class Media(Common):
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


class Thresh(ComplexCommon):
    common_name = "thresh"

    all_vars = (
        "RMT2,RMSQ,"
        "AP(mxmed),AE(mxmed),UP(mxmed),UE(mxmed),TE(mxmed),THMOLL(mxmed)"
    ).lower()
    all_types_and_comments = {
        REAL: {
            'rmt2':  "2*electron mass in MeV",
            'rmsq':  "electron mass squared in MeV**2",
            'ap':    "photon creation threshold energy",
            'ae':    "electron creation threshold energy (total)",
            'up':    "upper photon energy in PEGS4 data set",
            'ue':    "upper electron energy in PEGS4 data set",
            'te':    "electron creation threshold energy (kinetic)",
            'thmoll':"Moller threshold = AE + TE",
        }
    }
thresh = Thresh()


class Useful(ComplexCommon):
    common_name = "useful"

    all_vars = "PZERO,PRM,PRMT2,RM,MEDIUM,MEDOLD".lower()

    all_types_and_comments = {
        ENERGY_PRECISION: {
            'pzero':   "precise zero",
            'prm':     "precise electron mass in MeV",
            'prmt2':   "2*PRM",
        },
        REAL: {
            'rm':      "electron mass in MeV",
        },
        INTEGER: {
            'medium':  "medium index of current region",
            'medold':  "medium index of previous region"
            # The rest mass value is as recommended by CODATA 2014"
            # http://physics.nist.gov/cgi-bin/cuu/Value?mec2mev"
            # DATA RM,PRM,PRMT2,PZERO/0.5109989461,0.5109989461,1.0219978922,0.D0/;
        },
    }
useful = Useful()

class Geom(ComplexCommon):
    """From tutor4"""
    common_name = "geom"

    all_vars = "zbound"

    all_types_and_comments = {
        REAL: {
            "zbound": ""
        },
        INTEGER: {
            "iwatch": ""
        },
    }
geom = Geom()
class Score(ComplexCommon):
    """From tutor4"""
    common_name = "score"

    all_vars = "escore(3),iwatch"

    all_types_and_comments = {
        REAL: {
            "escore": ""
        },
        INTEGER: {
            "iwatch": ""
        },
    }
score = Score()


class Misc(ComplexCommon):
    """Miscellaneous Common"""
    common_name = "misc"

    all_vars = "dunit,kmpi,kmpo,rhor(mxreg),med(mxreg),iraylr(mxreg),iphotonucr(mxreg)"

    all_types_and_comments = {
        REAL: {
            'dunit':   "unit scaling factor",
            'rhor':    "density of a given region",
        },
        INTEGER: {
            'kmpi':    "fortran unit number of the pegs4 datafile",
            'kmpo':    "fortran unit number of pegs4 echo file",
        },
        SHORT_INT: {
            'med':   "medium number for a given region",
            'iraylr': "Rayleigh switch for a given region",
            'iphotonucr': "photonuclear switch for a given region",
        }
    }
misc = Misc()


class Epcont(ComplexCommon):
    """Electron-photon control variables"""
    common_name = "epcont"

    all_vars = (
        "edep,edep_local,tstep,tustep,ustep,tvstep,vstep,"
        "rhof,eold,enew,eke,elke,gle,e_range,"
        "x_final,y_final,z_final,"
        "u_final,v_final,w_final,"
        "idisc,irold,irnew,iausfl(mxaus)"
    )

    all_types_and_comments = {
        ENERGY_PRECISION: {
            'edep':   "energy deposition in mev",
            'edep_local': "local energy deposition in mev",
        },
        REAL: {
            'tstep': "distance to a discrete interaction",
            'tustep': "intended step length, befor check with geometry",
            'ustep': "transport distance calculated from tustep",
            'vstep': "transport distance after truncation by howfar",
            'tvstep': "curved path-length calculated from tvstep",
            'rhof': "mass density ratio",
            'eold': "energy before deduction of energy loss",
            'enew': "energy after  deduction of energy loss",
            'eke': "kinetic energy",
            'elke': "log(eke)",
            'gle': "log(energy) in photon",
            'e_range': "range of electron before an iarg=0 ausgab call",
            'x_final': "position at end of step",
            'y_final': "",
            'z_final': "",
            'u_final':("direction at end of step"
                                    "only set (and relevant) "
                                    "for electrons"),
            'v_final': "",
            'w_final': "",

        },
        INTEGER: {
            'idisc': "flag indicating user discard",
            'irold': "region before transport",
            'irnew': "region after transport",
            'iausfl': "flags for ausgab calls",
        }
    }
epcont = Epcont()


class Egs_vr(ComplexCommon):
    """EGSnrc internal Variance Reduction Macros"""
    common_name = "egs_vr"

    all_vars = (
        "e_max_rr(mxreg),prob_rr,nbr_split,i_play_rr,i_survived_rr,"
        "n_rr_warning,i_do_rr(mxreg)"
    )

    all_types_and_comments = {
        REAL: {
            'e_max_rr': "max energy at which to do range rejection (RR)",
            'prob_RR':  "probability for survival in R. Roulette",
        },
        INTEGER: {
            'nbr_split':      "do brems splitting if > 1",
            'i_play_RR':      "0 => don't play Russian Roulette; 1 => play Russian Roulette",
            'i_survived_RR':  ("0 => all particles survive RR; n => n particles were"
                                " eliminated by RR in this interaction"),
            'n_RR_warning':   "a counter for user errors",
        },
        SHORT_INT: {
            'i_do_rr':  "0 => no RR, region by region; 1 => there is RR",
        },
    }
egs_vr = Egs_vr()


class Ch_steps(ComplexCommon):
    """EGSnrc internal Variance Reduction Macros"""
    common_name = "ch_steps"

    all_vars = "count_pii_steps,count_all_steps,is_ch_step"

    all_types_and_comments = {
        REAL: {
            'count_pii_steps': "",  # count_pII_steps, but f2py loses case
            'count_all_steps': "",
        },
        LOGICAL: {
            'is_ch_step': "",
        },
    }
ch_steps = Ch_steps()


class Nist_brems(ComplexCommon):
    """Brems commons"""
    common_name = "nist_brems"

    all_vars = (
        "nb_fdata(mxbrxs,mxbres,mxmed),"  # actually (0:mxbrxs,...)
        "nb_xdata(mxbrxs,mxbres,mxmed),"  # actually (0:mxbrxs,...)
        "nb_wdata(mxbrxs,mxbres,mxmed),"
        "nb_idata(mxbrxs,mxbres,mxmed),"
        "nb_emin(mxmed),nb_emax(mxmed),"
        "nb_lemin(mxmed),nb_lemax(mxmed),"
        "nb_dle(mxmed),nb_dlei(mxmed),"
        "log_ap(mxmed)"
    )

    all_types_and_comments = {
        REAL: {
            "nb_fdata": "",
            "nb_xdata": "",
            "nb_wdata": "",
            "nb_emin": "",
            "nb_emax": "",
            "nb_lemin": "",
            "nb_lemax": "",
            "nb_dle": "",
            "nb_dlei": "",
            "log_ap": "",
        },
        INTEGER: {
            "nb_idata": "",
        },
    }
nist_breams = Nist_brems()


class Brempr(ComplexCommon):
    """EGSnrc BREMPR"""
    common_name = "brempr"

    all_vars = (
        "dl1(8,mxmed),dl2(8,mxmed),dl3(8,mxmed),"
        "dl4(8,mxmed),dl5(8,mxmed),dl6(8,mxmed),"
        "alphi(2,mxmed),bpar(2,mxmed),delpos(2,mxmed),"
        "wa(mxmed,mxel),pz(mxmed,mxel),zelem(mxmed,mxel),rhoz(mxmed,mxel),"
        "pwr2i(mxpwr2i),"
        "delcm(mxmed),zbrang(mxmed),lzbrang(mxmed),nne(mxmed),"
        "ibrdst,iprdst,ibr_nist,pair_nrc,itriplet,"
        "asym(mxmed,mxel,2)"
    )

    all_types_and_comments = {
        REAL: {
            "dl1": ("Parameter for the fit of the screening"
                   "rejection function, eq. (2.7.14 and 15)"),
            "dl2": "",
            "dl3": "",
            "dl4": "",
            "dl5": "",
            "dl6": "",
            "alphi":  "Prob. for the (1-br)/br part in `brems`, eq. (2.7.64)",
            "bpar":   "Prob. for the 12*(br-1/2)**2 part in `pair`, eq. (2.7.105)",
            "delpos": "maximum delta, eq. (2.7.31)",
            "wa":     "atomic weight",
            "pz":     "atomic fraction of an element in a compound",
            "zelem":  "Z for a given component",
            "rhoz":   "density of an element in a compound",
            "pwr2i":  "powers of 1/2 (used for sampling (1-br)/br",
            "delcm":  "136*m*exp(Zg), eq. (2.7.51)",
            "zbrang": "composite factor for angular distributions",
            "lzbrang":"-log(Zbrang)",
        },
        INTEGER: {
            "nne":    "number of elements/compound",
            "ibrdst": "flag to switch on bremsstrahlung angular distributions",
            "iprdst": "flag to switch on pair angular distributions",
            "ibr_nist":  "use the NIST bremsstrahlung cross sections",
            "itriplet":  "if set to 1, explicitely simulate triplet events",
            "pair_nrc":  ("=0 => use Bethe-Heitler pair cross sections"
                          "=1 => use the NRC pair cross sections"),
        },
        STRING: {
            "asym": "",
        }
    }
brempr = Brempr()


class Eii_data(ComplexCommon):
    """EGSnrc EII-DATA common block"""
    common_name = "eii_data"

    all_vars = (
        "eii_xsection_a(max_eii_bins),eii_xsection_b(max_eii_bins),"
        "eii_cons(mxmed),eii_a(max_eii_shells),eii_b(max_eii_shells),"
        "eii_l_factor,eii_z(max_eii_shells),eii_sh(max_eii_shells),"
        "eii_nshells(mxelement),eii_nsh(mxmed),eii_first(mxmed,mxel),"
        "eii_no(mxmed,mxel),eii_flag"
    )

    all_types_and_comments = {
        REAL: {
            "eii_xsection_a": "EII x-section interpolation coeff.",
            "eii_xsection_b": "EII x-section interpolation coeff.",
            "eii_cons": "",
            "eii_a": "energy grid coeff. for each shell ",
            "eii_b": "energy grid coeff. for each shell ",
            "eii_l_factor": "L-shell EII xsection scaling factor",
        },
        INTEGER: {
            "eii_z": "Z of each shell",
            "eii_sh": "shell type (1=K, 2=LI, eyc.)",
            "eii_nshells": "No. of EII shells for each element",
            "eii_nsh": "No. of EII shells for each medium ",
            "eii_first": "First EII shell in the list of shells",
            "eii_no": "N. of EII shells",
            "eii_flag": (
                "EII flag                          "
                "         = 0 => no EII            "
                "         = 1 => simple EII        "
                "         > 1 => future use        "
            ),
        },
    }
eii_data = Eii_data()


class Edge(ComplexCommon):
    """Contains binding energies

    Contains binding energies for K,L1,L2,L3, 'average' M
    and 'average' N shells; photo-absorption interaction probabilities
    with these shells; + fluorescence, Auger, Coster-Kronig transition
    probabilities

    IEDGFL is a flag for turning on/off atomic relaxations
    IPHTER is a flag for turning on/off photo-lectron
           angular distribution
    both are left-overs from the previous coding
    Have put now also data to calculate elemental PE
    cross sections needed to sample the element the photon
    is interacting with.

    """
    common_name = "edge"

    all_vars = (
        "binding_energies(mxshxsec,mxelement),"
        "interaction_prob(mxshell,mxelement),"
        "relaxation_prob(mxtrans,mxelement),"
        "edge_energies(mxedge,mxelement),"
        "edge_number(mxelement),"
        "edge_a(mxedge,mxelement),"
        "edge_b(mxedge,mxelement),"
        "edge_c(mxedge,mxelement),"
        "edge_d(mxedge,mxelement),"
        "iedgfl(mxreg),iphter(mxreg)"
    )
    all_types_and_comments = {
        REAL: {
            "binding_energies": " K,L1,L2,L3,M,N binding energies  ",
            "interaction_prob": (" prob. for interaction with one of"
                                 " the above shells (provided photon"
                                 " energy is above be)              "),
            "relaxation_prob":  " relaxation probabilities         ",
            "edge_energies":    " photo-absorption edge energies   ",
            "edge_a": " photo cross section fit parameters ",
            "edge_b": "",
            "edge_c": "",
            "edge_d": "",
        },
        SHORT_INT: {
            "IEDGFL":  "flag for switching on fluorscent emission",
            "IPHTER":  "flag for switching on photo-electron angular distr.",
        },
        INTEGER: {
            "edge_number": " number of `edges' for each element",
        }
    }
edge = Edge()


class Photin(ComplexCommon):
    """EGSnrc Photon ('Photin') common block"""
    common_name = "photin"

    all_vars = (
        "ebinda(mxmed),"
        "ge0(mxmed),ge1(mxmed),"
        "gmfp0(mxge,mxmed),gbr10(mxge,mxmed),gbr20(mxge,mxmed),"
        "gmfp1(mxge,mxmed),gbr11(mxge,mxmed),gbr21(mxge,mxmed),"
        "rco0(mxmed),rco1(mxmed),"
        "rsct0(mxrayff,mxmed),rsct1(mxrayff,mxmed),"
        "cohe0(mxge,mxmed),cohe1(mxge,mxmed),"
        # ali:photonuc, 1 line
        "photonuc0(mxge,mxmed),photonuc1(mxge,mxmed),"
        "dpmfp,"
        "mpgem(mxsge,mxmed),"
        "ngr(mxmed)"
    )

    all_types_and_comments = {
        REAL: {
            "ebinda":  "energy of the K-edge for a given medium",
            "ge0":     "used for indexing in logarithmic interpolations",
            "ge1": "",
            "gmfp0":   "used for gamma MFP interpolation",
            "gmfp1":   "",
            "gbr10":   "used for branching into pair interpolation",
            "gbr11":   "",
            "gbr20":   "used for branching into Compton interpolation",
            "gbr21":   "",
            "rco0":    "used for indexing in momentum trans. sampling in Rayleigh",
            "rco1":    "",
            "rsct0":   "used for interpolation of momentum trans. func. in R",
            "rsct1":   "",
            "cohe0":   "used for Rayleigh modification interpolation",
            "cohe1":   "",
            "photonuc0": "used for photonuclear modification interpolation",
            "photonuc1": "",
            "dpmfp":     "number of MFP's to go to the next interaction",
        },
        INTEGER: {
            "mpgem":  "??? ",
            "ngr":    "array size for Rayleigh scattering data",
        },
    }
photin = Photin()


class X_options(ComplexCommon):
    """Flags for EADL relaxation and Sabbatucci and Salvat PE xsections"""
    common_name = "x_options"

    all_vars = (
        "eadl_relax,mcdf_pe_xsections"
     )

    all_types_and_comments = {
        LOGICAL: {
            "eadl_relax":       "Use EADL relaxation",
            "mcdf_pe_xsections": "Use Sabbatucci and Salvat PE xsections",
        },
    }
x_options = X_options()


if __name__ == "__main__":
    # elec = ElecIn()
    # for info in elec.arr_names_types_comments():
    #     print(info)
    # # print(elec.types_comments)

    for obj in list(globals().values()):
        if isinstance(obj, Common):
            print(obj.python_common_block())