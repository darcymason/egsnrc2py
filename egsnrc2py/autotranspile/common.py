import numpy as np
from egsnrc import params

# stack ========== 
# Call signature:
# e: np.ndarray[np.float32], x: np.ndarray[np.float64], y: np.ndarray[np.float64], z: np.ndarray[np.float64], u: np.ndarray[np.float64], v: np.ndarray[np.float64], w: np.ndarray[np.float64], dnear: np.ndarray[np.float64], wt: np.ndarray[np.float64], iq: np.ndarray[np.int32], ir: np.ndarray[np.int32], latch: np.ndarray[np.int32], latchi: np.int32, np: np.int32, npold: np.int32
stack = {
    # Arrays -------
    "e": np.zeros(params.MXSTACK + 1, np.float32),  # total particle energy

    # particle co-ordinates
    "x": np.zeros(params.MXSTACK + 1, np.float64),
    "y": np.zeros(params.MXSTACK + 1, np.float64),
    "z": np.zeros(params.MXSTACK + 1, np.float64),

    # particle direction cosines
    "u": np.zeros(params.MXSTACK + 1, np.float64),
    "v": np.zeros(params.MXSTACK + 1, np.float64),
    "w": np.zeros(params.MXSTACK + 1, np.float64),

    "dnear": np.zeros(params.MXSTACK + 1, np.float64),  # perpendicular distance to nearest boundary
    "wt": np.zeros(params.MXSTACK + 1, np.float64),  # particle weight
    "iq": np.zeros(params.MXSTACK + 1, np.int32),  # charge, -1 for electrons, 0 for photons, 1 for positrons
    "ir": np.zeros(params.MXSTACK + 1, np.int32),  # current region
    "latch": np.zeros(params.MXSTACK + 1, np.int32),  # extra phase space variable

    # Scalars -----
    "latchi": np.int32(0),  # needed because shower does not pass latch-BLOCK DATA sets 0
    "np": np.int32(0),  # stack pointer
    "npold": np.int32(0),  # stack pointer before an interaction
}
# bounds ========== 
# Call signature:
# ecut: np.ndarray[np.float64], pcut: np.ndarray[np.float64], vacdst: np.float64
bounds = {
    # Arrays -------
    "ecut": np.zeros(params.MXREG + 1, np.float64),  # Minimum electron transport energy
    "pcut": np.zeros(params.MXREG + 1, np.float64),  # Minimum photon transport energy

    # Scalars -----
    "vacdst": np.float64(0),  # Infinity (1E8)
}
# media ========== 
# Call signature:
# rlc: np.ndarray[np.float64], rldu: np.ndarray[np.float64], rho: np.ndarray[np.float64], msge: np.ndarray[np.int32], mge: np.ndarray[np.int32], mseke: np.ndarray[np.int32], meke: np.ndarray[np.int32], mleke: np.ndarray[np.int32], mcmfp: np.ndarray[np.int32], mrange: np.ndarray[np.int32], iraylm: np.ndarray[np.int32], iphotonucm: np.ndarray[np.int32], media, iphotonuc: np.int32, nmed: np.int32, eii_xfile: str, photon_xsections: str, comp_xsections: str, photonuc_xsections: str
media = {
    # Arrays -------
    "rlc": np.zeros(params.MXMED + 1, np.float64),  # radiation length in centimeters for a given medium
    "rldu": np.zeros(params.MXMED + 1, np.float64),  # radiation length after user scaling over-ride
    "rho": np.zeros(params.MXMED + 1, np.float64),  # mass density of a given medium
    "msge": np.zeros(params.MXMED + 1, np.int32),  # ??? 
    "mge": np.zeros(params.MXMED + 1, np.int32),  # number of photon mapped energy intervals for a given medium
    "mseke": np.zeros(params.MXMED + 1, np.int32),  # ??? 
    "meke": np.zeros(params.MXMED + 1, np.int32),  # number of e mapped energy intervals for a given medium
    "mleke": np.zeros(params.MXMED + 1, np.int32),  # ??? 
    "mcmfp": np.zeros(params.MXMED + 1, np.int32),  # ??? 
    "mrange": np.zeros(params.MXMED + 1, np.int32),  # ??? 
    "iraylm": np.zeros(params.MXMED + 1, np.int32),  # Rayleigh switch for a given medium
    "iphotonucm": np.zeros(params.MXMED + 1, np.int32),  # photonuclear switch for a given medium
    "media": [""] * (params.MXMED + 1),  # media names

    # Scalars -----
    "iphotonuc": np.int32(0),  # set to 1 if any IPHOTONUCM is set to 1
    "nmed": np.int32(0),  # number of media
    "eii_xfile": "",
    # Defaults to eii_ik.data if On or Off options selected 
    # which is the EII implemented by Iwan for EGSnrc 
    # else, following options available: 
    #   eii_'casnati'.data    
    #   eii_'kolbenstvedt'.data 
    #   eii_'gryzinski'.data 
    # these must be in $HEN_HOUSE/data

    "photon_xsections": "",
    # If photon_xsections is not empty, photon cross sections will be
    # re-initialized using data files  
    #   'photon_xsection'_photo.data   
    #   'photon_xsection'_pair.data    
    #   'photon_xsection'_triplet.data 
    #   'photon_xsection'_rayleigh.data
    # that must be placed in $HEN_HOUSE/data

    "comp_xsections": "",
    # If comp_xsections is not empty or not set to 'default' and 
    # bound Compton scattering is On, then total Compton cross sections 
    # will be taken from 'comp_xsections'_compton.data 
    # instead of being computed from the theoretical expressions

    "photonuc_xsections": "",
    # If photonuc_xsections is not empty or not set to 'default',
    # the photonuclear cross sections will be taken from
    # 'photonuc_xsections'_photonuc.data instead of using the data
    # in the default file iaea_photonuc.data.

}