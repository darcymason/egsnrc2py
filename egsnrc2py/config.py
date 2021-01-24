from pathlib import Path

BASE_PATH = Path(__file__).resolve().parent

MORTRAN_SOURCE_PATH = BASE_PATH / "HEN_HOUSE" / "src"
EGS_HOME_PATH = BASE_PATH / "egs_home"
AUTO_TRANSPILE_PATH = BASE_PATH / "autotranspile"
TEMPLATES_PATH = BASE_PATH / "templates"

# Type conversions
# From egsnrc.macros:
#   REPLACE {$LOGICAL} WITH {;logical}
#   REPLACE {$REAL}    WITH {;real*8}
#   REPLACE {$INTEGER} WITH {;integer*4}
#   REPLACE {$LONG_INT} WITH {;integer*8} "change this to integer*4 for compilers"
#                                       "that do not support integer*8"
#   REPLACE {$SHORT_INT} WITH {;integer*2} "change this to integer*4 for compilers"
#                                       "that do not support integer*2"
REAL = "np.float64"
ENERGY_PRECISION = "np.float32"
INTEGER = "np.int32"
LOGICAL = "bool"
STRING = "str"

default_float = "np.float32"

