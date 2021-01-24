
from pathlib import Path
import tut1

# Define paths based on current location
# this python file should be in the user code directory under egs_home
#  - e.g. in tutor1, tutor2, etc.
HERE  = Path(__file__).resolve().parent
EGS_HOME = HERE.parent
USER_CODE = HERE.name
PEGS_FILE = "tutor_data"

def print_info():
    for name in ('egs_home', 'user_code', 'pegs_file'):
        print(f"{name}: ", getattr(tut1.egs_io, name))


# tut1.tutor1()

# tut1.egs_init()
tut1.egs_set_defaults()
print_info()
tut1.egs_check_arguments()
print("---After check_arguments--")
print_info()

tut1.egs_io.egs_home = f"{str(EGS_HOME) + '/':<128}"  # need trailing "/"
tut1.egs_io.pegs_file = f"{PEGS_FILE:<256}"
tut1.egs_io.user_code = f"{USER_CODE:<64}"
print("---After setting pegs_file and user_code --")
print_info()

tut1.egs_init1()

tut1.hatch()

print("Sample some arrays")
print("ESIG_E", tut1.elecin.esig_e)
# print("Defined ELECIN variables", dir(tut1.elecin))
print("ededx0[1,0]", tut1.elecin.ededx0[1,0])




