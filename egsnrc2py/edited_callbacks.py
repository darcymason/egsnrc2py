# Keep track of which calls currently still point back to an f2py conversion
# These will be converted to globals, e.g. ranlux = egsfortran.ranlux

fortran_calls = [
    "ranlux"
]
randomset = """
def randomset():
    global rng_seed

    if rng_seed > 24:
        ranlux(rng_array)
        rng_seed = 1

    random_num = rng_array[rng_seed]
    rng_seed += 1

    return random_num
"""