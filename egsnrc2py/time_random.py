from egsnrc2py import egsfortran
from random import random
from collections import deque

# Want to use ranlux for testing Python code's equivalence to Mortran,
# but out of interest check timing:
# Call functions with timeit to see relative times
# Consuming 10^6 randoms on ~4 yr old laptop in virtual linux box:
#    ~90 ms (Python random lib), 
#   ~320 ms (f2py ranlux via generator)
#   ~570 ms (f2py ranlux via array indexing)

rng_seed = egsfortran.randomm.rng_seed
rng_array = egsfortran.randomm.rng_array

def consume_iterator(iterator):
    """Consume iterator with minimal overhead"""
    deque(iterator, maxlen=0)

def randomset():
    global rng_seed

    ranlux = egsfortran.ranlux

    if rng_seed > 24:
        ranlux(rng_array)
        rng_seed = 1

    random_num = rng_array[rng_seed-1]
    rng_seed += 1

    return random_num


def random_iter(luxury_level, seedin):
    ranlux = egsfortran.ranlux
    egsfortran.init_ranlux(luxury_level, seedin)
    rng_array = egsfortran.randomm.rng_array
    ranlux(rng_array)
    
    while True:
        yield from rng_array
        ranlux(rng_array)

def randoms_generator(num_randoms=1_000_000):
    it = random_iter(1, 1)
    consume_iterator(next(it) for x in range(num_randoms))

def randomsets_indexing(num_randoms=1_000_000):
    global rng_seed

    egsfortran.init_ranlux(1, 1)
    egsfortran.ranlux(rng_array)
    rng_seed = 1

    consume_iterator(randomset() for x in range(num_randoms))

def python_randoms(num_randoms=1_000_000):
    consume_iterator(random() for x in range(num_randoms))