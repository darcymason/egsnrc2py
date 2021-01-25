import pytest

pytest.importorskip("egsnrc2py.egsfortran")  # from numpy.f2py, used while in transition


from egsnrc2py.edited_callbacks import randomset_def, randomiter_def
import egsnrc2py.egsfortran as egsfor
import numpy as np
import sys


ranlux = egsfor.ranlux
init_ranlux = egsfor.init_ranlux
rng_seed = egsfor.randomm.rng_seed
rng_array = egsfor.randomm.rng_array


# @pytest.mark.skipif(sys.platform=="win32")
class TestRandom:
    def test_ranlux(self):
        """Test RANLUX random number generator"""
        # Based on ranlux_test.mortran in EGSnrc
        # Assumes default egsfortran random macros
        # Exec seems to work if-and-only-if globals() arg passed and no locals()
        exec(randomset_def, globals())  # actually generate the fn def into local scope
        global rng_seed  # absolutely essential, else Python treats as local and starts at 0

        init_ranlux(1, 1)
        ranlux(rng_array)
        rng_seed = 1
        first10_sum = sum(randomset() for x in range(10))
        assert 6.041212022304535 == first10_sum
        first10k_sum = first10_sum + sum(randomset() for x in range(9990))
        assert 5037.366532325745 == first10k_sum
        first_million_sum = first10k_sum + sum(randomset() for x in range(990_000))
        assert 500181.8234628493 == first_million_sum

    def test_ranlux_iter(self):
        """Test RANLUX random number generator as an iterator"""
        # Based on ranlux_test.mortran in EGSnrc
        # Assumes default egsfortran random macros
        # Exec seems to work if-and-only-if globals() arg passed and no locals()
        exec(randomiter_def, globals())  # actually generate the fn def into local scope

        init_ranlux(1, 1)
        ranlux(rng_array)
        rnd_iter = random_iter()
        first10_sum = sum(next(rnd_iter) for i in range(10))
        assert 6.041212022304535 == first10_sum
        first10k_sum = first10_sum + sum(next(rnd_iter) for x in range(9990))
        assert 5037.366532325745 == first10k_sum
        first_million_sum = first10k_sum + sum(next(rnd_iter) for x in range(990_000))
        assert 500181.8234628493 == first_million_sum
