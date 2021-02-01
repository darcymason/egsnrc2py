import pytest
from pathlib import Path

pytest.importorskip("egsnrc2py.egsfortran")  # from numpy.f2py, used while in transition
from egsnrc2py import egsfortran
from egsnrc2py.egs_home.tutor4 import tutor4

HERE  = Path(__file__).resolve().parent
TEST_DATA = HERE / "data"
TUTOR4_PATH = HERE.parent / "egs_home" / "tutor4" / "tutor4.py"

# @pytest.mark.skipif(sys.platform=="win32")

def known_in_out(filepath, in_types, out_type):
    """Iterator over a filename, yielding known inputs and result"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    gen = iter(lines)
    for line in gen:
        if not line.startswith("in "):
            continue
        inputs = line[3:].split()
        inputs = [typ(x.strip()) for x, typ in zip(inputs, in_types)]
        out = next(gen)
        if not out.startswith("out "):
            raise ValueError("'out' line must follow 'in' line")
        output_val = out_type(out[4:].strip())

        yield inputs, output_val


class TestTutor4:
    def test_output(self, capfd):
        """Test that (partially) Python tutor4 produces known output"""
        
        # Ensure proper random initial state
        # (other tests use ranlux)
        egsfortran.init_ranlux(1,0)
        tutor4.main()
        captured = capfd.readouterr()

        # Test last line of history 10
        lines = captured.out.splitlines()
        rev_lines_iter = reversed(lines)
        while not next(rev_lines_iter).startswith(" END OF HISTORY      10"):
            pass
        last_line_hist10 = next(rev_lines_iter)

        # Could test more, but testing last couple of lines of last history
        # should pretty much guarantee had exact 'trajectory' through 
        # random numbers and all physics as original EGSnrc mortran/fortran
        # (within the significant figures shown)
        assert (
            "Discard -user request             :    1   "
            "16.496  -1   3  -0.004  -0.001   0.100 -0.280"
            "  0.259  0.924         0 1.000E+00"
        ) in last_line_hist10
        # Ignore test outputs we might be generating
        while (secondlast := next(rev_lines_iter)).startswith(("in", "out", "fn:")):
            pass
        expected_2nd_last_hist10 = (
            "17.150  -1   2   0.006  -0.005   0.063 -0.240 -0.009  0.971" 
        )
        assert expected_2nd_last_hist10 in secondlast
        
    def test_compute_drange(self):
        "Calculate correct values for $COMPUTE-DRANGE in Python"
        # Compare against ones captured from TUTOR4 run with extra prints
        tutor4.init()  # get all data loaded
        # Known inputs for compute-drange from Mortran tutor4 run
        for inputs, expected in known_in_out(TEST_DATA / "compute-drange.txt",
            (int, int, float, float, int, float, float), float
        ):
            # compute_drange(lelec, medium, eke1, eke2, lelke1, elke1, elke2)
            got = tutor4.compute_drange(*inputs)
            assert got == pytest.approx(expected,abs=0.0000001)

    def test_calc_tstep(self):
        "Calc correct values for modified $CALCULATE-TSTEP-FROM-DEMFP in Python"
        # Compare against ones captured from TUTOR4 run with extra prints
        tutor4.init()  # get all data loaded
        # Known inputs for compute-drange from Mortran tutor4 run
        for inputs, expected in known_in_out(TEST_DATA / "calc-tstep.txt",
            (int, int, int, int, float, float, float, float, float), float
        ):
            # 
            print("in ", ",".join(str(x) for x in inputs))
            got = tutor4.calc_tstep_from_demfp(*inputs)
            
            assert got == pytest.approx(expected,abs=0.0000001)