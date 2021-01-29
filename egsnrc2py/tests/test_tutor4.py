import pytest
from pathlib import Path

pytest.importorskip("egsnrc2py.egsfortran")  # from numpy.f2py, used while in transition
from egsnrc2py import egsfortran
from egsnrc2py.egs_home.tutor4 import tutor4

HERE  = Path(__file__).resolve().parent
TUTOR4_PATH = HERE.parent / "egs_home" / "tutor4" / "tutor4.py"

# @pytest.mark.skipif(sys.platform=="win32")
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
        expected_2nd_last_hist10 = (
            "17.150  -1   2   0.006  -0.005   0.063 -0.240 -0.009  0.971" 
        )
        assert expected_2nd_last_hist10 in next(rev_lines_iter)
        