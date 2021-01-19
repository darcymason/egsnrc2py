from textwrap import dedent

from egsnrc2py.macros import re_from_to, func_details
from egsnrc2py.macros import (
    _parse_and_apply_macros, macros, parameters, empty_callbacks
)

recurse_macro1 = dedent(
    """
    REPLACE {$electron_region_change;} WITH {
        ir(np) = irnew; irl = irnew; medium = med(irl);
    };
    REPLACE {$photon_region_change;} WITH { $electron_region_change; }
    """
)

recurse_code1 = "$photon_region_change;"

class TestMacroReplace:
    def test_recursive_replace(self):
        """Macro inside a macro is replaced"""
        macros.clear()
        parameters.clear()
        code = _parse_and_apply_macros(recurse_macro1 + recurse_code1)
        lines = [line for line in code.splitlines() if line.strip().replace(";", "")]
        assert "Inline replace: $ photon_region_change" in lines[0]
        assert "if electron_region_change:" in lines[5]
        assert lines[3].strip() == "else:"

    def test_blank_inline_replace(self):
        macros.clear()
        parameters.clear()
        empty_callbacks.clear()
        macro_defn = "REPLACE {$CALL_USER_ELECTRON} WITH {;}\n"
        code = "$CALL_USER_ELECTRON"
        code = _parse_and_apply_macros(macro_defn + code)
        lines = [line for line in code.splitlines() if line.strip().replace(";", "")]
        assert 4 == len(lines)
        assert "if call_user_electron:" in lines[1]  # after comment line
        assert "call_user_electron()" == lines[2].strip()

    def test_wait_replace(self):
        """Macro with REPLACE in its expansion is handled as expected"""
        m_from = "S1TRACE#;"
        m_to = (
            "REPLACE {;{P1}({WAIT {ARB}})={WAIT {ARB}};} WITH "
            "{{EMIT ;{P1}({WAIT {P1}})}={WAIT {P2}};}"
        )
        expected = (
            r"REPLACE {;\g<1>(#)=#;} WITH "
            r"{{EMIT ;\g<1>({P1})}={P2};}"

        )
        re_from, re_to = re_from_to(m_from, m_to)
        assert expected == re_to

    def test_func_details(self):
        name = "compute_drange"  # has a function definition
        args = "eke,ekei,lelke,elke,elkei,tuss".split(",")
        func_args, return_vars = func_details(name, args)
        expect_args = "eke1,eke2,lelke1,elke1,elke2".split(",")
        assert expect_args == func_args
        assert ["tuss"] == return_vars


    def test_recursive2(self):
        macros_code = dedent("""REPLACE {PARAMETER #=#;} WITH
            { REPLACE {{P1}} WITH {{P2}}}

            PARAMETER $MXXXX=YYY;     "GAMMA SMALL ENERGY INTERVALS"
            x = $MXXXX;

            PARAMETER $MXXXX=ZZZ;
            y = $MXXXX;
            z = $MXSGE;
            """
        )
        macros.clear()
        parameters.clear()
        got = _parse_and_apply_macros(macros_code).splitlines()
        # Check some lines - lots of whitespace left behind after macros removed
        assert '"GAMMA SMALL ENERGY INTERVALS"' in got[2]
        assert 'x = YYY;' in got[3]
        assert 'y = ZZZ;' in got[6]
        assert 'z = $MXSGE;' in got[7]
        assert 2 == len(macros)
        assert 'PARAMETER #=#;' in macros
        assert '$MXXXX' in macros