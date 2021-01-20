from textwrap import dedent

from egsnrc2py.macros import re_from_to, func_details
from egsnrc2py.macros import (
    _parse_and_apply_macros, macros, parameters, empty_callbacks, init_macros,
    apply_macros
)
from egsnrc2py.util import fix_identifiers

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
    def setup(self):
        init_macros()

    def test_recursive_replace(self):
        """Macro inside a macro is replaced"""
        code = _parse_and_apply_macros(recurse_macro1 + recurse_code1)
        lines = [line for line in code.splitlines() if line.strip().replace(";", "")]
        assert "Inline replace: $ photon_region_change" in lines[0]
        assert "if electron_region_change:" in lines[5]
        assert lines[3].strip() == "else:"

    def test_blank_inline_replace(self):
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
        expected = (  # Note, indent is always \g<1>
            r"REPLACE {;\g<2>(#)=#;} WITH "
            r"{{EMIT ;\g<2>({P1})}={P2};}"

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
        got = _parse_and_apply_macros(macros_code).splitlines()
        # Check some lines - lots of whitespace left behind after macros removed
        assert '"GAMMA SMALL ENERGY INTERVALS"' in got[2]
        assert 'x = YYY;' in got[3]
        assert 'y = ZZZ;' in got[6]
        assert 'z = $MXSGE;' in got[7]
        assert 2 + 1 == len(macros)  # +1 for added macro in init_macros
        assert 'PARAMETER #=#;' in macros
        assert '$MXXXX' in macros

    def test_param_replace(self):
        macros_code = dedent(
            """REPLACE {PARAMETER #=#;} WITH
            { REPLACE {{P1}} WITH {{P2}}}

            PARAMETER $MXMED=10;      "MAX. NO. OF DIFFERENT MEDIA (EXCL. VAC.)"
            x = $MXMED;
            """
        )
        code = _parse_and_apply_macros(macros_code)
        assert "x = MXMED;" in code

    def test_replace_comment_macros(self):
        code = '"Default FOR $SELECT-ELECTRON-MFP; is: $RANDOMSET rnne1;'
        code = fix_identifiers(code)
        assert '$ SELECT' in code
        assert '$ RANDOMSET' in code

    def test_replace_set_interval(self):
        code = 'elkems = Log(ekems);\n   $SET INTERVAL elkems,eke;'
        # SET INTERVAL added in `init_macros`
        init_macros()
        code = _parse_and_apply_macros(code)
        assert "   # Unhandled macro '$ SET INTERVAL elkems,eke;" in code.splitlines()[1]

    def test_randomset_call(self):
        macros_code = """REPLACE {$RANDOMSET#;} WITH
            {;
            IF ( rng_seed > 24 ) [
                call ranlux(rng_array); rng_seed = 1;
            ]
            {P1} = rng_array(rng_seed);
            rng_seed = rng_seed + 1;
            ;
            }
        """
        code = "$RANDOMSET rnn1;"
        macros_code, code = apply_macros(macros_code, code)
        assert 'rnn1 = randomset()' in code