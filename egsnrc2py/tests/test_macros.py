from textwrap import dedent

from egsnrc2py.macros import MacrosAndCode

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
        macros = MacrosAndCode(recurse_macro1, recurse_code1, recurse=True)
        lines = macros.source_code.splitlines()
        assert "Inline replace: $photon_region_change;" in lines[0]
        assert lines[3] == "else:"
        assert "Inline replace: $electron_region_change;" in lines[4]