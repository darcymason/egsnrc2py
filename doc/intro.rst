.. _intro:

intro.rst

Transpiling EGSnrc from Mortran to Python
=========================================

Concepts
--------

This library, ``egsnrc2py``, is a *transpiler* (source code converted
to source code) for helping convert EGSnrc Mortran code to Python.

The process is complex and needs
to be run multiple times with adjustments to keep improving the process and
automating as much as possible.

After the automated process, there will still need to be quite a few hand-edits
to make the Python code runnable.

The transpile process may be run again after changes to the EGS Mortran
source code, although the hand-edits would have to be reapplied, likely with
the help of `diff`s of hand-edited vs newly auto-generated.

Package Organization
---------------------

EGSnrc original source code is in ``HEN_HOUSE/src``, following the structure in the
`EGSnrc repo <https://github.com/nrc-cnrc/EGSnrc>`_.  The code will be brought
in as needed, and divided into pieces during development (e.g. first transpile
efforts were for ``subroutine ELECTR``, excerpted from ``egsnrc.mortran`` and stored
in ``electr.mortran``)

Autotranspile output goes into the `autotranspile` folder.

Hand-edits (final versions) are in folder ``egsnrc_py``.  This is the EGSnrc
code in Python format. The folder is set up as a Python project structure --
ultimately if all this works, that would be copied to its own repo for
ongoing work.

