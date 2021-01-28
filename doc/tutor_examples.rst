.. _tutor:

Tutor Examples
==============

The tutor4 example from EGSnrc is being used as a testbed for 
checking ongoing switching of code to Python.  The Numpy ``f2py``
module is used to make the original compiled Fortran code (for tutor4)
available to be called from Python (in shared lib ``egsfortran``).

Global variables were defined in Python to match the COMMON blocks
in Fortran, to allow both Python and Fortran to share the same variables.

Initially, ``tutor4.mortran`` and the ``shower`` main loop were translated
to Python code.  Going forward, more pieces of the original Fortran are
being replaced.

So far this is working only for linux; f2py seems to be more difficult
to get working properly under Windows.


Pre-reqs
--------

* Python 3.9 available and callable with ``python3.9``
* EGSnrc config set up for ``linux``
* EGS_HOME set to ``.../egsnrc2py/egsnrc2py/egs_home``


Steps
-----

* in ``egsnrc2py/egsnrc2py/egs_home/tutor4`` folder:

* ``make``
* ``tutor4 -p tutor_data`` to launch the standard EGSnrc mortran/Fortran run
* ``./make_f2py``.  This creates the ``egsfortran...so`` file and copies it to the
  main ``egsnrc2py`` folder so ``egsfortran`` is importable as part of the package.
* ``python3.9 tutor4.py`` to run the Python version

``tutor4`` outputs details of the particles and interactions at each step.  This
provides a validation baseline as new Python code is introduced.


Python / egsfortran Tips
------------------------

All variables have been converted to lower case in Python.  Fortran ignores
case in variable names, so care must be taken not to leave any in 
uppercase or mixed-case, which Python will treat as a distinct variable,
leading to subtle bugs.

Fortran has 1-based array indexing by default while
Python is zero-based, so care must be taken to ensure indices 
are modified (usually subtract 1) in Python.  Note, however, that
some EGSnrc arrays override the default and specify a zero-based array.

All globals used in the program must be defined also in Python.  If not, 
then setting or reading values will actually be affecting a local
Python variable, and this can be very difficult to debug.  In some cases
this could mean that a Fortran variable has kept its default value (often 0)
rather than what you are trying to set.

Assigning immutables to a global name must use the common block name 
and attribute, e.g. ``stack.np = 1``, otherwise Python will re-bind
the name to a non-egsfortran Python local/global.
This is not specific to this code, but normal Python behavior for
binding of variable names.

Setting elements of arrays can be done without the common block reference,
as this changes the internal state of the array, not where
the variable name is pointing. E.g. ``iq[0] = 1`` is okay without the
common block reference.  *Using* variable values, as part of a right-hand-side
expression, can always be done without concern.
