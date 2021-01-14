Running Tutor1 - original and Python start
===============

Background
----------
In the course of converting EGSnrc code to Python, want to be able to test the core
physics routines by patching in other code via numpy.f2py.

`tutor1` folder was copied EGSnrc user codes folder to this repo.

Build the example as usual
--------------------------
```
SET EGS_HOME=(<path to egsnrc2py>/egsnrc2py/egs_home/) # note trailing '/' required
make
tutor1 -p tutor_data
```

... and should see the output from an actual (small) EGSnrc run.

Python
-------

Initially, the intent was just to establish the ability to call Fortran code and
get access to arrays, etc.  I was able to generate this in linux (but 
not on Windows, library complains of missing DLLs on loading, that have not yet
been solved)

To get a Python importable routine, edit the tutor1_linux.f (name may be different) produced from the Mortran compile, and wrap the initial "program" part in a SUBROUTINE by just adding a single line, e.g.

```
SUBROUTINE tutor1()
```

to the top of the file.  The END statement is already there before the next subroutine.

Then run

```
python3 -m numpy.f2py -c tutor1_linux.f -m tut1
```

There will be a stream of warnings, but hopefully at the end all has completed 
succesfully and the last statement is about removing a temporary build directory.

Then the Python code tutor1.py can be run:

```
python3 tutor1.py
```

It sets some variables normally picked up from the command line in EGSnrc
runs, and then calls hatch() to read the physics data into Fortran common
block variables.  More will be added after some further conversion of 
EGSnrc code...
