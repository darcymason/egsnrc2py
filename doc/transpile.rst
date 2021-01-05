.. _transpile:

Transpiling Details
===================



The steps for auto-transpiling are:


Macros
------

* collect all macros
* fix identifiers that are not legal in Python
* determine which are simple numbers, which are callables, and which are for common blocks
* For simple numbers:

* make the replacement value the same name but with the $ sign removed
* add the assignment of ``identifier = value`` to the Python ``parameters`` module
* replace $<name> with <name> in all other macros that use identifier

* for callables/code:

* if blank definition, substute ``if (<name-exists>): call <name>``
* if non-blank

    * if no extra macro logic e.g. ``[if]`` blocks, create Mortran subroutine with
    that name and add the replacement code to the transpile-source (in memory)
    (Note this is needed when unwind IF blocks etc, later)
    * if extra macro logic, then

    * in the tranpile-source, find all matching to the macro
    * for each, add to ``needs_mortran`` file
    * run Mortran conversion on that file, read the results, and map
        the macro match to the results from the Mortran replacement

* for common blocks

* (undecided) - may just hand-produce each of these with a mapping file
* (OR) - parse the macro to produce a file with a Python ``dict`` mapping them

Code logic
----------

* need a mapping of different subroutines and which args are input/output/both
(could write code to determine which args are assigned inside the code)
* ``goto``s - may need to map the locations.  At the very least, they should beginning
replaced with a boolean flag and a ``break`` statement (assuming inside a loop
or IF branch), that can then be used to control the program flow later with
hand-editing

Code structure
---------------

* for each SUBROUTINE, find beginning and end
* with that code, 'regularize' and IF and loop branching, etc.

* replace all e.g. ``IF [block]`` with ``IF\n [\n [indent]\n]`` statements. Note the
  statements that are separated by ";" need to each have their own line.
  (Note this must come after macro replacements above so that substituted
  code is also regularized). This is a recursive operation to get indents
  with indents correct.
* replace the subroutine in transpile-output, with Python def and return value
  (using the outputs by subroutine info)
* indent the entire subroutine.

Testing
-------

* test should be put in place on fully transpiled code (runnable after hand-edits),
with mock functions for random numbers, ``howfar``, etc.