printf - a common lisp library
------------------------------

This project began as a simple exercise to try out the smug monadic
parser generator.  As such things do, it got totally out of control.
This version of printf is mostly based off the man page for the
version of printf() in GNU libc.  Not everything is implemented, but
there's enough here for a reasonable stab.

Implemented functions
---------------------

printf:
   Writes formatted output to \*standard-output\*.  The positional
   arguments are passed as parameters to the function.

vprintf:
   Writes formatted output to \*standard-output\*.  The positional
   arguments are passed as a list.

sprintf:
   Returns a string containing the results of the formatted output.
   The positional arguments are passed as parameters to the function.

vsprintf:
   Returns a string containing the results of the formatted output.
   The positional arguments are passed as a list.

fprintf:
   Writes formatted output to STREAM.  The positional arguments are
   passed as parameters to the function.

fvprintf:
   Writes formatted output to STREAM.  The positional arguments are
   passed as parameters as a list.
