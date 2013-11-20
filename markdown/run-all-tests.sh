# Tests are in a variety of named submodules:
#
# 'test'          : Normal.
# 'slow-test'     : Quite slow to run.
# 'disabled-test' : Disabled (for the time being).
#
# This shell script runs all of them.
raco test -x -s test -s slow-test -s disabled-test .
