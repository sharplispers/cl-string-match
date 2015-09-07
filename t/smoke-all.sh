#!/bin/sh

sbcl --eval '(push :sm-debug-enabled *features*)' --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --quit
lx86cl --eval '(push :sm-debug-enabled *features*)' --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --eval '(quit)'
echo '(push :sm-debug-enabled *features*) (ql:quickload :CL-STRING-MATCH-TEST) (sm-test:run) (quit)' | abcl
ecl -eval '(push :sm-debug-enabled *features*)' -eval '(ql:quickload "CL-STRING-MATCH-TEST")' -eval '(sm-test:run)' -eval '(quit)'

