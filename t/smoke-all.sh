#!/bin/sh

sbcl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --quit
lx86cl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --eval '(quit)'
echo '(ql:quickload :CL-STRING-MATCH-TEST) (sm-test:run) (quit)' | abcl
ecl -eval '(ql:quickload "CL-STRING-MATCH-TEST")' -eval '(sm-test:run)' -eval '(quit)'

