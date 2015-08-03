#!/bin/sh

sbcl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(test:run)' --quit
lx86cl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(test:run)' --eval '(quit)'
echo '(ql:quickload :CL-STRING-MATCH-TEST) (test:run) (quit)' | abcl
ecl -eval '(ql:quickload "CL-STRING-MATCH-TEST")' -eval '(test:run)' -eval '(quit)'

