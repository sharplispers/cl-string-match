#!/bin/sh

echo ============ SBCL ===============
sbcl --eval '(push :sm-debug-enabled *features*)' --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --quit
echo ============ Clozure CL ===============
lx86cl --eval '(push :sm-debug-enabled *features*)' --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(sm-test:run)' --eval '(quit)'
echo ============ Armed Bear CL ===============
echo '(push :sm-debug-enabled *features*) (ql:quickload :CL-STRING-MATCH-TEST) (sm-test:run) (quit)' | abcl
echo ============ Embeddable CL ===============
ecl -q -eval '(push :sm-debug-enabled *features*)' -eval '(ql:quickload "CL-STRING-MATCH-TEST")' -eval '(sm-test:run)' -eval '(quit)' | grep -v ';;;'

