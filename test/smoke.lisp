
;; sbcl --load smoke.lisp

(ql:quickload :cl-string-match)
(ql:quickload :lisp-unit)

(in-package :cl-string-match)
(use-package :lisp-unit)

(define-test basic-test
    (assert-equal 1 (string-contains-brute "a" "-a-")))

(run-tests :all)

(cl:quit)

;; EOF