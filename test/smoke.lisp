
;; sbcl --load smoke.lisp --quit

(ql:quickload :cl-string-match)
(ql:quickload :lisp-unit)

(in-package :cl-string-match)
(use-package :lisp-unit)

(setq *print-failures* t)

(defparameter *funcs*
  '(string-contains-brute
    string-contains-bm))

(defmacro run-assertions (val needle haystack)
  (loop :for func :in *funcs*
     :collect `(assert-equal ,val (,func ,needle ,haystack))))

(define-test basic-test
    (run-assertions 0 "a" "a--")
    (run-assertions 1 "a" "-a-")
  (run-assertions 2 "a" "--a"))

(run-tests :all)

;; EOF