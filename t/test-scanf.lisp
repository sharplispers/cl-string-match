;; --------------------------------------------------------
;; Unit tests for the trivial-scanf implementation
;; --------------------------------------------------------

(in-package :cl-string-match-test)

;; --------------------------------------------------------

(define-test test-scanf-simple
  (:tag :contrib :scanf)
  (assert-false (snf:scanf "abc" "abc"))
  (assert-equal '(#\c)
		(snf:scanf "ab%c" "abc"))
  (assert-equal '(#\c 123)
		(snf:scanf "ab%c%d" "abc123"))
  (assert-equal '(#\c -123)
		(snf:scanf "ab%c%d" "abc-123efg"))
  (assert-equal '(#\c 123)
		(snf:scanf "ab%c%d" "abc+123efg")))

;; --------------------------------------------------------

(define-test test-scanf-whitespace
  (:tag :contrib :scanf)
  (assert-equal '(#\c #\d)
		(snf:scanf "ab%c  %c" "abc  d"))
  (assert-equal '(#\c #\d)
		(snf:scanf "ab%c  %c" "abcd")))

;; --------------------------------------------------------

(defun run-scanf ()
  (run-tags '(:scanf) :cl-string-match-test))

;; EOF
