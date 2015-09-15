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
		(snf:scanf "ab%c%d" "abc+123efg"))
  (assert-equal '(123 83)
		(snf:scanf "ab%d %o" "ab123 1239fg"))
  (assert-equal '("123")
		(snf:scanf "%s" "123 ")))

;; --------------------------------------------------------

(define-test test-scanf-numbers
  (:tag :contrib :scanf)
  (assert-equal '(123)
		(snf:scanf " %d " "123"))
  (assert-equal '(123)
		(snf:scanf " %3d " "12345"))
  (assert-equal '(10)
		(snf:scanf " %i " "0xAmmm"))
  (assert-equal '(1)
		(snf:scanf " %i " "0199")))

;; --------------------------------------------------------

(define-test test-scanf-whitespace
  (:tag :contrib :scanf)
  (assert-equal '(#\c #\d)
		(snf:scanf "ab%c  %c" "abc  d"))
  (assert-equal '(#\c #\d)
		(snf:scanf "ab%c  %c" "abcd")))

;; --------------------------------------------------------

(define-test test-scanf-flags
  (:tag :contrib :scanf)
  (assert-equal '(123)
		(snf:scanf "ab%*c%d" "abc123"))
  (assert-equal '(#\c)
		(snf:scanf "ab%c%*d" "abc123"))
  (assert-false (snf:scanf "ab%*c %*d"
			   "abc   123"))
  (assert-equal '(123)
		(snf:scanf "ab%*c%3d" "abc1234"))
  (assert-equal '(#\c (#\1 #\2 #\3))
		(snf:scanf "ab%c%3c" "abc1234"))
  (assert-equal '("123")
		(snf:scanf "  %3s "
			   "  12345 ")))

;; --------------------------------------------------------

(defun run-scanf ()
  (run-tags '(:scanf) :cl-string-match-test))

;; EOF
