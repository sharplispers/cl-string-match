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
  (assert-equal '(123 123 -123)
		(snf:scanf " %d %d %d" "123+123-123"))
  (assert-equal '(123)
		(snf:scanf " %3d " "12345"))
  (assert-equal '(10)
		(snf:scanf " %i " "0xAmmm"))
  (assert-equal '(1)
		(snf:scanf " %i " "0199"))

  (assert-float-equal '(999.0)
		(snf:scanf " %f " "0.999e+3"))
  (assert-float-equal '(999.0)
		(snf:scanf " %f " "  999.0 "))
  (assert-float-equal '(999.0)
		(snf:scanf " %f " "  999 ")))

;; --------------------------------------------------------

(define-test test-scanf-chars-set
  (:tag :contrib :scanf)
  (assert-equal '("abc")
		(snf:scanf "%[cba]" "abc"))
  (assert-equal '("adf")
		(snf:scanf "%[abc-f]" "adf_"))
  (assert-equal '("xyz")
		(snf:scanf "%[^abc-]" "xyz-e"))
  (assert-equal '("xyz" #\e)
		(snf:scanf " %[^]abc-]%*c%c" "xyz]e")))

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

(define-test test-scanf-mismatch
  ;; Test how scanf function behaves with invalid input string
  (:tag :contrib :scanf)
  (assert-false (snf:scanf "abd" "abc"))
  
  (assert-false (snf:scanf "ab%d" "abc"))
  (assert-equal '(#\c)
		(snf:scanf "ab%c%d" "abcd"))
  (assert-equal '(#\c)
		(snf:scanf "ab%c%f" "abcd")))

;; --------------------------------------------------------

(defun run-scanf ()
  (run-tags '(:scanf) :cl-string-match-test))

;; EOF
