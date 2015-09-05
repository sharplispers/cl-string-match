;;; Test the ASCII strings implementation

;; --------------------------------------------------------
;; some tests
;; --------------------------------------------------------

(in-package :cl-string-match-test)

;; --------------------------------------------------------

(define-test test-ub-read-line
  (with-open-file (in "test.txt"
       :direction :input
       :element-type 'ascii:ub-char)
   (assert-equal 5
		 (loop :with reader = (ascii:make-ub-line-reader :stream in)
		       :for i :from 0 :below 10
		       :for line = (ascii:ub-read-line reader)
		       :while line
		       :do (format t "read-line [~a]: ~a~%" i (ascii:ub-to-string line))
		       :count line))))

;; --------------------------------------------------------

(define-test test-ub-read-line-string
    (with-open-file (in "test.txt"
			:direction :input
			:element-type 'ascii:ub-char)
      (assert-equal 5
		    (loop :with reader = (ascii:make-ub-line-reader :stream in)
		       :for i :from 0 :below 10
		       :for line = (ascii:ub-read-line-string reader)
		       :while line
		       :do (format t "read-line-string [~a]: ~a~%" i line)
		       :count line))))

;; --------------------------------------------------------

(defun test-ub-count-lines (fname)
  (with-open-file (in fname
                      :direction :input
                      :element-type 'ascii:ub-char)
    (loop :with reader = (ascii:make-ub-line-reader :stream in)
       :for i :from 0
       :for line = (ascii:ub-read-line reader)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))

;; --------------------------------------------------------

(defun test-count-lines (fname)
  (with-open-file (in fname
                      :direction :input)
    (loop
       :for i :from 0
       :for line = (read-line in nil)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))

;; --------------------------------------------------------

(define-test test-simple-chars
  "Test if it is possible to encode/decode characters with codes
within range 0..255 using standard Lisp functions CODE-CHAR and
CHAR-CODE.

See ticket #30"
  (assert-false
   (loop :for i :from 0 :below 256
	 :unless (= (char-code (code-char i)) i)
	 :return T)))

;; --------------------------------------------------------

(define-test test-char-substitutions
    "Test if ub-to-string properly replaces bad characters."
  (let* ((str-1-ub (ascii:string-to-ub "abaca"))
	 (st (ascii:make-substitution-table '((#.(char-code #\a)
					       #.(char-code #\u)))))
	 (str-1-def "ubucu")
	 (str-2-str (ascii:ub-to-string
		     (ascii:octets-to-ub #(0 #.(char-code #\a)
					   0 #.(char-code #\a) 0))))
	 (str-2-def "?a?a?"))

    (assert-equal
     str-1-def
     (ascii:ub-to-string str-1-ub :subst st))

    (assert-true
     (ascii:ub-string=
      (ascii:string-to-ub str-2-str)
      (ascii:string-to-ub str-2-def)))))

;; --------------------------------------------------------

(define-test test-simple-strings
    "Test simple string operations"
  (let* ((the-string-1 "abcdefgh123")
	 (string1 (ascii:string-to-ub the-string-1))
         ;; (string2 (ascii:string-to-ub "ABCDEFGH123"))
         (string1.1 (ascii:ub-subseq string1 1))
         (string1.1.1 (ascii:ub-subseq string1.1 1)))

    (assert-true (= (ascii:ub-char string1 1)
		    (ascii:ub-char string1.1 0)))

    (assert-true (= (ascii:ub-char string1 2)
		    (ascii:ub-char string1.1.1 0)))

    (assert-true (= (ascii:ub-char string1.1 1)
		    (ascii:ub-char string1.1.1 0)))

    (assert-true (= (char-code #\c)
		    (ascii:ub-char string1.1.1 0)))

    (assert-true (string= the-string-1
			  (ascii:ub-to-string
			   (ascii:string-to-ub the-string-1))))))


;; EOF
