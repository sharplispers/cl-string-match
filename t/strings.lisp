;;; Test the ASCII strings implementation

;; --------------------------------------------------------
;; some tests
;; --------------------------------------------------------

(in-package :cl-string-match-test)

;; --------------------------------------------------------

(define-test test-ub-read-line
  (:tag :contrib :ascii-strings)
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
  (:tag :contrib :ascii-strings)
  (with-open-file (in "test.txt"
		      :direction :input
		      :element-type 'ascii:ub-char)
    (let ((reader (ascii:make-ub-line-reader :stream in)))
      (assert-equal 5
		    (loop
		       :for i :from 0 :below 10
		       :for line = (ascii:ub-read-line-string reader)
		       :while line
		       :do (format t "read-line-string [~a]: ~a~%" i line)
		       :count line))
      ;; check if putting the reader into the start will yield the
      ;; same results as if it was just newly created
      (ascii:ub-line-reader-file-position reader 0)
      (assert-equal 5
		    (loop
		       :for i :from 0 :below 10
		       :for line = (ascii:ub-read-line-string reader)
		       :while line
		       :do (format t "read-line-string.2 [~a]: ~a~%" i line)
		       :count line)))))

;; --------------------------------------------------------

(defun test-ub-count-lines (fname)
  (:tag :contrib :ascii-strings)
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
  (:tag :contrib :ascii-strings)
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
  (:tag :contrib :ascii-strings)
  (assert-false
   (loop :for i :from 0 :below 256
      :unless (= (char-code (code-char i)) i)
      :return T)))

;; --------------------------------------------------------

(define-test test-char-substitutions
  "Test if ub-to-string properly replaces bad characters."
  (:tag :contrib :ascii-strings)
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

(define-test test-urandom-strings-reader
  "Test reading random binary data."
  (:tag :contrib :ascii-strings)
  (let ((times 10000))
    (with-open-file (in "/dev/urandom"
			:direction :input
			:element-type 'ascii:ub-char)
      (assert-equal times
		    (loop :with reader = (ascii:make-ub-line-reader :stream in)
		       :for i :from 0 :below times
		       :for line = (ascii:ub-read-line reader)
		       :while line
		       :count line)))))

;; --------------------------------------------------------

(define-test test-simple-strings
  "Test simple string operations"
  (:tag :contrib :ascii-strings)
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
			   (ascii:string-to-ub the-string-1))))

    ;; from the CLHS examples section
    (assert-true
     (ascii:ub-string= (ascii:string-to-ub "foo")
		       (ascii:string-to-ub "foo")))
    (assert-false
     (ascii:ub-string= (ascii:string-to-ub "foo")
		       (ascii:string-to-ub "Foo")))
    (assert-false
     (ascii:ub-string= (ascii:string-to-ub "foo")
		       (ascii:string-to-ub "bar")))
    (assert-true
     (ascii:ub-string= (ascii:string-to-ub "together")
		       (ascii:string-to-ub "frog")
		       :start1 1 :end1 3 :start2 2))
    ;; todo: (string-equal "foo" "Foo") =>  true
    (assert-true
     (ascii:ub-string= (ascii:string-to-ub "abcd")
		       (ascii:string-to-ub "01234abcd9012")
		       :start2 5 :end2 9))
    (assert-equal
     3
     (ascii:ub-string< (ascii:string-to-ub "aaaa")
		       (ascii:string-to-ub "aaab")))
    (assert-equal
     4
     (ascii:ub-string>= (ascii:string-to-ub "aaaaa")
			(ascii:string-to-ub "aaaa")))

    ;; todo: (string-not-greaterp "Abcde" "abcdE") =>  5
    ;; todo: (string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7 :start2 2 :end2 6) =>  6
    ;; todo: (string-not-equal "AAAA" "aaaA") =>  false
    ))

;; --------------------------------------------------------

(defun run-strings ()
  (run-tags '(:ascii-strings) :cl-string-match-test))

;; EOF
