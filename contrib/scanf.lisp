;;; A trivial scanf implementation in Lisp

;; There are two ways to implement the scanf function:
;;
;;  1. Compile the format string into machine code and run this
;;     program on the input string(s)
;;
;;  2. Interpret directives from the format string on the fly
;;
;; The code in this file follows the 2nd way.

;; As a source of ideas and documentation following resources were
;; used:

;; FreeBSD scanf implementation can be viewed here:
;; https://github.com/freebsd/freebsd/blob/master/lib/libc/stdio/vfscanf.c
;; It goes char-after-char in the given format and dispatches current
;; action depending on the character read
;;
;; Man page is located here:
;; http://www.freebsd.org/cgi/man.cgi?query=scanf&sektion=3


(defpackage :trivial-scanf
  (:use :common-lisp :alexandria :iterate)
  (:nicknames :sf)
  (:documentation "A trivial scanf implementation in Common Lisp.")

  (:export
   :scanf))


(in-package :trivial-scanf)

;; --------------------------------------------------------

(defun scanf (fmt str &key (start 0))
  "Parse the given string according to the fmt

Following directives are recognized:

  d - decimal number

  f - floating point number

  c - character
"

  (let ((fmt-pos 0)
	(fmt-len (length fmt))
	(str-pos start)
	(results '()))
    (iter
     ;; todo: what if the string is too short?
     (for c = (char fmt fmt-pos))
     (while (< fmt-pos fmt-len))
     (case c
       ;; directive
       (#\%
	;; todo: handle more directives
	(incf fmt-pos)
	(case (char fmt fmt-pos)
	  (#\d
	   ;; read and store a decimal integer
	   (multiple-value-bind (int end-pos)
	       (parse-integer str :start str-pos :junk-allowed T)
	     (push int results)
	     (setf str-pos end-pos)))
	  (#\f
	   (error "not yet implemented"))
	  (#\c
	   ;; read and store a single character
	   (push (char str str-pos) results)
	   (incf str-pos))
	  (#\%
	   ;; handle '%' similar to an ordinary character,
	   (unless (char= #\% (char str str-pos))
	     (error "mismatch"))
	   (incf str-pos))
	  (otherwise
	   (error "invalid directive")))
	;; advance format and reduce its length because we've consumed
	;; a directive char
	(incf fmt-pos)
	(decf fmt-len))

       ;; an ordinary character
       (otherwise
	(unless (char= (char str str-pos)
		       (char fmt fmt-pos))
	  (error "mismatch"))
	(incf str-pos)
	(incf fmt-pos))))
    results))

;; EOF
