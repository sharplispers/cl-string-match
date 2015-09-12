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
	(str-pos start))
    (iter
      (while (< fmt-pos fmt-len))
      ;; todo: what if the string is too short?
      (for c = (char fmt fmt-pos))
      (case c
	(#\%				; a directive
	 ;; todo: handle directives
	 )
	(otherwise			; an ordinary character
	 (unless (char= (char str str-pos)
			(char fmt fmt-pos))
	   (error "mismatch"))
	 (incf str-pos)
	 (incf fmt-pos))))))

;; EOF
