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
;;
;; This Common Lisp implementation is influenced and uses some
;; documentation from the FreeBSD scanf that is:
;;
;; * Copyright (c) 1990, 1993
;; *	The Regents of the University of California.  All rights reserved.
;; *
;; * Copyright (c) 2011 The FreeBSD Foundation
;; * All rights reserved.
;; * Portions of this software were developed by David Chisnall
;; * under sponsorship from the FreeBSD Foundation.


(defpackage :trivial-scanf
  (:use :common-lisp :alexandria :iterate :proc-parse)
  (:nicknames :snf)
  (:documentation "A trivial scanf implementation in Common Lisp.")

  (:export
   :scanf))

;; --------------------------------------------------------

(in-package :trivial-scanf)

#+ignore
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim
   #-sm-debug-enabled
   (optimize (speed 3)
	     (safety 0))
   #+sm-debug-enabled
   (optimize safety debug)))

;; --------------------------------------------------------

(defun fscanf (fmt stream &key (start 0)))

(defun scanf (fmt str &key (start 0) end)
  "Parse the given string according to the fmt

Each returned value corresponds properly with each successive
conversion specifier (but see the * conversion below). All conversions
are introduced by the % (percent sign) character.

White space (such as blanks, tabs, or newlines) in the format string
match any amount of white space, including none, in the
input. Everything else matches only itself. Scanning stops when an
input character does not match such a format character. Scanning also
stops when an input conversion cannot be made (see below).

Following the % character introducing a conversion there may be a
number of flag characters, as follows:

 * Suppresses assignment.  The conversion that follows occurs as
   usual, but no pointer is used; the result of the conversion is
   simply discarded.

In addition to this flag, there may be an optional maximum field
width, expressed as a decimal integer, between the % and the
conversion. If no width is given, a default of ``infinity'' is
used (with one exception, below); otherwise at most this many bytes
are scanned in processing the conversion.

The following conversions are available:

 % Matches a literal `%'.  That is, ``%%'' in the format string
   matches a single input `%' character.  No conversion is done, and
   assignment does not occur.

 d Matches an optionally signed decimal integer.

 i Matches an optionally signed integer.  The integer is read in base
   16 if it begins with `0x' or `0X', in base 8 if it begins with `0',
   and in base 10 otherwise.  Only characters that correspond to the
   base are used.

 o Matches an octal integer.

 u Matches an optionally signed decimal integer.

 x, X  Matches an optionally signed hexadecimal integer.

 a, A, e, E, f, F, g, G
   Matches a floating-point number in the style of strtod(3).

 s Matches a sequence of non-white-space characters The input string
   stops at white space or at the maximum field width, whichever
   occurs first.

 S The same as s.

 c Matches a sequence of width count characters (default 1) The usual
   skip of leading white space is suppressed. To skip white space
   first, use an explicit space in the format.

 C The same as c.

 [ Matches a nonempty sequence of characters from the specified set of
   accepted characters The usual skip of leading white space is
   suppressed.  The string is to be made up of characters in
   (or not in) a particular set; the set is defined by the characters
   between the open bracket [ character and a close bracket ]
   character.  The set excludes those characters if the first
   character after the open bracket is a circumflex ^.  To include a
   close bracket in the set, make it the first character after the
   open bracket or the circumflex; any other position will end the
   set.  The hyphen character - is also special; when placed between
   two other characters, it adds all intervening characters to the
   set.  To include a hyphen, make it the last character before the
   final close bracket.  For instance, `[^]0-9-]' means the set
   ``everything except close bracket, zero through nine, and hyphen''.
   The string ends with the appearance of a character not in the (or,
   with a circumflex, in) set or when the field width runs out.

 n Nothing is expected; instead, the number of characters consumed
   thus far from the input is stored in results list.  This is not a
   conversion, although it can be suppressed with the * flag.

"

  (let ((fmt-pos 0)
	(fmt-len (length fmt))
	(results '()))

    (with-string-parsing (str :start start :end (or end (length str)))
      (labels ((parse-int (&key (radix 10) ; decimal by default
				(width MOST-POSITIVE-FIXNUM)) ; poor mans inifinity
		 (let* ((width% (or width MOST-POSITIVE-FIXNUM))
			(start-pos (pos))
			(sign (case (current)
				(#\- (advance) T)
				(#\+ (advance) NIL)
				(otherwise NIL))))

		   (bind (int-str (skip-while
				   #'(lambda (c)
				       (and (< (- (pos) start-pos) width%)
					    (digit-char-p c radix)))))
		     (let ((num (parse-integer int-str :radix radix)))
		       #+sfn-debug (format t "num: ~a~%" num)
		       (if sign
			   (return-from parse-int (- 0 num))
			   (return-from parse-int num)))))))
	(iter
	  ;; todo: what if the string is too short?
	  (while (< fmt-pos fmt-len))
	  (for c = (char fmt fmt-pos))
	  #+sfn-debug (format t "str: ~a fmt[~a]: ~a~%" (current) fmt-pos c)
	  (case c
	    ;; a directive
	    (#\%
	     ;; todo: handle more directives and their parameters
	     (incf fmt-pos)
	     #+sfn-debug (format t "directive: ~a~%" (char fmt fmt-pos))
	     (let* ((suppress?
		     ;; Suppresses assignment. The conversion that
		     ;; follows occurs as usual, but the result of the
		     ;; conversion is simply discarded
		     (if (char= (char fmt fmt-pos) #\*)
			 (incf fmt-pos)
			 NIL))
		    (width?
		     ;; In addition to this flag, there may be an
		     ;; optional maximum field width, expressed as a
		     ;; decimal integer, between the % and the
		     ;; conversion.
		     (if (digit-char-p (char fmt fmt-pos))
			 (multiple-value-bind (num new-pos)
			     (parse-integer fmt :start fmt-pos :junk-allowed T)
			   (setf fmt-pos new-pos)
			   num)
			 NIL)))

	       ;; DISPATCHING CONVERSION DIRECTIVE
	       (case (char fmt fmt-pos)
		 ;; an optionally signed decimal integer
		 ((#\d #\u)
		  (let ((n (parse-int :width width?)))
		    (unless suppress?
		      (push n results))))

		 ;; The integer is read in base 16 if it begins with
		 ;; `0x' or `0X', in base 8 if it begins with `0',and in
		 ;; base 10 otherwise
		 (#\i
		  (let ((n
			 (if (char= #\0 (current))
			     (progn
			       (advance)
			       (if (or (char= #\x (current))
				       (char= #\X (current)))
				   (progn
				     (advance)
				     (parse-int :radix 16 :width width?))
				   (parse-int :radix 8 :width width?)))
			     (parse-int :radix 10 :width width?))))
		    (unless suppress?
		      (push n results))))

		 ;; Matches an octal integer
		 (#\o
		  (let ((n (parse-int :radix 8 :width width?)))
		    (unless suppress?
		      (push n results))))

		 ;; a floating-point number
		 ((#\a #\A #\e #\E #\f #\F #\g #\G)
		  (error "not yet implemented"))

		 ;; Matches a sequence of non-white-space characters The
		 ;; input string stops at white space or at the maximum
		 ;; field width, whichever occurs first.
		 ((#\s #\S)
		  (let ((start-pos (pos))
			(width (or width? MOST-POSITIVE-FIXNUM)))
		    (bind (str (skip-until
				#'(lambda (c)
				    (or (char= c #\Space)
					(char= c #\Tab)
					(char= c #\Newline)
					(>= (- (pos) start-pos) width)))))
		      (unless suppress?
			(push str results)))))

		 ;; Matches a sequence of width count characters
		 ;; (default 1) The usual skip of leading white space is
		 ;; suppressed. To skip white space first, use an
		 ;; explicit space in the format.
		 ((#\c #\C)
		  (if width?
		      (progn
			(let ((chars))
			  (dotimes (i width?)
			    (push (current) chars)
			    (advance))
			  (unless suppress?
			    (push (reverse chars) results))))
		      (progn
			(unless suppress?
			  (push (current) results))
			(advance))))

		 ;; Matches a nonempty sequence of characters from the
		 ;; specified set of accepted characters The usual skip
		 ;; of leading white space is suppressed.  The string is
		 ;; to be made up of characters in (or not in) a
		 ;; particular set; the set is defined by the characters
		 ;; between the open bracket [ character and a close
		 ;; bracket ] character.  The set excludes those
		 ;; characters if the first character after the open
		 ;; bracket is a circumflex ^.  To include a close
		 ;; bracket in the set, make it the first character
		 ;; after the open bracket or the circumflex; any other
		 ;; position will end the set.  The hyphen character -
		 ;; is also special; when placed between two other
		 ;; characters, it adds all intervening characters to
		 ;; the set.  To include a hyphen, make it the last
		 ;; character before the final close bracket.  For
		 ;; instance, `[^]0-9-]' means the set ``everything
		 ;; except close bracket, zero through nine, and
		 ;; hyphen''. The string ends with the appearance of a
		 ;; character not in the (or,with a circumflex, in) set
		 ;; or when the field width runs out.
		 (#\[
		  (error "not yet implemented"))

		 ;; Nothing is expected; instead, the number of
		 ;; characters consumed thus far from the input is
		 ;; stored in results list.  This is not a conversion,
		 ;; although it can be suppressed with the * flag.
		 (#\n
		  (unless suppress?
		    (push (pos) results)))

		 ;; handle '%' similar to an ordinary character,
		 (#\%
		  (unless (char= #\% (current))
		    ;; mismatch
		    (return-from scanf))
		  (advance))

		 (otherwise
		  (error (format nil "invalid directive: ~a" (char fmt fmt-pos)))))
	       ;; advance to the next format character
	       (incf fmt-pos)))

	    ;; WHITESPACE
	    ((#\Space #\Tab #\Newline)
	     (skip* #\Space #\Tab #\Newline)
	     (incf fmt-pos))

	    ;; AN ORDINARY CHARACTER
	    (otherwise
	     (unless (char= (current)
			    (char fmt fmt-pos))
	       (return-from scanf))
	     (advance)
	     (incf fmt-pos))))))
    (reverse results)))

;; EOF
