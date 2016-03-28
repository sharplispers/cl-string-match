;;; A simple scanf implementation in Lisp

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


(defpackage :simple-scanf
  (:use :common-lisp :alexandria :iterate :proc-parse)
  (:nicknames :snf)
  (:documentation "A simple scanf implementation in Common Lisp.

It is not totally compatible with the POSIX scanf(3) function. See
documentation for the `scanf' function for description of implemented
features subset.

The `scanf' function can be used to extract necessary information from
strings or for matching them against a simple pattern.")

  (:export
   :scanf))

;; --------------------------------------------------------

(in-package :simple-scanf)

(declaim (optimize speed))

;; --------------------------------------------------------

(defun fscanf (fmt stream &key (start 0))
  (error "not yet implemented"))

;; --------------------------------------------------------

(defun pred-char= (c)
  "Returns a function of one argument that tests it for equality with
the char C."
  #'(lambda (x) (char= x c)))

;; --------------------------------------------------------

(defun pred-char-range (left right)
  "Returns a function of one argument that tests if it is within a
character range limited with LEFT and RIGHT.

Please take into account that standard char comparison functions are
used: char<= and char>=. In some cases, alphabetic characters order
from the Unicode characters space might be incompatible with its
numeric order.

For example, ukrainian short i comes between a and ya alphabetically,
but its char-code is bigger than that of 'ya'."

  #'(lambda (c) (and (char>= c left)
		     (char<= c right))))

;; --------------------------------------------------------

(defun postprocess-results (results)
  ;; currently we just reverse results, but this
  ;; function encapsulates this functionality to make
  ;; it more uniform across exit points
  (reverse results))
(declaim (inline postprocess-results))

;; --------------------------------------------------------

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
   Matches a floating-point number in the style of PARSE-FLOAT.

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

Returns:

  Returns a list of extracted data using specified conversions in the
  order that they appear. In case if a un-suppressed conversion
  specifier does not match input, processing is stopped, and the data
  extracted at this moment is returned."

  (let ((fmt-pos 0)
	(fmt-len (length fmt))
	(results '()))

    (with-string-parsing (str :start start :end (or end (length str)))
      (labels ((PARSE-INT (&key (radix 10) ; decimal by default
				(width MOST-POSITIVE-FIXNUM) ; poor mans inifinity
				(suppress NIL) ; do we really need it?
				)
		 (let* ((width% (or width MOST-POSITIVE-FIXNUM))
			(start-pos (pos)))

		   (bind (int-str
			  (when (or (char= (current) #\-)
				    (char= (current) #\+))
			    (advance))
			  (skip-while
			   #'(lambda (c)
			       (and (< (- (pos) start-pos) width%)
				    (digit-char-p c radix)))))
		     (unless suppress
		       (let ((num (parse-integer int-str :radix radix :junk-allowed T)))
			 #+sfn-debug (format t "num: ~a~%" num)
			 (return-from parse-int num))))))

	       (PARSE-FLOAT (&key (width MOST-POSITIVE-FIXNUM) ; poor mans inifinity
				  (suppress NIL) ; do we really need it?
				  )
		 ;; a floating-point number consists of a integer
		 ;; part, decimal part separated from the integer part
		 ;; with a dot, and an optional exponent part:
		 ;;
		 ;; for example: 0.999e+3; 999.0; 999 are all valid
		 ;; floating-point numbers that this function handles
		 (let* ((width% (or width MOST-POSITIVE-FIXNUM))
			(start-pos (pos)))

		   (bind (float-str
			  ;; integer part
			  (when (or (char= (current) #\-)
				    (char= (current) #\+))
			    (advance))
			  (skip-while
			   #'(lambda (c)
			       (and (< (- (pos) start-pos) width%)
				    (digit-char-p c))))
			  (when (char= (current) #\.)
			    (advance)
			    ;; decimal part
			    (skip-while
			     #'(lambda (c)
				 (and (< (- (pos) start-pos) width%)
				      (digit-char-p c))))
			    ;; exponent part
			    (when (char-equal (current) #\e)
			      (advance)
			      (when (or (char= (current) #\-)
					(char= (current) #\+))
				(advance))
			      (skip-while
			       #'(lambda (c)
				   (and (< (- (pos) start-pos) width%)
					(digit-char-p c)))))))
		     (unless suppress
		       (let ((num (parse-float:parse-float float-str :junk-allowed T)))
			 #+sfn-debug (format t "num: ~a~%" num)
			 (return-from parse-float num))))))


	       (TRANSLATE-CHAR-SEQ ()
		 ;; this function is called when the directive
		 ;; dispatcher stumbles upon a [ directive. It parses
		 ;; format string starting from the fmt-pos and
		 ;; generates a list of functions of one argument that
		 ;; test if the given char satisfies a condition from
		 ;; the format directive. Exclusion flag is handled by
		 ;; the caller
		 (let* ((predicates nil))
		   (when (char= (char fmt fmt-pos) #\])
		     (push #'(lambda (c) (char= c #\])) predicates)
		     (incf fmt-pos))

		   (iter
		     ;; because of the range operation we will deal
		     ;; with the characters in triples
		     (for c1 = (and (< fmt-pos fmt-len) (char fmt fmt-pos)))
		     (for c2 = (and (< (+ fmt-pos 1) fmt-len) (char fmt (+ fmt-pos 1))))
		     (for c3 = (and (< (+ fmt-pos 2) fmt-len) (char fmt (+ fmt-pos 2))))
		     (until (or (null c1) (char= c1 #\] )))
		     (push
		      (if (and c2 (char= c2 #\-))
			  ;; might be a range op
			  (if (and c3 (char= c3 #\]))
			      ;; no, it is not, just a dash
			      (progn
				(incf fmt-pos)
				(pred-char= c1))
			      ;; it *is* a range op
			      (progn
				(incf fmt-pos 3)
				(pred-char-range c1 c3)))
			  ;; no dash, just a char
			  (progn
			    (incf fmt-pos)
			    (pred-char= c1)))
		      predicates))
		   predicates))

	       (PREDICATE-MATCHES (preds c)
		 ;; returns T if at least one predicate in the preds
		 ;; list matches with the character c. Predicates list
		 ;; might be generated by the TRANSLATE-CHAR-SEQ
		 (iter
		   (for pred in preds)
		   (when (funcall pred c)
		     (return-from PREDICATE-MATCHES T)))))
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
		  (let ((n (parse-int :width width? :suppress suppress?)))
		    (unless suppress?
		      (unless n
			;; mismatch
			(return-from scanf (values (postprocess-results results) nil)))
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
				     (parse-int :radix 16 :width width? :suppress suppress?))
				   (parse-int :radix 8 :width width? :suppress suppress?)))
			     (parse-int :radix 10 :width width? :suppress suppress?))))
		    (unless suppress?
		      (unless n
			;; mismatch
			(return-from scanf (values (postprocess-results results) nil)))
		      (push n results))))

		 ;; Matches an octal integer
		 (#\o
		  (let ((n (parse-int :radix 8 :width width? :suppress suppress?)))
		    (unless suppress?
		      (unless n
			;; mismatch
			(return-from scanf (values (postprocess-results results) nil)))
		      (push n results))))

		 ;; a floating-point number
		 ((#\a #\A #\e #\E #\f #\F #\g #\G)
		  (let ((n (parse-float :width width? :suppress suppress?)))
		    (unless suppress?
		      (unless n
			;; mismatch
			(return-from scanf (values (postprocess-results results) nil)))
		      (push n results))))

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
					(char= c #\Return)
					(char= c #\Page)
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
			    (unless suppress?
			      (push (current) chars))
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
		  (incf fmt-pos)
		  ;; todo: rest of processing
		  (let* ((exclude? (if (char= #\^ (char fmt fmt-pos))
				       (progn (incf fmt-pos) T)
				       NIL))
			 (predicates (translate-char-seq)))

		    (bind (str
			   (if exclude?
			       (skip-until #'(lambda (c) (predicate-matches predicates c)))
			       (skip-while #'(lambda (c) (predicate-matches predicates c)))))
		      (unless suppress?
			(push str results)))))

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
		    (return-from scanf (values (postprocess-results results) nil)))
		  (advance))

		 (otherwise
		  (error (format nil "invalid directive: ~a" (char fmt fmt-pos)))))
	       ;; advance to the next format character
	       (incf fmt-pos)))

	    ;; WHITESPACE
	    ((#\Space #\Tab #\Return #\Newline #\Page)
	     (skip* #\Space #\Tab #\Return #\Newline #\Page)
	     (incf fmt-pos))

	    ;; AN ORDINARY CHARACTER
	    (otherwise
	     (unless (char= (current)
			    (char fmt fmt-pos))
	       (return-from scanf (values (postprocess-results results) nil)))
	     (advance)
	     (incf fmt-pos))))))
    (values (postprocess-results results) T)))

;; EOF
