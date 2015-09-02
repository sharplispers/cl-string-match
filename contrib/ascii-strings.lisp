;;; ASCII strings: vectors made of unsigned bytes


(defpackage :ascii-strings
  (:use :common-lisp :alexandria)
  (:nicknames :ascii)
  (:documentation "This library implements functions and data types
similar to the standard Common Lisp functions and types but prefixed
with ub- to avoid naming conflicts.

This package aims at providing single-byte strings functionality
for Unicode-enabled Common Lisp implementations. Another aim is to
reduce memory footprint and boost performance of the
string-processing algorithms.

There are similar libraries/packages with slight differences. Check,
for instance, com.informatimago.common-lisp.cesarum.ascii.")
  (:export
   ;; common functions and types
   :ub-char
   :ub-string
   :ub-buffer
   :ub-char-code-limit
   :ascii-char-code

   ;; standard char functions
   :ub-char=
   :ub-char/=
   :ub-char<
   :ub-char>
   :ub-char<=
   :ub-char>=
   :ub-char-equal
   :ub-char-not-equal
   :ub-char-lessp
   :ub-char-greaterp
   :ub-char-not-greaterp
   :ub-char-not-lessp
   :ub-alpha-char-p
   :ub-alphanumericp
   :ub-digit-char
   :ub-digit-char-p
   :ub-graphic-char-p
   :ub-standard-char-p
   :ub-char-upcase
   :ub-char-downcase
   :ub-upper-case-p
   :ub-lower-case-p
   :ub-both-case-p
   :ub-char-code
   :ub-char-int
   :ub-code-char
   :ub-char-name
   :ub-name-char

   ;; standard string functions
   :ub-string-upcase
   :ub-string-downcase
   :ub-string-capitalize
   :ub-nstring-upcase
   :ub-nstring-downcase
   :ub-nstring-capitalize
   :ub-string-trim
   :ub-string-left-trim
   :ub-string-right-trim
   :ub-string=
   :ub-string/=
   :ub-string<
   :ub-string>
   :ub-string<=
   :ub-string>=
   :ub-string-equal
   :ub-string-not-equal
   :ub-string-lessp
   :ub-string-greaterp
   :ub-string-not-greaterp
   :ub-string-not-lessp

   ;; custom functions
   :ub-to-string
   :string-to-ub
   :make-ub-string
   :ub-subseq

   ;; line reader
   :make-ub-line-reader
   :ub-line-reader-file-position
   :ub-read-line
   :ub-read-line-raw
   :ub-read-line-string))

(in-package :ascii-strings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 3)
		     (safety 0))))

;; --------------------------------------------------------

(deftype ub-char () '(unsigned-byte 8))

;; strings must be vectors to allow for fill pointers and
;; displacement. However, SBCL does a better job on optimizations when
;; it deals with simple arrays.
(deftype ub-string () '(vector ub-char))

;; buffer of octets should be a simple array to allow compiler
;; optimizations
(deftype ub-buffer () '(simple-array ub-char (*)))

(define-constant ub-char-code-limit 256
  :documentation
  "Maximum number of UB-CHARs is limited by a single byte. That is:
 just 256 characters are possible.")

;; --------------------------------------------------------
;; CHARACTER FUNCTIONS
;; --------------------------------------------------------

(declaim (inline ub-char=))
(defun ub-char= (c1 c2)
  (= c1 c2))

(declaim (inline ub-char/=))
(defun ub-char/= (c1 c2)
  (/= c1 c2))

(declaim (inline ub-char<))
(defun ub-char< (c1 c2)
  (< c1 c2))

(declaim (inline ub-char>))
(defun ub-char> (c1 c2)
  (> c1 c2))

(declaim (inline ub-char<=))
(defun ub-char<= (c1 c2)
  (<= c1 c2))

(declaim (inline ub-char=>))
(defun ub-char>= (c1 c2)
  (>= c1 c2))

(defun ub-CHAR-EQUAL ())
(defun ub-CHAR-NOT-EQUAL ())
(defun ub-CHAR-LESSP ())
(defun ub-CHAR-GREATERP ())
(defun ub-CHAR-NOT-GREATERP ())
(defun ub-CHAR-NOT-LESSP ())

;; Function CHARACTER
;; Function CHARACTERP

(declaim (inline ub-alpha-char-p))
(defun ub-alpha-char-p (c)
  (or (ub-lower-case-p c)
      (ub-upper-case-p c)))

(declaim (inline ub-alphanumericp))
(defun ub-alphanumericp (c)
  (or (ub-alpha-char-p c)
      (and (>= c 48) (<= c 57))))

(defun ub-DIGIT-CHAR ())

(defun ub-DIGIT-CHAR-P ())

(defun ub-GRAPHIC-CHAR-P ())

(defun ub-STANDARD-CHAR-P ())

(declaim (inline ub-char-upcase))
(defun ub-char-upcase (c)
  (if (ub-lower-case-p c)
      (- c 32)
      c))

(declaim (inline ub-char-downcase))
(defun ub-char-downcase (c)
  (if (ub-upper-case-p c)
      (+ c 32)
      c))

(declaim (inline ub-upper-case-p))
(defun ub-upper-case-p (c)
  (and (>= c 65) (<= c 90)))

(declaim (inline ub-lower-case-p))
(defun ub-lower-case-p (c)
  (and (>= c 97) (<= c 122)))

(defun ub-BOTH-CASE-P ())

(declaim (inline ub-char-code))
(defun ub-char-code (char)
  char)

(declaim (inline ub-char-int))
(defun ub-char-int (char)
  char)

(declaim (ftype (function (character) ub-char) ascii-char-code)
         (inline ascii-char-code))
(defun ascii-char-code (char)
  "This is like the standard CHAR-CODE but returns an octet."
  (ldb (byte 8 0) (char-code char)))

(defun ub-CODE-CHAR ())

(defun ub-CHAR-NAME ())

(defun ub-NAME-CHAR ())

;; --------------------------------------------------------
;; STRING FUNCTIONS
;; --------------------------------------------------------

;; --------------------------------------------------------
;; Accessors to the elements of the ub-string

(declaim (ftype (function (ub-string fixnum) ub-char) ub-char)
         (inline ub-char))
(defun ub-char (string index)
  (aref string index))

(declaim (ftype (function (ub-string fixnum) ub-char) ub-schar)
         (inline ub-schar))
(defun ub-schar (string index)
  (aref string index))

;; --------------------------------------------------------

;; this function is ported from SBCL sources mipsstrops.lisp file and
;; is adopted for the ascii strings data types
;;
;; todo: implement remaining string comparison functions and properly
;; handle nil values for start,end parameters
(defun %ub-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (ub-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))

  (let ((len1 (- end1 start1))
        (len2 (- end2 start2)))
    (declare (fixnum len1 len2))
    (cond
      ((= len1 len2)
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index1 end1) nil)
         (declare (fixnum index1 index2))
         (if (/= (ub-schar string1 index1) (ub-schar string2 index2))
             (return index1))))
      ((> len1 len2)
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index2 end2) index1)
         (declare (fixnum index1 index2))
         (if (/= (ub-schar string1 index1) (ub-schar string2 index2))
             (return index1))))
      (t
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index1 end1) index1)
         (declare (fixnum index1 index2))
         (if (/= (ub-schar string1 index1) (ub-schar string2 index2))
             (return index1)))))))

;; --------------------------------------------------------

(defun ub-STRING-UPCASE ())
(defun ub-STRING-DOWNCASE ())
(defun ub-STRING-CAPITALIZE ())

(defun ub-NSTRING-UPCASE ())
(defun ub-NSTRING-DOWNCASE ())
(defun ub-NSTRING-CAPITALIZE ())

(defun ub-STRING-TRIM ())

(defun ub-STRING-LEFT-TRIM ())

(defun ub-STRING-RIGHT-TRIM ())

;; --------------------------------------------------------

(defun ub-string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (not (%ub-string-compare string1 start1 end1 string2 start2 end2)))

;; --------------------------------------------------------

(defun ub-STRING/= ())
(defun ub-STRING< ())
(defun ub-STRING> ())
(defun ub-STRING<= ())
(defun ub-STRING>= ())
(defun ub-STRING-EQUAL ())
(defun ub-STRING-NOT-EQUAL ())
(defun ub-STRING-LESSP ())
(defun ub-STRING-GREATERP ())
(defun ub-STRING-NOT-GREATERP ())
(defun ub-STRING-NOT-LESSP ())

;; --------------------------------------------------------
;; Data convertors

(defun ub-to-string (ustr &key (start 0) end)
  "Converts either an UB-STRING or UB-BUFFER into a standard Common
Lisp string.

START, END the start and end offsets within the given USTR to
           translate into a standard string.
"
  (declare (type fixnum start))
  (check-type ustr (or ub-string ub-buffer))

  (let* ((%end (the fixnum (or end (length ustr))))
	 (%length (- %end start))
	 (str (make-string (the fixnum %length))))
    (etypecase ustr
      (ub-buffer
       (loop :for i :from 0 :below %length
	  :do (setf (char str i)
		    (code-char (aref (the ub-buffer ustr)
				     (+ i start))))))
      (ub-string
       (loop :for i :from 0 :below %length
	  :do (setf (char str i)
		    (code-char (aref ustr
				     (+ i start)))))))
    str))

;; --------------------------------------------------------

(defun string-to-ub (str)
  "Convert a standard Lisp String into an octets vector."
  (declare (type simple-string str)
           (optimize speed))

  (let ((ustr (make-ub-string (length str))))
    (loop :for i :from 0 :below (length str)
       :do (setf (aref ustr i)
                 (ascii-char-code (char str i))))
    ustr))

;; --------------------------------------------------------

(declaim (ftype (function (fixnum) ub-string) make-ub-string)
         (inline make-ub-string))
(defun make-ub-string (size)
  (make-array size :element-type 'ub-char))

;; --------------------------------------------------------
;; Reading lines from the input stream
;; --------------------------------------------------------

(define-constant +ub-line-reader-buffer-size+ (* 1024 1024))

;; --------------------------------------------------------

(defstruct (ub-line-reader)
  stream
  (buffer (make-array +ub-line-reader-buffer-size+
                      :element-type 'ub-char)
          :type ub-buffer)
  ;; position of the last real data element in the buffer. it might be
  ;; less than the buffer size if read-sequence was incomplete or we
  ;; are nearing the end of file
  (fill 0 :type fixnum)
  ;; current position of the reader in the buffer
  (pos 0  :type fixnum)
  (eof nil :type boolean))

;; --------------------------------------------------------

(defun ub-line-reader-file-position (reader)
  "Returns the current position within the stream according to the
amount of information really read.

When the buffer caches more information than was really read by one of
UB-READ-LINE function the standard FILE-POSITION function will return
position of the buffer that is larger than the position that was read
by the user.

Returned number can be used by the standard FILE-POSITION function to
adjust the position within a stream."

  (check-type reader ub-line-reader)

  (with-slots (stream fill pos) reader
    ;; the logic is rather simple: file-position will return position
    ;; read till then end of the buffer (specified by the FILL slot)
    ;; so we subtract it and then add the position advanced by the
    ;; user that is specified by the POS slot
    (+ (- (file-position stream)
	  fill)
       pos)))

;; --------------------------------------------------------

(defun ub-read-line-raw (reader)
  "Reads data into the pre-allocated buffer in the READER structure
and returns two values: start and end positions of the line within the
buffer that can be used to extract this line contents from the
buffer.

Please note, that unlike the standard read-line or the
liberal-read-line by jasonmelbye this function works with the
Unix-type of lines - sequence of characters delimited by the Newline
symbol."

  (check-type reader ub-line-reader)

  (when (ub-line-reader-eof reader)
    (return-from ub-read-line-raw nil))

  (let* ((old-pos (ub-line-reader-pos reader))
         (new-pos (loop :for i :from old-pos :below (ub-line-reader-fill reader)
			:until (= (aref (ub-line-reader-buffer reader) i)
				  #.(char-code #\Newline))
			:finally (return i))))

    (declare (type fixnum old-pos)
             (type fixnum new-pos))

    (if (and (= (ub-char (ub-line-reader-buffer reader)
                         new-pos)
                #.(char-code #\Newline))
             ;; we are not advancing forward, it looks like we've
             ;; reached end of file and the last character is a newline
             (/= old-pos
                 (ub-line-reader-fill reader)))
        ;; we have found the end of the current line, advance the
        ;; position and return the new line
	(progn
	  (setf (ub-line-reader-pos reader) (1+ new-pos))
	  (return-from ub-read-line-raw (values old-pos new-pos)))

      ;; else
      (progn
	;; when the line ends in the next chunk of data, we have to
	;; move the currently read incomplete line to the beginning
	;; of the reader buffer, fill the rest of the buffer with
	;; data from the stream and proceed with our task
	(setf (ub-line-reader-fill reader) (- new-pos
					      (ub-line-reader-pos reader))
	      (ub-line-reader-pos reader)  0)

	(replace (the ub-buffer (ub-line-reader-buffer reader))
		 (the ub-buffer (ub-line-reader-buffer reader))
		 :start1 0 :end1 (ub-line-reader-fill reader)
		 :start2 old-pos :end2 new-pos)

	(let ((old-fill (ub-line-reader-fill reader)))

	  (setf (ub-line-reader-fill reader)
		(read-sequence (ub-line-reader-buffer reader)
			       (ub-line-reader-stream reader)
			       :start (ub-line-reader-fill reader)
			       :end +ub-line-reader-buffer-size+))
	  (if (= (ub-line-reader-fill reader) old-fill)
	      (progn
		;; end of file is reached, return the last line and
		;; set the corresponding flag
		(setf (ub-line-reader-eof reader) T)
		(return-from ub-read-line-raw
		  (values (ub-line-reader-pos reader)
			  old-fill)))

	    ;; read the complete line after the second part was
	    ;; obtained from the disk
	    (ub-read-line-raw reader)))))))
(declaim (inline ub-read-line-raw))

;; --------------------------------------------------------

(defun ub-read-line (reader)
  "Reads data into a pre-allocated buffer in the reader structure and
returns an array displaced to the contents of the next line in that
buffer."

  (check-type reader ub-line-reader)

  (when (ub-line-reader-eof reader)
    (return-from ub-read-line nil))

  (multiple-value-bind (start end) (ub-read-line-raw reader)
    (make-array (- end start)
		:element-type 'ub-char
		:displaced-to (ub-line-reader-buffer reader)
		:displaced-index-offset start)))

;; --------------------------------------------------------

(defun ub-read-line-string (reader)
  "Reads data into a pre-allocated buffer in the reader structure and
returns a standard Lisp string."

  (check-type reader ub-line-reader)

  (when (ub-line-reader-eof reader)
    (return-from ub-read-line-string nil))

  (multiple-value-bind (start end) (ub-read-line-raw reader)
    (ub-to-string (ub-line-reader-buffer reader)
		  :start start :end end)))

;; --------------------------------------------------------

(defun ub-subseq (sequence start &optional (end nil))
  "Returns either the SEQUENCE itself, or a dispaced array to the
SEQUENCE. This is meant as a memory-efficient replacement for the
ordinary SUBSEQ that allocates a new sequence."

  (check-type sequence vector)
  (check-type start fixnum)
  (check-type end (or null fixnum))

  (if (and (zerop start)
           (or (null end)
               (= end (length sequence))))
      ;; limits match, return the sequence itself
      sequence
      ;; create a displaced array
      (make-array (- (if end
			 (min end (length sequence))
			 (length sequence))
		     start)
		  :element-type (array-element-type sequence)
		  :displaced-to sequence
		  :displaced-index-offset start)))

;; EOF
