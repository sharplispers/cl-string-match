;;; ASCII strings: vectors made of unsigned bytes

;; Functions and data types are similar to the standard Common Lisp
;; functions and types but are prefixed with ub- to avoid naming
;; conflicts.
;;
;; This package aims at providing single-byte strings functionality
;; for Unicode-enabled Common Lisp implementations. Another aim is to
;; reduce memory footpring and boost performance of the
;; string-processing algorithms.

(defpackage :ascii-strings
  (:use :common-lisp :alexandria)
  (:nicknames :ascii)
  (:export
   ;; common functions and types
   :ub-char
   :ub-string
   :ub-buffer
   :ub-char-code-limit

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
   :ub-read-line))

(in-package :ascii-strings)

(declaim (optimize speed (safety 0)))

;; --------------------------------------------------------

(deftype ub-char () '(unsigned-byte 8))

;; strings must be vectors to allow for fill pointers and displacement
(deftype ub-string () '(vector ub-char))

;; buffer of octets should be a simple array to allow compiler
;; optimizations
(deftype ub-buffer () '(simple-array ub-char (*)))

(define-constant ub-char-code-limit 256
  :documentation
  "Maximum number of UB-CHARs is limited by a single byte. That is:
 just 256 characters are possible.")

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

(defun ub-STRING-UPCASE ())
(defun ub-STRING-DOWNCASE ())
(defun ub-STRING-CAPITALIZE ())

(defun ub-NSTRING-UPCASE ())
(defun ub-NSTRING-DOWNCASE ())
(defun ub-NSTRING-CAPITALIZE ())

(defun ub-STRING-TRIM ())

(defun ub-STRING-LEFT-TRIM ())

(defun ub-STRING-RIGHT-TRIM ())

(defun ub-STRING= ())
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

(defun ub-to-string (ustr)
  (declare (type ub-string ustr)
           (optimize speed))

  (let ((str (make-string (length ustr))))
    (loop :for i :from 0 :below (length ustr)
       :do (setf (char str i)
                 (code-char (ub-char ustr i))))
    str))

;; --------------------------------------------------------

(defun string-to-ub (str)
  (declare (type simple-string str)
           (optimize speed))
  (let ((ustr (make-ub-string (length str))))
    (loop :for i :from 0 :below (length str)
       :do (setf (aref ustr i)
                 (char-code (char str i))))
    ustr))

;; --------------------------------------------------------

;; (declaim (ftype (function (index) ub-string) make-ub-string)
;;         (inline make-ub-string))

(declaim (ftype (function (fixnum) ub-string) make-ub-string))
(defun make-ub-string (size)
  (make-array size :element-type 'ub-char))

;; --------------------------------------------------------
;; Reading lines from the input stream
;; --------------------------------------------------------

(define-constant +ub-line-reader-buffer-size+ (* 16 1024))

;; --------------------------------------------------------

(defstruct (ub-line-reader
             (:conc-name ub-line-reader.))
  stream
  (buffer (make-array +ub-line-reader-buffer-size+ :element-type 'ub-char) :type ub-buffer)
  ;; position of the last real data element in the buffer. it might be
  ;; less than the buffer size if read-sequence was incomplete or we
  ;; are nearing the end of file
  (fill 0 :type fixnum)
  ;; current position of the reader in the buffer
  (pos 0  :type fixnum)
  (eof nil :type boolean))

;; --------------------------------------------------------

(defun ub-read-line (reader)
  (declare (type ub-line-reader reader)
           (optimize speed (safety 0)))

  (when (ub-line-reader.eof reader)
    (return-from ub-read-line nil))

  ;; (format t "UB-READ-LINE: pos: ~a~%" (ub-line-reader.pos reader))
  (let* ((old-pos (ub-line-reader.pos reader))
         (new-pos
          (loop :for i :from old-pos
             :below (ub-line-reader.fill reader)
             :until (= (ub-char (ub-line-reader.buffer reader) i)
                       #.(char-code #\Newline))
             :finally (return i))))
    (declare (type fixnum old-pos)
             (type fixnum new-pos))

    ;; (format t "pos: old=~a new=~a fill=~a~%" old-pos new-pos (ub-line-reader.fill reader))

    (if (and (= (ub-char (ub-line-reader.buffer reader)
                         new-pos)
                #.(char-code #\Newline))
             ;; we are not advancing forward, it looks like we've
             ;; reached end of file and the last character is a newline
             (/= old-pos
                 (ub-line-reader.fill reader)))
        ;; we have found the end of the current line, advance the
        ;; position and return the new line
        (let ((new-line (ub-subseq (ub-line-reader.buffer reader)
                                   old-pos new-pos)))
          (setf (ub-line-reader.pos reader) (1+ new-pos))

          (return-from ub-read-line new-line))

        ;; else
        (progn
          ;; when the line ends in the next chunk of data, we have to
          ;; move the currently read incomplete line to the beginning
          ;; of the reader buffer, fill the rest of the buffer with
          ;; data from the stream and proceed with our task

          ;; (format t "read the next chunk of data~%")
          (setf (ub-line-reader.fill reader) (- new-pos
                                                (ub-line-reader.pos reader))
                (ub-line-reader.pos reader)  0)

          (replace (the ub-buffer (ub-line-reader.buffer reader))
                   (the ub-buffer (ub-line-reader.buffer reader))
                   :start1 0 :end1 (ub-line-reader.fill reader)
                   :start2 old-pos :end2 new-pos)

          (let ((old-fill (ub-line-reader.fill reader)))

            (setf (ub-line-reader.fill reader)
                  (read-sequence (ub-line-reader.buffer reader)
                                 (ub-line-reader.stream reader)
                                 :start (ub-line-reader.fill reader)
                                 :end +ub-line-reader-buffer-size+))
            (if (= (ub-line-reader.fill reader) old-fill)
                (progn
                  ;;(format t "eof~%")

                  ;; end of file is reached, return the last line and
                  ;; set the corresponding flag
                  (setf (ub-line-reader.eof reader) T)
                  (ub-subseq (ub-line-reader.buffer reader)
                             (ub-line-reader.pos reader) old-fill))

                ;; read the complete line after the second part was
                ;; obtained from the disk
                (ub-read-line reader)))))))

;; --------------------------------------------------------

(defun ub-subseq (sequence start &optional (end nil))
  "Returns either the SEQUENCE itself, or a dispaced array to the
SEQUENCE. This is meant as a memory-efficient replacement for the
ordinary SUBSEQ that allocates a new sequence."

  (declare (type (vector sequence)))
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

;; --------------------------------------------------------
;; some tests
;; --------------------------------------------------------

(defun test-ub-read-line ()
  (with-open-file (in "test.txt"
                      :direction :input
                      :element-type 'ub-char)
    (loop :with reader = (make-ub-line-reader :stream in)
       :for i :from 0 :below 10
       :for line = (ub-read-line reader)
       :while line
       :do (format t "[~a]: ~a~%" i (ub-to-string line)))))

(defun test-ub-count-lines (fname)
  (with-open-file (in fname
                      :direction :input
                      :element-type 'ub-char)
    (loop :with reader = (make-ub-line-reader :stream in)
       :for i :from 0
       :for line = (ub-read-line reader)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))

(defun test-count-lines (fname)
  (with-open-file (in fname
                      :direction :input
                      )
    (loop 
       :for i :from 0
       :for line = (read-line in nil)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))

;; EOF
