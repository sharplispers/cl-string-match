;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2013, Victor Anyakin <anyakinvictor@yahoo.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the organization nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; The following implementation is based on algorithm described in:
;;
;; Algorithm described in: Chapter 5, p. 772 in
;;  “Algorithms”, Robert Sedgewick and Kevin Wayne. 4th
;;
;; ``Efficient Text Searching in Java'' By Laura Werner.
;; (appeared in Java Report, February 1999)
;;
;; http://icu-project.org/docs/papers/efficient_text_searching_in_java.html
;;
;; And some other sources.
;;
;; Current implementation uses bad character and good suffix skip
;; heuristics.

(in-package :cl-string-match)

;; --------------------------------------------------------

;; Member variables for storing precomputed pattern data
(defstruct bm
  (bad-char)			; bad character skip (or occurence shift; delta-1)
  (good-suffix)			; good suffix shift (or matching shift; delta-2)
  (pat)				; the pattern
  )

;; --------------------------------------------------------

;; Map a collation element to an array index
(defun hash-bm (order)
  (round (mod (char-code order) 256)))

;; --------------------------------------------------------

(defun initialize-bad-char (idx)
  "Initialize the bad character skip for the Boyer-Moore algorithm."

  (declare (type bm idx))

  (let ((pat-len (length (bm-pat idx))))
    (loop :for j :from 0 :below (- pat-len 1)
       :do (setf (aref (bm-bad-char idx)
		       (hash-bm (char (bm-pat idx) j)))
		 (- pat-len j 1)))))

;; --------------------------------------------------------

(defun bm-suffixes (pat suffixes)
  (let* ((pat-len (length pat))
	 (g (- pat-len 1))
	 (f 0))

    (setf (aref suffixes (- pat-len 1)) pat-len)

    (loop :for i :downfrom (- pat-len 2) :to 0 :do
       (if (and (> i g)
		(< (elt suffixes (- (+ i pat-len) 1 f))
		   (- i g)))
	   ;; then
	   (setf (elt suffixes i)
		 (elt suffixes (- (+ i pat-len) 1 f)))
	   ;; else
	   (progn
	     (when (< i g)
	       (setf g i))
	     (setf f i)
	     (loop :while (and (>= g 0)
			       (char= (char pat g)
				      (char pat (- (+ g pat-len) 1 f))))
		:do (decf g))
	     (setf (elt suffixes i)
		   (- f g)))))))

;; --------------------------------------------------------

(defun initialize-good-suffix (idx)
  "Initialize the good suffix skip function table for the
Boyer-Moore algorithm."
  (let ((suff (make-array (length (bm-pat idx))))
	(pat-len (length (bm-pat idx))))

    (bm-suffixes (bm-pat idx) suff)

    (loop :with j = 0
       :for i :downfrom (- pat-len 1) :to 0
       :when (= (elt suff i)
		(+ i 1))
       :do
       (loop :while (< j (- pat-len i 1))
	  :when (= (elt (bm-good-suffix idx) j)
		   pat-len)
	  :do (setf
	       (elt (bm-good-suffix idx) j)
	       (- pat-len i 1))
	  :do (incf j)))

    (loop :for i :from 0 :upto (- pat-len 2) :do
       (setf (elt (bm-good-suffix idx)
		  (- pat-len 1 (elt suff i)))
	     (- pat-len i 1)))))

;; --------------------------------------------------------

(defun initialize-bm (pat)
  (declare (type string pat)
	   #.*standard-optimize-settings*)

  (when (> (length pat) 0)
    (let ((idx (make-bm
		:pat pat
		:bad-char (make-array 256
				      :element-type 'integer
				      :initial-element (length pat))
		:good-suffix (make-array (length pat)
					 :element-type 'integer
					 :initial-element (length pat)))))

      (initialize-bad-char idx)
      (initialize-good-suffix idx)
      idx)))

;; --------------------------------------------------------

(defun search-bm (idx txt)
  "Search for pattern bm in txt."

  (declare (type bm idx)
	   (type simple-string txt)
	   #.*standard-optimize-settings*)

  (when (= 0 (length (bm-pat idx)))
    (return-from search-bm 0))
  (when (= 0 (length txt))
    (return-from search-bm nil))

  (let* ((txt-len (length txt))
	 (pat-len (length (bm-pat idx)))
	 (delta (- txt-len pat-len)))

    (declare (fixnum txt-len)
	     (fixnum pat-len)
	     (fixnum delta))

    ;; using (<= j delta) instead of (< j delta)
    (loop :with j fixnum = 0 :while (<= j delta) :do
       (let ((i (- pat-len 1)))
	 ;; Does the pattern match the text at position i ?
	 (loop
	    :while (and (>= i 0)
			(char= (char (bm-pat idx) i)
			       (char txt (+ i j))))
	    :do (decf i))

	 (when (< i 0)
	   (return-from search-bm j))

	 (let ((skip (max (- (elt (bm-good-suffix idx) i) 1)
			  (+ (- (aref (bm-bad-char idx)
				      (hash-bm (char txt (+ i j))))
				pat-len)
			     i 1))))
	   (incf j skip))))

    (return-from search-bm NIL)))

;; --------------------------------------------------------

(defun string-contains-bm (pat txt)
  (declare (type simple-string pat)
	   (type simple-string txt)
	   #.*standard-optimize-settings*)

  (when (= 0 (length pat))
    (return-from string-contains-bm 0))
  (when (= 0 (length txt))
    (return-from string-contains-bm nil))

  (search-bm (initialize-bm pat) txt))

;; (length "abaababaababaababaabab")
;; (setf idx (initialize-bm "abaababaababaababaabab"))


;; pat= "gloria"
;; (initialize-bm "gloria")

;; txt="Sic transit gloria mundi, non transit gloria Gundi!"

;; (string-contains-bm "gloria" "Sic transit gloria mundi, non transit gloria Gundi!")

;; EOF
