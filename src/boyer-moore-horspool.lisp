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


;;; Boyer-Moore-Horspool algorithm implementation based on description
;; from Wikipedia article
;;
;; http://en.wikipedia.org/wiki/Boyer-Moore-Horspool_algorithm

(in-package :cl-string-match)

;; --------------------------------------------------------

(defmacro define-bmh-matcher (index-name
			      initialize-name
                              search-name
                              matcher-name
                              &key
			      (key-get 'char)
			      (key-code 'char-code)
			      (key-cmp= 'char=)
			      (empty-pat "")
			      (alphabet-size char-code-limit)
			      (data-type 'simple-string))

  (let ((make-index (format-name "MAKE-~a" index-name))
	(the-bad-char-skip (format-name "~a-BAD-CHAR-SKIP" index-name))
	(the-pat (format-name "~a-PAT" index-name))
	(the-pat-len (format-name "~a-PAT-LEN" index-name)))
    `(progn

       ;; --------------------------------------------------------
       ;; Member variables for storing precomputed pattern data
       (defstruct ,index-name
	 (bad-char-skip #() :type (simple-array fixnum (*)))
	 (pat ,empty-pat :type ,data-type)
	 (pat-len 0 :type fixnum))

       ;; --------------------------------------------------------

       (defun ,initialize-name (pat)
	 "Preprocess the needle.

Initialize the table to default value."

	 (declare (type ,data-type pat)
		  #.*standard-optimize-settings*)

	 ;; When a character is encountered that does not occur in the
	 ;; needle, we can safely skip ahead for the whole length of the
	 ;; needle.
	 (let ((idx
		(,make-index
		 :pat pat
		 :pat-len (length pat)
		 :bad-char-skip 
		 (make-array ,alphabet-size
			     :element-type 'fixnum
			     :initial-element (the fixnum (length pat))))))

	   (loop :for c :across pat
	      :for i :from 0 :to (length pat) :do
	      (setf (aref (,the-bad-char-skip idx)
			  (,key-code c))
		    (- (length pat) i 1)))
	   idx))

       ;; --------------------------------------------------------

       (defun ,search-name (bmh txt)
	 "Search for pattern BMH in TXT."

	 (declare (type ,data-type txt)
		  #.*standard-optimize-settings*)
	 (let ((haystack 0)
	       (hlen (length txt))
	       (last (- (,the-pat-len bmh) 1)))
	   (declare (type fixnum haystack)
		    (type fixnum hlen)
		    (type fixnum last))

	   ;; Search the haystack, while the needle can still be within it.
	   (loop :while (>= hlen (,the-pat-len bmh)) :do
	      (progn
		;; scan from the end of the needle
		(loop :for scan :of-type fixnum :from last :downto -1
		   :while (,key-cmp= (,key-get txt (the fixnum (+ haystack scan)))
				     (,key-get (,the-pat bmh) scan))
		   :when (= scan 0)
		   :do (return-from ,search-name haystack))

		;; otherwise, we need to skip some bytes and start
		;; again. Note that here we are getting the skip value based
		;; on the last byte of needle, no matter where we didn't
		;; match. So if needle is: "abcd" then we are skipping based
		;; on 'd' and that value will be 4, and for "abcdd" we again
		;; skip on 'd' but the value will be only 1. The alternative
		;; of pretending that the mismatched character was the last
		;; character is slower in the normal case (E.g. finding
		;; "abcd" in "...azcd..." gives 4 by using 'd' but only
		;; 4-2==2 using 'z'.
		(let ((skip (aref (,the-bad-char-skip bmh)
				  (,key-code (,key-get txt last)))))
		  (declare (type fixnum skip))
		  (setf hlen (- hlen skip))
		  (setf haystack (+ haystack  skip)))))
	   nil))


       ;; --------------------------------------------------------

       (defun ,matcher-name (pat txt)
	 (declare (type ,data-type pat)
		  (type ,data-type txt)
		  #.*standard-optimize-settings*)

	 (,search-name (,initialize-name pat) txt))

       )))

;; --------------------------------------------------------

(define-bmh-matcher bmh initialize-bmh search-bmh string-contains-bmh)

;; The following set of BMH matchers operate on strings that contain
;; characters in the range 0-256 (single-byte or octet). Therefore,
;; the skip array in the index is not equal to the CHAR-CODE-LIMIT
;; that is huge for Lisp implementations with Unicode support, but has
;; a fixed size of 256 cells
(define-bmh-matcher bmh8 initialize-bmh8 search-bmh8 string-contains-bmh8
		    :empty-pat ""
                    :key-code ascii-char-code
                    :alphabet-size ub-char-code-limit)

(export 'bmh8)
(export 'initialize-bmh8)
(export 'search-bmh8)
(export 'string-contains-bmh8)

;; EOF