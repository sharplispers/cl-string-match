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

;; Member variables for storing precomputed pattern data
(defstruct bmh
  (bad-char-skip)
  (pat)
  (pat-len))

;; --------------------------------------------------------

(defun initialize-bmh (pat)
  "Preprocess.
Initialize the table to default value. "

  (declare #.*standard-optimize-settings*)

  ;; When a character is encountered that does not occur in the
  ;; needle, we can safely skip ahead for the whole length of the
  ;; needle.
  (let ((idx
	 (make-bmh
	  :pat pat
	  :pat-len (length pat)
	  :bad-char-skip (make-array char-code-limit
				     :initial-element (length pat)))))

    (loop :for c :across pat
       :for i :from 0 :to (length pat) :do
       (setf (aref (bmh-bad-char-skip idx)
		   (char-code c))
	     (- (length pat) i 1)))
    idx))

;; --------------------------------------------------------

(defun search-bmh (bmh txt)
  "Search for pattern bm in txt."

  (declare #.*standard-optimize-settings*)
  (let ((haystack 0)
	(hlen (length txt))
	(last (- (bmh-pat-len bmh) 1)))

    ;; Search the haystack, while the needle can still be within it.
    (loop :while (>= hlen (bmh-pat-len bmh)) :do
       (progn
	 ;; scan from the end of the needle
	 (loop :for scan = last :then (- scan 1)
	    :while (char= (char txt (+ haystack scan))
			  (char (bmh-pat bmh) scan))
	    :when (= scan 0)
	    :do (return-from search-bmh haystack))

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
	 (let ((skip (aref (bmh-bad-char-skip bmh)
			   (char-code (char txt last)))))
	   
	   (setf hlen (- hlen skip))
	   (setf haystack (+ haystack  skip)))))
    nil))

;; --------------------------------------------------------

(defun string-contains-bmh (pat txt)
  (declare (type string pat)
	   (type string txt)
	   #.*standard-optimize-settings*)

  (search-bmh (initialize-bmh pat) txt))


;; EOF