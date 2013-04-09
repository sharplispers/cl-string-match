;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

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

;; Member variables for storing precomputed pattern data
(defstruct bm
  (right)
  (pat))

;; Map a collation element to an array index
(defun hash-bm (order)
  (round (mod (char-code order) 256)))

(defun initialize-bm (pat)
  (let ((bm (make-bm
	     :pat pat
	     :right (make-array 256 :element-type 'integer
				:initial-element -1)))
	(pat-len (length pat)))
    
    (loop :for j :from 0 :below pat-len
       :do (setf (aref (bm-right bm)
		       (hash-bm (char pat j)))
		 j))
    bm))

(defun search-bm (bm txt)
  "Search for pattern bm in txt."
  (let* ((txt-len (length txt))
	 (pat-len (length (bm-pat bm)))
	 (delta (- txt-len pat-len))
	 (skip 0))

    (loop :for i = 0 :then (+ i skip) :while (< i delta) :do
       (progn
	 ;; Does the pattern match the text at position i ?
	 (setf skip 0)
	 (loop
	    :for j :downfrom (- pat-len 1) :downto 0
	    :when (char/= (char (bm-pat bm) j)
			  (char txt (+ i j))) :do
	    (progn
	      (setf skip
		    (- j (aref (bm-right bm)
			       (hash-bm (char txt (+ i j))))))
	      (when (< skip 1)
		(setf skip 1))
	      (loop-finish)))
	 ;; found
	 (when (= skip 0)
	   (return-from search-bm i))))
    (return-from search-bm NIL)))

(defun string-contains-bm (pat txt)
  (search-bm (initialize-bm pat) txt))
    
;; EOF