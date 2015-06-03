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


;; additional information can be found at:
;;
;; http://www.geeksforgeeks.org/searching-for-patterns-set-3-rabin-karp-algorithm/
;;
;; or:
;;
;; http://www-igm.univ-mlv.fr/~lecroq/string/node5.html


(in-package :cl-string-match)

;; --------------------------------------------------------

(deftype ub32 ()
  '(unsigned-byte 31))

;; (define-constant +big-prime+ (the (unsigned-byte 32) 1646866399))
(define-constant +big-prime+ (the ub32 999925501));;   
(define-constant +alph-size+ (the ub32 256)) ;; CHAR-CODE-LIMIT

;; --------------------------------------------------------

(defstruct rk
  (pat nil    :type (or null simple-string))
  (pat-hash 0 :type ub32)
  (pat-len  0 :type ub32)
  (alph-size +alph-size+ :type ub32)
  (rm 1       :type ub32))

;; --------------------------------------------------------

(defun horner-hash (key end)
  "Horner hashing function implementation.

Computes the hash function for an END-digit base- +ALPH-SIZE+ number
represented as a char array in time proportional to END. (We pass END
as an argument so that we can use the function for both the pattern
and the text.)"

  (declare (type simple-string key)
	   #.*standard-optimize-settings*)

  (let ((h 0))
    (declare (type ub32 h))

    (loop :for j :of-type fixnum :from 0 :below end :do
       (setf h
	     (mod (the ub32
		       (+ (the ub32 (* (the ub32 +alph-size+)
				       (the ub32 h)))
			  (the ub32 (char-code (char key j)))))
		  (the ub32 +big-prime+))))
    (return-from horner-hash (the ub32 h))))
(declaim (ftype (function (simple-string fixnum) ub32) horner-hash)
	 (inline horner-hash))

;; --------------------------------------------------------

(defun initialize-rk (pat)
  (declare (type string pat)
	   #.*standard-optimize-settings*)

  (let ((idx (make-rk
	      :pat pat	; saving patter is required only for Las-Vegas
	      :pat-len (length pat)
	      :pat-hash (the ub32 (horner-hash pat (length pat)))
	      :rm 1)))

    ;; Compute R^(M-1) % Q for use in removing leading digit.
    (loop :for i :from 0 :below (- (length pat) 1) :do
       (setf (rk-rm idx)
	     (mod
	      (the ub32
		(* (the ub32 (rk-alph-size idx))
		   (the ub32 (rk-rm idx))))
	      (the ub32 +big-prime+))))
    idx))

;; --------------------------------------------------------

(defun check-rk-lv (idx txt i)
  "Las Vegas version: does pat[] match txt[i..i-M+1] ?"
  (declare (type simple-string txt)
	   (type rk idx)
	   #.*standard-optimize-settings*)

  (string= (rk-pat idx) txt
	   :start2 i
	   :end2 (+ (rk-pat-len idx)
		    i)))

;; (loop for j :from 0 :below (rk-pat-len idx) :when (char/= (char (rk-pat idx) j) (char txt (+ i j))) :do (return-from check-rk-lv nil)) T


;; --------------------------------------------------------

(defun check-rk-mk (i)
  "Monte Carlo version: always return true"
  (declare (ignore i))
  T)
(declaim (inline check-rk))

;; --------------------------------------------------------

(defun search-rk (idx txt-s)
  "Implementation of the Rabin-Karp substring search algorithm."
  (declare (type simple-string txt-s)
	   (type rk idx)
	   #.*standard-optimize-settings*)

  (let* ((txt txt-s)
	 (txt-len (length txt))
	 (txt-hash (horner-hash txt (rk-pat-len idx)))
	 (M (rk-pat-len idx)))

    (declare (fixnum txt-len M)
	     (ub32 txt-hash))

    ;; check for initial match
    (when (= txt-hash (rk-pat-hash idx))
      (when (check-rk-lv idx txt-s 0)
	(return-from search-rk 0)))

    (loop :for i :of-type fixnum :from 0 :to (- txt-len (rk-pat-len idx)) :do
       (progn
	 ;; Remove leading digit, add trailing digit, check for match.
	 (when (= (rk-pat-hash idx)
		  txt-hash)
	   (when (check-rk-lv idx txt-s i)
	     (return-from search-rk i)))

	 ;; Calulate hash value for next window of text: Remove
	 ;; leading digit, add trailing digit
	 (when (< i (- txt-len M))
	   ;; txtHash = (alphSize * (txtHash - txt[i]*RM) + txt[i+M]) % prime;
	   (let ((base (the ub32
			    (+ (the ub32 (* (the ub32 +alph-size+)
					    (- (the ub32 txt-hash)
					       (the ub32 (* (the ub32 (mod (the ub32 (char-code (char txt i)))
									   (the ub32 +alph-size+)))
							    (the ub32 (rk-rm idx)))))))

			       (the ub32 (mod (the ub32 (char-code (char txt (+ i m))))
					      (the ub32 +alph-size+)))))))

	     (setf txt-hash
		   (mod (the ub32 base)
			(the ub32 +big-prime+)))

	     ;; We might get negative value of t, converting it to positive
	     (when (< base 0)
	       (setf txt-hash (- (the ub32 +big-prime+)
				 (the ub32 txt-hash) )))))))
    NIL))

;; --------------------------------------------------------

(defun string-contains-rk (pat txt)
  (declare (type simple-string pat)
	   (type simple-string txt)
	   #.*standard-optimize-settings*)

  (search-rk (initialize-rk pat) txt))

;; EOF
