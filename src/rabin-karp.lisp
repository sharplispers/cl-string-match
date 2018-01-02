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


(in-package :cl-string-match)

(defsection @rabin-karp-section (:title "Rabin-Karp algorithm")
  "additional information can be found at:

http://www.geeksforgeeks.org/searching-for-patterns-set-3-rabin-karp-algorithm/

or:

http://www-igm.univ-mlv.fr/~lecroq/string/node5.html

Parts of the source code are modeled after the Java implementation
by Robert Sedgewick and Kevin Wayne:

http://algs4.cs.princeton.edu/53substring/RabinKarp.java.html
"
  (initialize-rk function)
  (search-rk function)
  (string-contains-rk function))

;; --------------------------------------------------------

(deftype rk-ub32 ()
  '(unsigned-byte))

(define-constant +big-prime+ (the rk-ub32 723925871))
(define-constant +alph-size+ (the rk-ub32 256)) ;; CHAR-CODE-LIMIT

;; --------------------------------------------------------

(defstruct rk
  (pat ""     :type simple-string)
  (pat-hash 0 :type rk-ub32)
  (pat-len  0 :type rk-ub32)
  (alph-size +alph-size+ :type rk-ub32)
  (rm 1       :type rk-ub32))

;; --------------------------------------------------------

(declaim (inline horner-hash))
(defun horner-hash (key end)
  "Horner hashing function implementation.

Computes the hash function for an END-digit base- +ALPH-SIZE+ number
represented as a char array in time proportional to END. (We pass END
as an argument so that we can use the function for both the pattern
and the text.)"

  (declare #.*standard-optimize-settings*
	   ;; we expect this function is called by trusted code
	   (type simple-string key)
	   (type fixnum end))

  (let ((h 0))
    (declare (type rk-ub32 h))

    (loop :for j :of-type fixnum :from 0 :below end :do
       (setf h
	     (mod (the rk-ub32
		       (+ (the rk-ub32 (* (the rk-ub32 +alph-size+)
					  (the rk-ub32 h)))
			  (the rk-ub32 (char-code (char key j)))))
		  (the rk-ub32 +big-prime+))))
    (return-from horner-hash (the rk-ub32 h))))
;; (declaim (ftype (function (simple-string fixnum) rk-ub32) horner-hash)
;;	 (inline horner-hash))

;; --------------------------------------------------------

(declaim (inline check-rk-lv))
(defun check-rk-lv (idx txt i)
  "Las Vegas version: does pat[] match txt[i..i-M+1] ?"
  (declare #.*standard-optimize-settings*
	   ;; we expect this function is called by trusted code
	   (type simple-string txt)
	   (type rk idx))

  (string= (rk-pat idx) txt
	   :start2 i
	   :end2 (+ i (rk-pat-len idx))))

;; --------------------------------------------------------

(declaim (inline check-rk-mk))
(defun check-rk-mk (i)
  "Monte Carlo version: always return true"
  (declare (ignore i))
  T)

;; --------------------------------------------------------

(defun initialize-rk (pat)
  (declare #.*standard-optimize-settings*)
  (check-type pat simple-string)

  (let ((idx (make-rk
	      :pat pat	; saving patter is required only for Las-Vegas
	      :pat-len (length pat)
	      :pat-hash (the rk-ub32 (horner-hash pat (length pat)))
	      :rm 1)))

    ;; Compute R^(M-1) % Q for use in removing leading digit.
    (loop :for i :from 0 :below (- (length pat) 1) :do
       (setf (rk-rm idx)
	     (mod
	      (the rk-ub32
		   (* (rk-alph-size idx)
		      (rk-rm idx)))
	      (the rk-ub32 +big-prime+))))
    idx))

;; --------------------------------------------------------

(defun search-rk (idx txt-s)
  "Implementation of the Rabin-Karp substring search algorithm."
  (declare #.*standard-optimize-settings*)
  (check-type txt-s simple-string)
  (check-type idx rk)

  (when (= 0 (rk-pat-len idx))
    (return-from search-rk 0))
  (when (= 0 (length txt-s))
    (return-from search-rk nil))

  (let* ((txt txt-s)
	 (txt-len (length txt))
	 (txt-hash (horner-hash txt (rk-pat-len idx)))
	 (pat-len (rk-pat-len idx)))
    (declare (fixnum txt-len pat-len)
	     (rk-ub32 txt-hash))

    ;; check for initial match
    (when (= txt-hash (rk-pat-hash idx))
      (when (check-rk-lv idx txt-s 0)
	(return-from search-rk 0)))

    ;; main check loop
    (loop :for i :of-type fixnum :from (rk-pat-len idx) :below txt-len
       :for offset = (+ (- i pat-len) 1) :do
       (progn
	 ;; Remove leading digit, add trailing digit, check for match.
	 ;; txtHash = (txtHash + Q - RM*txt.charAt(i-M) % Q) % Q;
	 (setf txt-hash
	       (mod (- (+ txt-hash +big-prime+)
		       (mod (* (rk-rm idx) (char-code (char txt-s (- i pat-len))))
			    +big-prime+))
		    +big-prime+))
	 ;; txtHash = (txtHash*R + txt.charAt(i)) % Q;
	 (setf txt-hash
	       (mod (+
		     (* txt-hash +alph-size+)
		     (char-code (char txt-s i)))
		    +big-prime+))
	 (when (= txt-hash (rk-pat-hash idx))
	   (when (check-rk-lv idx txt-s offset)
	     (return-from search-rk offset)))
	 ))
    NIL))

;; --------------------------------------------------------

(defun string-contains-rk (pat txt)
  (declare #.*standard-optimize-settings*)
  (check-type pat simple-string)
  (check-type txt simple-string)

  (when (= 0 (length pat))
    (return-from string-contains-rk 0))
  (when (= 0 (length txt))
    (return-from string-contains-rk nil))

  (search-rk (initialize-rk pat) txt))

;; EOF

#|
;; Remove leading digit, add trailing digit, check for match.
(when (= (rk-pat-hash idx)
	  txt-hash)
  (when (check-rk-lv idx txt-s i)
    (return-from search-rk i)))

;; Calulate hash value for next window of text: Remove
;; leading digit, add trailing digit
(when (< i (- txt-len M))
  ;; txtHash = (alphSize * (txtHash - txt[i]*RM) + txt[i+M]) % prime;
  (let ((base (the rk-ub32
		    (+ (the rk-ub32 (* (the rk-ub32 +alph-size+)
				    (- (the rk-ub32 txt-hash)
				       (the rk-ub32 (* (the rk-ub32 (mod (the rk-ub32 (char-code (char txt i)))
								   (the rk-ub32 +alph-size+)))
						    (the rk-ub32 (rk-rm idx)))))))

		       (the rk-ub32 (mod (the rk-ub32 (char-code (char txt (+ i m))))
				      (the rk-ub32 +alph-size+)))))))

    (setf txt-hash
	   (mod (the rk-ub32 base)
		(the rk-ub32 +big-prime+)))

    ;; We might get negative value of t, converting it to positive
    (when (< base 0)
      (setf txt-hash (- (the rk-ub32 +big-prime+)
			 (the rk-ub32 txt-hash))))
|#
