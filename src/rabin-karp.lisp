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

;; --------------------------------------------------------

(defparameter +big-prime+ 479001599)

;; --------------------------------------------------------

(defstruct rk
  (pat)
  (pat-hash)
  (pat-len)
  ;;  (alph-size 256)
  (alph-size CHAR-CODE-LIMIT)
  (rm 1))

;; --------------------------------------------------------

(defun horner-hash (key end)
  "Horner hashing function implementation.

Computes the hash function for an END-digit base- CHAR-CODE-LIMIT
number represented as a char array in time proportional to END. (We
pass END as an argu- ment so that we can use the function for both the
pattern and the text.)"

  (declare (type string key))

  (let ((h 0))
    (loop :for j :from 0 :below end :do
       (setf h
	     (mod (+ (* char-code-limit h)
		     (char-code (char key j)))
		  +big-prime+)))
    h))

;; --------------------------------------------------------

(defun initialize-rk (pat)
  (declare (type string pat))

  (let ((idx (make-rk
	      :pat pat	; saving patter is required only for Las-Vegas
	      :pat-len (length pat)
	      :rm 1)))

    ;; Compute R^(M-1) % Q for use in removing leading digit.
    (loop :for i :from 1 :below (length pat) :do
       (setf (rk-rm idx)
	     (rem
	      (* (rk-alph-size idx) (rk-rm idx))
	      +big-prime+)))

    (setf (rk-pat-hash idx)
	  (horner-hash pat (rk-pat-len idx)))
    idx))

;; --------------------------------------------------------

(defun check-rk (i)
  (declare (ignore i))
  T)

;; --------------------------------------------------------

;; TODO: disfunctional, debug required
(defun search-rk (idx txt)
  (declare (type string txt)
	   (type rk idx))

  (let* ((txt-len (length txt))
	 (txt-hash (horner-hash txt txt-len))
	 (M (rk-pat-len idx)))

    ;; check for initial match
    (when (= txt-hash (rk-pat-hash idx))
      (return-from search-rk 0))

    (loop :for i :from M :below txt-len :do
       (progn
	 ;; Remove leading digit, add trailing digit, check for match.

	 ;; txtHash = (txtHash + Q - RM*txt.charAt(i-M) % Q) % Q;
	 (setf txt-hash (mod (- (+ txt-hash +big-prime+)
				(mod (* (rk-rm idx)
					(char-code (char txt (- i M))))
				     +big-prime+))
			     +big-prime+))
	 ;; txtHash = (txtHash*R + txt.charAt(i)) % Q
	 (setf txt-hash
	       (mod (+ (* txt-hash (rk-alph-size idx))
		       (char-code (char txt i)))
		    +big-prime+))

	 (when (and (= (rk-pat-hash idx)
		       txt-hash)
		    (check-rk (+ (- i M)
				 1)))
	   (return-from search-rk (+ (- i M)
				     1)))))
    NIL))

;; --------------------------------------------------------

;; EOF