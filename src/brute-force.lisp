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

;; got idea from CL-PPCRE
(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")

;; --------------------------------------------------------

(defun string-contains-brute (pat txt)
  "A Brute-force substring search implementation.

Brute-force substring search requires O(N x M) character compares to
search for a pattern of length M in a text of length N, in the worst
case.

Algorithm described in: Chapter 5, p. 760 in
  “Algorithms”, Robert Sedgewick and Kevin Wayne. 4th"

  (declare (type simple-string pat)
	   (type simple-string txt))

  (let ((pat-len (length pat))
	(txt-len (length txt)))
    
    (loop :for txt-pos fixnum :from 0 :to (- txt-len pat-len) :do
       (loop :for pat-pos fixnum :from 0 :below pat-len
	  :until (char/= (char txt (+ txt-pos pat-pos))
			 (char pat pat-pos))
	  :finally
	  (when (= pat-pos pat-len)
	    (return-from string-contains-brute txt-pos))))
    NIL))

;; EOF