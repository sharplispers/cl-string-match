;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2013, 2018 Victor Anyakin <anyakinvictor@yahoo.com>
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

(defsection @brute-force-section (:title "Brute force")
  "A Brute-force substring search implementation.
Brute-force algorithm is one of the simpliest but less robust among
the substring search algorithms.

CL-STRING-MATCH offers a code template for application specific
sequence and data types: DEFINE-BRUTE-MATCHER and two pre-defined
brute search functions, one for a standard Lisp
string (STRING-CONTAINS-BRUTE) and another for unsigned-byte (8 bits
per char) strings (STRING-CONTAINS-BRUTE-UB).

Brute-force substring search requires O(n x m) character compares to
search for a pattern of length m in a text of length n, in the worst
case.

Algorithm described in: Chapter 5, p. 760 in 'Algorithms', Robert
  Sedgewick and Kevin Wayne. 4th
"
  (define-brute-matcher macro)
  (string-contains-brute function)
  (string-contains-brute-ub function))

;; --------------------------------------------------------

(defmacro define-brute-matcher (variant-tag
				&key
				  (key-get 'char)
				  (key-cmp/= 'char/=)
				  (data-type 'simple-string))
  "Expands into a function definition performing a brute search.

The function will be named STRING-CONTAINS-BRUTE suffixed with the
VARIANT-TAG.

KEY-GET is the getter function of two elements: sequence and index of
element within this sequence.

KEY-CMP/= is comparator, that given two elements returns T when they
are not equal.

DATA-TYPE is the type of the sequence over which the search is
performed."

  (let ((matcher-name (format-name "STRING-CONTAINS-BRUTE~A" variant-tag)))
    `(progn
       (defun ,matcher-name (pat txt &key (start1 0) end1 (start2 0) end2)
	 "Search for the given substring PAT in the given text
TXT. Search can optionally be performed over fragments delimeted by
START1, END1 for the pattern and START2, END2 for the text."

	 (declare #.*standard-optimize-settings*)

	 (check-type pat ,data-type)
	 (check-type txt ,data-type)
	 (check-type start1 fixnum)
	 (check-type end1 (or null fixnum))
	 (check-type start2 fixnum)
	 (check-type end2 (or null fixnum))

         (iter
           (declare (iterate:declare-variables))
           (with pat-len = (length pat))
           (with txt-len = (length txt))
           ;; we don't check if the start and end parameters are valid
           (with end1 = (if end1 (the fixnum end1) pat-len))
           (with end2 = (if end2 (the fixnum end2) txt-len))
           (for txt-pos from start2 to (- end2 end1))
           (iter
             (for pat-pos from start1 below end1)
             (until (,key-cmp/= (,key-get txt (- (+ txt-pos pat-pos) start1))
                                (,key-get pat pat-pos)))
             (finally
              (when (= pat-pos end1)
                ;; found match
                (return-from ,matcher-name txt-pos)))))
         ;; no match found
         NIL))))

;; --------------------------------------------------------

(define-brute-matcher "")

(define-brute-matcher "-UB"
    :key-get ub-char
    :key-cmp/= ub-char/=
    :data-type ub-string)

;; EOF
