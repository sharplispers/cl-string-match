;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2015, 2018 Victor Anyakin <anyakinvictor@yahoo.com>
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

;; Different utility functions for dealing with strings or similar
;; data structures that don't fit under conventional topics in this
;; library.

(in-package :cl-string-match)

;; --------------------------------------------------------

(defsection @util-functions (:title "Utility functions")
  "The CL-STRING-MATCH library also provides a number of functions
that while being potentially useful are not exactly string matching
functions."

  (prefixed-with function)
  (suffixed-with function)
  (same-prefix macro)
  (fib-word function))

;; --------------------------------------------------------

(defun prefixed-with (txt pref)
  "Returns T if the given string `TXT` is prefixed (starts with) the
given prefix `PREF`."
  (when (>= (length txt) (length pref))
    (string= txt pref :end1 (length pref))))

;; --------------------------------------------------------

(defun suffixed-with (txt suff)
  "Returns T if the given string `TXT` is suffixed (ends with) the
given suffix `SUFF`."
  (when (>= (length txt) (length suff))
    (string= txt suff :start1 (- (length txt) (length suff)))))

;; --------------------------------------------------------

(defmacro same-prefix (str1 str2 prefix-length)
  "Generates a logical expression that evaluates to T when the first
PREFIX-LENGTH characters of the two given strings are the same.

The same could be achieved with the STRING= function, but the
generated code differs in that it is an unrolled loop and therefore
might give a performance gain compared to a more general function.

The macro does not attempt to save given strings to variables,
therefore it works best when it is given two string variables, and not
string literals.

Example usage:

```lisp
(defconstant +time-stamp-length+ 5)
(same-prefix prev-line next-line #.+time-stamp-length+)
```

will expand into:

```lisp
(AND (>= (LENGTH PREV-LINE) 5) (>= (LENGTH NEXT-LINE) 5)
     (CHAR= (CHAR PREV-LINE 0) (CHAR NEXT-LINE 0))
     (CHAR= (CHAR PREV-LINE 1) (CHAR NEXT-LINE 1))
     (CHAR= (CHAR PREV-LINE 2) (CHAR NEXT-LINE 2))
     (CHAR= (CHAR PREV-LINE 3) (CHAR NEXT-LINE 3))
     (CHAR= (CHAR PREV-LINE 4) (CHAR NEXT-LINE 4)))
```

"

  `(and (>= (length ,str1) ,prefix-length)
        (>= (length ,str2) ,prefix-length)
        ,@(iter (for i from 0 below prefix-length)
                (collect `(char= (char ,str1 ,i)
                                 (char ,str2 ,i))))))

;; (same-prefix prev-line next-line 5)

;; --------------------------------------------------------

;;; Fibonacci word

(defun fib-word (n s0 s1)
  "A simple generation of Fibonacci strings (or words).

A Fibonacci word is the concatenation of the previous sequence and the
one before that. Or, S_n = S_{n-1} U S_{n-2}.

This function provides a very simple and straightforward
implementation without memoization or any kind of optimization.

N should be equal 2 or greater as there is really no way to compute a
Fibonacci word of smaller order."

  (cond
    ((= n 0) s0)
    ((= n 1) s1)
    (t
     (let ((s-n ""))
       (iter
         (with s-n2 = s0)
         (with s-n1 = s1)
         (for i from 1 below n)
         (setf s-n (concatenate 'string s-n1 s-n2))
         (setf s-n2 s-n1
               s-n1 s-n))
       s-n))))

;; EOF
