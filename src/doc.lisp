;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2017, 2018 Victor Anyakin <anyakinvictor@yahoo.com>
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

;; --------------------------------------------------------
;; Generates API Reference using MGL-PAX library by Gábor Melis
;; <mega@retes.hu>
;; --------------------------------------------------------

(in-package :cl-string-match)

;; --------------------------------------------------------

(defsection @cl-string-match-manual (:title "CL-STRING-MATCH API Reference")
  "CL-STRING-MATCH String and pattern matching library reference.

CL-STRING-MATCH [![Quickdocs](http://quickdocs.org/badge/cl-string-match.svg)](http://quickdocs.org/cl-string-match/) is supported by Quicklisp and is known by its system name:

```lisp
(ql:quickload :cl-string-match)
```

CL-STRING-MATCH exports functions in `cl-string-match` package (that
is also nicknamed as `sm`).

Shortcut functions search given pattern `pat` in text `txt`. They are
usually much slower (because they build index structures every time
they are called) but are easier to use:

* `string-contains-brute` *pat* *txt* — Brute-force
* `string-contains-bm` *pat* *txt* — Boyer-Moore
* `string-contains-bmh` *pat* *txt* — Boyer-Moore-Horspool
* `string-contains-kmp` *pat* *txt* — Knuth-Morris-Pratt
* `string-contains-ac` *pat* *txt* — Aho-Corasick
* `string-contains-rk` *pat* *txt* — Rabin-Karp

A more robust approach is to use pre-calculated index data that is
processed by a pair of `initialize` and `search` functions:

* `initialize-bm` *pat* and `search-bm` *bm* *txt*
* `initialize-bmh` *pat* and `search-bmh` *bm* *txt*
* `initialize-bmh8` *pat* and `search-bmh8` *bm* *txt*
* `initialize-rk` *pat* and `search-rk` *rk* *txt*
* `initialize-kmp` *pat* and `search-kmp` *kmp* *txt*
* `initialize-ac` *pat* and `search-ac` *ac* *txt*. `initialize-ac`
  can accept a list of patterns that are compiled into a trie.

Brute-force algorithm does not use pre-calculated data and therefore
has no \"initialize\" function.

Boyer-Moore-Horspool implementation (the `-BMH` and `-BMH8` functions)
also accepts `:start2` and `:end2` keywords for the \"search\" and
\"contains\" functions.

Following example looks for a given substring *pat* in a given line of
text *txt* using Boyer-Moore-Horspool algorithm implementation:

```lisp
(let ((idx (initialize-bmh \"abc\")))
  (search-bmh idx \"ababcfbgsldkj\"))
```

Counting all matches of a given pattern in a string:

```lisp
(loop with str = \"____abc____abc____ab\"
      with pat = \"abc\"
      with idx = (sm:initialize-bmh8 pat)
      with z = 0 with s = 0 while s do
       (when (setf s (sm:search-bmh8 idx str :start2 s))
	 (incf z) (incf s (length pat)))
     finally (return z))
```

It should be noted that Boyer-Moore-Horspool (`bmh`) implementation
can offer an order of magnitude boost to performance compared to the
standard `search` function.

However, some implementations create a \"jump table\" that can be the
size of the alphabet (over 1M CHAR-CODE-LIMIT on implementations
supporting Unicode) and thus consume a significant chunk of
memory. There are different solutions to this problem and at the
moment a version for the ASCII strings is offered: `initialize-bmh8`
*pat* and `search-bmh8` *bm* *txt* as well as `string-contains-bmh8`
*pat* *txt* work for strings with characters inside the 256 char code
limit.
"
  (cl-string-match asdf:system)
  (@single-pattern-search section)
  (@multi-pattern-search section)
  (@regexp-pattern-search section)
  (@util-functions section))

(defsection @single-pattern-search (:title "Single pattern search")
  "Looking for a single pattern in a string"
  (@brute-force-section section)
  (@boyer-moore-section section)
  (@boyer-moore-horspool-section section)
  (@rabin-karp-section section)
  (@knuth-morris-pratt-section section)
  (@shift-or-section section))

(defsection @multi-pattern-search (:title "Multiple pattern search")
  "Multiple pattern search algorithms look for for multiple patterns
in a string at once."
  (@aho-corasick-section section))

(defsection @regexp-pattern-search (:title "Regular expressions")
  "Parsing and interpreting regular expressions."
  (@pre-regexp-section section))

;; --------------------------------------------------------

(defun update-md ()
  (let ((mgl-pax:*DOCUMENT-LINK-CODE* NIL)
	(mgl-pax:*DOCUMENT-LINK-SECTIONS* NIL))
    (with-open-file (stream (ensure-directories-exist
			     (asdf:system-relative-pathname
			      :cl-string-match "doc/md/sm-manual.md"))
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (document @cl-string-match-manual :stream stream)
      (print-markdown-footer stream))))

(defun print-markdown-footer (stream)
  (format stream "~%* * *~%")
  (format stream "###### \\[generated by ~
                   [MGL-PAX](https://github.com/melisgl/mgl-pax)\\]~%"))
;; (update-md)

#|
For whatever reason Clozure CL produces better results. See:

lx86cl --eval '(ql:quickload :cl-string-match)' --eval '(cl-string-match::update-md)' --eval '(ccl:quit)'

|#
;; EOF
