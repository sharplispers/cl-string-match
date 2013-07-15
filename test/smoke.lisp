;;; -*- package: CL-STRING-MATCH-TEST; Syntax: Common-lisp; Base: 10 -*-

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

;; Running tests from the command line:
;; 
;; sbcl --load smoke.lisp --quit
;; lx86cl --load smoke.lisp --eval '(quit)'

(ql:quickload :cl-string-match)
(ql:quickload :lisp-unit)

;; --------------------------------------------------------

(defpackage :cl-string-match-test
  (:use :common-lisp :cl-string-match :lisp-unit))

(in-package :cl-string-match-test)

(setq *print-failures* t)

;; --------------------------------------------------------

(defparameter *funcs*
  '(sm:string-contains-brute
    sm:string-contains-bm
    sm:string-contains-bmh
    sm:string-contains-rk
    sm:string-contains-kmp
    sm:string-contains-ac))

;; --------------------------------------------------------

(defmacro run-assertions (val needle haystack)
  `(progn ,@(loop :for func :in *funcs*
	       :collect `(assert-equal ,val (,func ,needle ,haystack)))))

;; --------------------------------------------------------

(define-test basic-test
  (run-assertions 0 "a" "a--")
  (run-assertions 1 "a" "-a-")
  (run-assertions 2 "a" "--a")
  (run-assertions nil "a" "-b-"))

;; --------------------------------------------------------

(define-test str-test
  (run-assertions 0 "abc" "abcab_")
  (run-assertions 1 "abc" "_abcab_")
  (run-assertions 2 "abc" "ababc"))

;; --------------------------------------------------------

(define-test ac-test
    ;; test Aho-Corasick implementation how it deals with multiple
    ;; patterns search

    (let ((trie (initialize-ac '("he" "she" "his" "hers"))))
      (assert-equal 0 (search-ac trie "she"))
      (assert-equal 1 (search-ac trie "_she"))
      (assert-equal nil (search-ac trie "_sh_"))
      
      (multiple-value-bind (pos idx)
	  (search-ac trie "___his")
	(assert-equal 3 pos)
	(assert-equal 2 idx))
      (multiple-value-bind (pos idx)
	  (search-ac trie "___h_s")
	(assert-equal nil pos)
	(assert-equal nil idx))))
    
;; --------------------------------------------------------

(run-tests :all)

;; EOF