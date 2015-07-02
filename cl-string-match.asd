;;; -*- package: CLSTRINGMATCH.SYSTEM; Syntax: Common-lisp; Base: 10 -*-

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clstringmatch.system)
    (defpackage :clstringmatch.system
      (:use :common-lisp :asdf))))

;; --------------------------------------------------------

(in-package :clstringmatch.system)

;; --------------------------------------------------------

(asdf:defsystem #:cl-string-match
  :description
  "Provides implementations of the standard sub-string search (string
matching) algorithms: brute-force, Boyer-Moore, Rabin-Karp, etc."
  :license "BSD"
  :author "Vityok https://bitbucket.org/vityok"
  :version "2015.6.4"
  :depends-on (:alexandria :ascii-strings
			   :yacc	; Portable RE
			   :jpl-queues	; Aho-Corasick
			   :iterate	; at least Aho-Corasick
			   )
  :components ((:module "src"
			:serial T
			:components
			((:file "package")
			 (:file "brute-force")
			 (:file "boyer-moore")
			 (:file "boyer-moore-horspool")
			 (:file "rabin-karp")
			 (:file "knuth-morris-pratt")
			 (:file "aho-corasick")
			 (:file "suffix-tree")
			 (:file "pre"))))
  :in-order-to ((test-op (load-op cl-string-match-test))))

;; EOF
