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

(defpackage :cl-string-match
  (:use :common-lisp :alexandria)
  (:nicknames :sm)
  (:export
   :string-contains-brute
   :string-contains-bm
   :string-contains-bmh
   :string-contains-rk
   :string-contains-kmp
   :string-contains-ac
   :initialize-bm
   :initialize-bmh
   :initialize-rk
   :initialize-kmp
   :initialize-ac
   :search-bm
   :search-bmh
   :search-rk
   :search-kmp
   :search-ac
   ;; Trie operations
   :empty-trie
   :trie-node
   :trie-build
   :trie-add-keyword
   :trie-traverse
   :trie-contains
   ;; Suffix tree
   :+infinity+
   :suffix-tree
   :suffix-tree.root
   :suffix-tree.str
   :make-suffix-tree
   :suffix-tree.char
   :suffix-tree.walk
   :suffix-tree.equals
   :suffix-tree.build-from-sexp
   :suffix-node
   :suffix-node.start
   :suffix-node.end
   :suffix-node.children
   :suffix-node.add-child
   :suffix-node.leafp
   :suffix-node.map-over-children
   :suffix-node.str
   :suffix-node.equals
   :ukk-node
   :make-ukk-node
   :build-suffix-tree-simple
   :build-suffix-tree-ukkonen))

;; --------------------------------------------------------

(in-package :cl-string-match)

;; got idea from CL-PPCRE. Need to place it here so that the Lisp
;; reader will have an idea about this variable when parsing package
;; sources
(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")

;; EOF
