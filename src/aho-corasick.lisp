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

;;; Aho-Corasick algorithm implementation
;;
;; Based on description in:
;;
;; Biosequence Algorithms, Spring 2005.
;; Lecture 4: Set Matching and Aho-Corasick Algorithm
;; Pekka Kilpelainen. University of Kuopio, Department of Computer Science
;;
;; Downloaded from: http://www.cs.uku.fi/~kilpelai/BSA05/lectures/slides04.pdf
;;
;;
;; Ideas and code for the tries implementation are borrowed from:
;; Jorge Gajon
;; http://gajon.org/trees-linked-lists-common-lisp/

(in-package :cl-string-match)

;; --------------------------------------------------------

(defun make-tree (data)
  "Creates a new node that contains 'data' as its data.

@author{gajon.org}"
  (cons (cons data nil) nil))

;; --------------------------------------------------------

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified.

@author{gajon.org}"
  
  (nconc (car tree) child)
  tree)

;; --------------------------------------------------------

(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
or nil if this node does not have children.

@author{gajon.org}"

  (when (listp tree)
    (cdr (car tree))))

;; --------------------------------------------------------

(defun next-sibling (tree)
  "Returns a reference to the next sibling of the node passed in,
  or nil if this node does not have any siblings.

@author{gajon.org}"
  (cdr tree))

;; --------------------------------------------------------

(defun data (tree)
  "Returns the information contained in this node.

@author{gajon.org}"

  (car (car tree)))

;; --------------------------------------------------------

(defun traverse (tree &optional (padding 0))
  "@author{gajon.org}"

  (when tree
    (format t "~&~v@TData: ~A" padding (data tree))
    (when (first-child tree)
      (format t "  Children: ~A"
              (maplist #'(lambda (x) (data x))
                       (first-child tree))))
    (traverse (first-child tree) (+ padding 3))
    (traverse (next-sibling tree) padding)))

;; --------------------------------------------------------

(defun test-trees ()
  "Expected output:

Data: 1  Children: (2 3 4 5)
   Data: 2  Children: (6 7 8)
      Data: 6
      Data: 7
      Data: 8
   Data: 3
   Data: 4  Children: (9)
      Data: 9  Children: (12)
         Data: 12
   Data: 5  Children: (10 11)
      Data: 10
      Data: 11
 ((1 (2 (6) (7) (8)) (3) (4 (9 (12))) (5 (10) (11))))
"
  (let ((one    (make-tree 1))
	(two    (make-tree 2))
	(four   (make-tree 4))
	(five   (make-tree 5)))
    (add-child one two)
    (add-child one (make-tree 3))
    (add-child one four)
    (add-child one five)
    (add-child two (make-tree 6))
    (add-child two (make-tree 7))
    (add-child two (make-tree 8))
    (add-child four (add-child (make-tree 9) (make-tree 12)))
    (add-child five (make-tree 10))
    (add-child five (make-tree 11))

    ;; Print the contents of the tree,
    (traverse one)
    ;; and return it.
    one))

;; --------------------------------------------------------

(defun build-trie (patterns)
  "Builds a Trie based on the given list of patterns."
  (flet ((EMPTY-TRIE ()
	   (make-tree nil))
	 (ADD-TRIE-NODE (trie label mark)
	   (add-child trie (cons label mark))
	   )
	 (ADD-KEYWORD (trie kw idx)
	   
	   ))
    
    (let ((trie (empty-trie)))
      (loop :for pat :in patterns
	 :count pat :into idx :do
	 (add-keyword trie pat idx)))))

;; --------------------------------------------------------

(defun test-trie ()
  (let ((trie (build-trie '("he" "she" "his" "hers"))))
    (traverse trie)))

;; EOF