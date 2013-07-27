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
;; TODO: check if the algorithm is really implemented like it must be,
;; including skip distances and proper trie construction as it is a
;; little bit more simple now than described in different books

(in-package :cl-string-match)

;; --------------------------------------------------------

(defstruct (trie-node)
  "Each node of a trie contains a list of child nodes, a label (the
letter) and a mark (some value attributed to the matching string)."

  children label mark)

;; --------------------------------------------------------

(defun trie-add-child (trie label mark)
  "Add a child node to the given node with the given label and mark."

  (declare #.*standard-optimize-settings*)

  (let ((child (make-trie-node :label label
			       :mark mark
			       :children (make-hash-table))))
    (setf (gethash label (trie-node-children trie))
	  child)
    child))

;; --------------------------------------------------------

(defun trie-add-keyword (trie kw idx)
  ;; Starting at the root, follow the path labeled by chars
  ;; of Pi
  ;;
  ;; If the path ends before Pi, continue it by adding new
  ;; edges and nodes for the remaining characters of Pi
  ;;
  ;; Store identifier i of Pi at the terminal node of the
  ;; path
  (declare (type simple-string kw)
	   #.*standard-optimize-settings*)
  (loop
     :with node = trie
     :for c :across kw
     :for child-node = (trie-find-child node c)
     :do (if (null child-node)
	     ;; found a place where the path ends, add new node here
	     (setf node (trie-add-child node c nil))
	     ;; the path continues further
	     (setf node child-node))
     ;; store the keyword index
     :finally (setf (trie-node-mark node) idx)))

;; --------------------------------------------------------

(defun trie-traverse (trie &optional (padding 0) stream)
  "Traverse the given trie and pretty-print it to the given stream."

  (declare #.*standard-optimize-settings*)

  (when trie
    (format stream "~&~v@TData: ~A ~A" padding (trie-node-label trie)
	    (trie-node-mark trie))
    (when (trie-node-children trie)
      (format stream "~&~v@T  Children: ~%" padding)
      (maphash #'(lambda (key val)
		   (trie-traverse val (+ padding 3) stream))
	       (trie-node-children trie)))
    (format stream "~%")))

;; --------------------------------------------------------

(defun trie-find-child (trie label)
  "Looks for a child of the given node trie with the given label."

  (declare #.*standard-optimize-settings*)
  (gethash label (trie-node-children trie)))

;; --------------------------------------------------------

(defun empty-trie ()
  "Creates a new instance and returns an empty trie."

  (declare #.*standard-optimize-settings*)
  (make-trie-node :children (make-hash-table) :label nil :mark nil))

;; --------------------------------------------------------

(defun trie-contains (trie s)
  "Returns T if the given Trie contains the given string."

  (declare #.*standard-optimize-settings*)
  (loop
     :for c :across s
     :for node = trie :then (trie-find-child node c)
     :while node
     :finally (return
		(when node
		  (trie-node-mark node)))))

;; --------------------------------------------------------

(defun trie-build (patterns)
  "Builds a Trie based on the given list of patterns."
  (declare (type list patterns)
	   #.*standard-optimize-settings*)

  (let ((trie (empty-trie)))
    (loop :for pat :in patterns
       :count pat :into idx :do
       (progn
	 (trie-add-keyword trie pat (- idx 1))))
    trie))

;; --------------------------------------------------------

(defun initialize-ac (patterns)
  "Returns a Trie that is used to look for the given patterns in the
text. It can deal either with a single pattern or a list of patterns."

  (declare #.*standard-optimize-settings*)
  (trie-build (if (listp patterns)
		  patterns
		  (list patterns))))

;; --------------------------------------------------------

(defun search-ac (trie txt)
  "Looks for patterns that are indexed in the given trie and returns two values:
start position of the first matching pattern and its index."

  (declare (type simple-string txt)
	   #.*standard-optimize-settings*)
  (loop
     :with i = 0
     :for c :across txt
     :for j :from 0 :below (length txt)
     :for node = (trie-find-child trie c) :then (trie-find-child node c)
     :do (if node
	     (if (trie-node-mark node)
		 ;; if this node has a mark this means that we've
		 ;; reached an end of some pattern
		 (return (values i
				 (trie-node-mark node))))
	     ;; update current node with matching current char
	     (setf node (trie-find-child trie c)
		   ;; and update index of the last non-matching
		   ;; character
		   i (+ j 1)))
     :unless node :do (setf node trie)))

;; --------------------------------------------------------

(defun string-contains-ac (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence."

  (declare #.*standard-optimize-settings*)

  (let ((trie (trie-build (list pat))))
    (loop
       :for c :across txt
       :for j :from 1 :to (length txt)
       :for node = (trie-find-child trie c) :then (trie-find-child node c)
       :do (if node
	       (if (trie-node-mark node)
		   (return (- j (length pat))))
	       (setf node (trie-find-child trie c)))
       :unless node :do (setf node trie))))

;; EOF
