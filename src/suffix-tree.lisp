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

;;; Suffix tree data structure and Ukkonen's algorithm implementation
;;; for their construction in linear time
;;;
;;; TODO

;; Based on description in StackOverflow question:
;;
;; http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
;;
;; And article Data Structures, Algorithms, & Applications in Java. Suffix Trees. by Sartaj Sahni
;;
;; http://www.cise.ufl.edu/~sahni/dsaaj/enrich/c16/suffix.htm

(in-package :cl-string-match)

;; --------------------------------------------------------

;; uncomment the following line to enable debug output
;; (push :debug-simple-stree *features*)

;; --------------------------------------------------------

(defstruct (suffix-tree (:conc-name suffix-tree.)
			(:print-function suffix-tree-printer))
  "ROOT is a SUFFIX-NODE with default values. It is intended to keep
references to its children. ROOT is just a simple placeholder for the
actual nodes as its children."

  (str "" :type simple-string)
  (root)
  (nodes-count 0 :type fixnum))

;; --------------------------------------------------------

(defun suffix-tree-printer (obj stream depth)
  (declare (ignore depth)
	   (type suffix-tree obj))

  (format stream "#<suffix-tree str: ~s children:~%  ~{ ~a~% ~}>"
	  (suffix-tree.str obj)
	  (suffix-node.children (suffix-tree.root obj))))

;; --------------------------------------------------------

(defstruct (suffix-node (:conc-name suffix-node.)
			(:print-function suffix-node-printer))
  "Documentation"
  (start 0 :type fixnum)
  (end   0 :type fixnum)
  (parent nil)
  (children nil)
  (id    0 :type fixnum))

;; --------------------------------------------------------

(defun suffix-node-printer (obj stream depth)
  (declare (ignore depth)
	   (type suffix-node obj))

  (format stream "#<node start: ~a; end: ~a; children: ~{~a~} >"
	  (suffix-node.start obj)
	  (suffix-node.end obj)
	  (suffix-node.children obj)))

;; --------------------------------------------------------

(defun print-suffix-tree-for-gv (tree &key (stream *standard-output*) (label nil))
  "Print the given suffix TREE for the Graphviz visualization in the
dot format."

  (labels ((PRINT-NODE (node)
	     (dolist (child (suffix-node.children node))
	       (format stream "n~a [label=\"[~a;~a)\" ];~% n~a-> n~a [ label=~S ];~%"
		       (suffix-node.id node)
		       (suffix-node.start node)
		       (suffix-node.end node)
		       (suffix-node.id node)
		       (suffix-node.id child)
		       (subseq (suffix-tree.str tree)
			       (suffix-node.start child)
			       (suffix-node.end child)))
	       (print-node child))))
    (format stream "digraph suffix_tree {~%")
    (print-node (suffix-tree.root tree))
    (when label
      (format stream "label=~S~%" label))
    (format stream "~&}~%")))

;; --------------------------------------------------------
;; ALGORITHM IMPLEMENTATIONS
;; --------------------------------------------------------

(defun add-child-node (tree node start end)
  "Adds a child node to the given NODE."

  (declare (type suffix-tree tree)
	   (type suffix-node node))

  (let ((child-node (make-suffix-node :start start
				      :end end
				      :parent node
				      :id (incf (suffix-tree.nodes-count tree)))))
    (push child-node (suffix-node.children node))))

;; --------------------------------------------------------

(defun find-child-node-by-char (tree node c)
  "Given the NODE lookup a child node that starts with the given char
C."

  #+debug-simple-stree
  (format t "find-child: ~a~%" c)
  (loop :with str = (suffix-tree.str tree)
     :for child :in (suffix-node.children node)
     :when (char= c
		  (char str
			(suffix-node.start child)))
     :return child))

;; --------------------------------------------------------

(defun split-node (tree old-node start end pos)
  "Split the given NODE at position K."
  ;; old-node will be trimmed till position K and will remain
  ;; referenced from its parents. Its children will be inherited by a
  ;; new-node that will keep the part of the prefix that did not match
  ;; with the new prefix

  #+debug-simple-stree
  (format t "split-node: ~a ~a ~a ~a~%" old-node start end pos)
  (let ((new-node (make-suffix-node :start (+ (- pos start)
					      (suffix-node.start old-node))
				    :end (suffix-node.end old-node)
				    :children (suffix-node.children old-node)
				    :id (incf (suffix-tree.nodes-count tree)))))
    (setf (suffix-node.children old-node) (list new-node)
	  (suffix-node.end old-node) (+ (- pos start)
					(suffix-node.start old-node)))
    (add-child-node tree old-node pos end)))

;; --------------------------------------------------------

(defun add-suffix-simple (tree start end)
  "Adds a suffix of a string STR designated by its START and END to
the suffix tree TREE."

  ;; At the first step, S[1..m]$ is inserted in an empty tree and suffix tree T1 is composed of
  ;; a unique node. At step i + 1, suffix S[i + 1..m]$ is inserted in Ti as follows. Traverse the tree
  ;; starting at the root r and find the longest prefix that matches a path of Ti . If such prefix is
  ;; not found, this is because none of the previous suffixes S[j..m]$, for j = 1 to j = i, starts
  ;; by character S[i + 1]. In this case, a new leaf numbered i + 1 with edge-label S[i + 1..m]$ is
  ;; created and joined to the root.

  ;; Thus, assume there is a path such that S[i + 1..m]$ is a prefix of maximal length of
  ;; that path. Because of the presence of the termination character $, the prefix cannot be a
  ;; substring of a suffix entered into the tree early on. Therefore, there is a character at position
  ;; k such that S[i + 1..k]$ is the prefix; let S[k..m]$ be the non-matching substring. Let (u, v)
  ;; be the edge of the path where character S[k] is found. The algorithm creates a new node w
  ;; and a leaf numbered i + 1. Node w splits edge (u, v) into edges (u, w) and (w, v), and edge
  ;; joining w and leaf i + 1 receives edge-label S[i + 1..k]$. The algorithm finishes when the
  ;; termination character is inserted.

  (declare (type fixnum start)
	   (type fixnum end))

  #+debug-simple-stree
  (format t "~a: ADD-SUFFIX-SIMPLE: start=~a end=~a suf=~a~%" start start end
	  (subseq (suffix-tree.str tree) start end))
  
  (loop :named outer
     :with current-node = (suffix-tree.root tree)
     :with tree-str = (suffix-tree.str tree)
     :with suffix-pos = start		; position of a current char
     :with child = (find-child-node-by-char tree current-node (char tree-str suffix-pos))
     :while (< suffix-pos end) :do
     (if child
	 ;; find the longest path from the root with label matching
	 ;; the _prefix_ of the given suffix
	 (if (< (- suffix-pos start)
		(- (suffix-node.end child)
		   (suffix-node.start child)))
	     ;; current char is still within the current node,
	     ;; check whether it matches with the path
	     (if (char= (char tree-str suffix-pos)
			(char tree-str (+ (- suffix-pos start)
					  (suffix-node.start child))))
		 ;; the chars match, continue the cycle
		 (progn
		   (incf suffix-pos))
		 ;; chars do not match, split the current node into
		 ;; one that matches, and the one that does not
		 (progn
		   (split-node tree child start end suffix-pos)
		   (return-from outer)))
	     ;; suffix is longer than the current arc, try to find
	     ;; a child node following the path from the current child
	     (progn
	       (setf current-node child
		     child (find-child-node-by-char tree
						    current-node
						    (char tree-str suffix-pos)))))

	 ;; current-node has no appropriate children, prefix path ends here and
	 ;; must be extended to accomodate the suffix
	 (progn
	   (add-child-node tree current-node suffix-pos end)
	   (return-from outer)))))

;; --------------------------------------------------------

(defun build-suffix-tree-simple (str)
  "Build a Suffix tree for the given string STR using naive (brute-force) algorithm.

Naive algorithm takes O(m²) time to build a suffix tree where m is the
given string length.

Essentially it operates as follows:

  while suffices remain:
    add next shortest suffix to the tree

Algorithm first ads a suffix Str[1..m] (entire string) to the
tree. Then it proceeds adding suffices Str[i..m] (i=2..m) to the tree."

  (declare (type simple-string str))
  (let ((tree (make-suffix-tree :str str
				:root (make-suffix-node)))
	(str-len (length str)))
    (loop :for i :from 0 :below str-len :do
       (progn
	 (add-suffix-simple tree i str-len)
	 #+debug-simple-stree
	 (with-open-file (out (format nil "stage_~a.dot" i)
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	   ;; for i in stage_*.dot ; do dot -Tpng -o $i.png $i ; done
	   (print-suffix-tree-for-gv tree
				     :stream out
				     :label (format nil "after adding {~a}"
						    (subseq str i str-len))))))
    tree))

;; --------------------------------------------------------

(defun build-suffix-tree-ukkonen (str)
  "Build a Suffix tree for the given string STR using Ukkonen's
algorithm.

Ukkonen's algorithm takes O(m²) time to build a suffix tree where m is
the given string length.

Ukkonen's algorithm is an on-line algorithm and can operate on a
stream of characters, adding one character at a time.

TODO"
  (declare (type simple-string str))

  )

;; --------------------------------------------------------

(defun build-suffix-tree-mccreight (str)
  "Build a Suffix tree for the given string STR using McCreight's
algorithm.

McCreight's algorithm takes O(m²) time to build a suffix tree where m is
the given string length.

TODO"
  (declare (type simple-string str))

  )

;; EOF
