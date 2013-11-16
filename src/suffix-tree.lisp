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

;; A number of sources were used to implement Ukkonnen's suffix tree algorithm:
;;
;; Description in StackOverflow question:
;; http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
;;
;; Article Data Structures, Algorithms, & Applications in Java. Suffix Trees. by Sartaj Sahni
;; http://www.cise.ufl.edu/~sahni/dsaaj/enrich/c16/suffix.htm
;;
;; Suffix Tree with Functional and imperative implementation. by Liu Xinyu
;; https://sites.google.com/site/algoxy/stree

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

(defun suffix-tree.char (tree i)
  "Return the char that is located in the string at index i."
  (char (suffix-tree.str tree) i))

;; --------------------------------------------------------

(defstruct (suffix-node (:conc-name suffix-node.)
			(:print-function suffix-node-printer))
  "Documentation"
  (start 0 :type fixnum)
  (end   0 :type fixnum)
  (parent   nil)
  (children nil)
  ;; suffix
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

(defun suffix-node.get-child (node c)
  "Return a child of the given node that corresponds to the given char
  c or return nil if there is none."  nil ) ; TODO

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
			       (min (suffix-node.end child)
				    (length (suffix-tree.str tree)))))
	       (format stream "n~a [label=\"[~a;~a)\"];~%"
		       (suffix-node.id child)
		       (suffix-node.start child)
		       (suffix-node.end child))
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
    (push child-node (suffix-node.children node))
    child-node))

;; --------------------------------------------------------

;; TODO: for branch function
(defun insert-child-node (tree node child-node)
  "Adds a child node to the given NODE."

  (declare (type suffix-tree tree)
	   (type suffix-node node)
	   (type suffix-node child-node))

  (push child-node (suffix-node.children node)))

;; --------------------------------------------------------

(defun find-child-node-by-char (tree node c)
  "Given the NODE lookup a child node that starts with the given char
C."

  #+debug-simple-stree
  (format t "find-child: ~a~%" c)
  (loop :with str = (suffix-tree.str tree)
     :for child :in (suffix-node.children node)
     :when (char= c
		  (char str (suffix-node.start child)))
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
				     :label (format nil "додано {~a}"
						    (subseq str i str-len))))))
    tree))

;; --------------------------------------------------------
;;
;; UKKONEN'S ALGORITHM IMPLEMENTATION
;;
;; --------------------------------------------------------

(defstruct (ukk-node (:conc-name ukk-node.)
                     (:include suffix-node))
  "Ukkonen's algorithm relies on the suffix link technique. Some other
 algorithms might also rely on it but the naive suffix tree algorithm
 does not require it.

 This structure extends the standard suffix tree node structure with
 the suffix link slot."
  (suffix nil))

;; --------------------------------------------------------

(defun stbstr (str l r)
  "The infinity is defined as the length of the string plus a big
number, if the right index exceed to the length of the list, the
result is from left to the end of the list."
  (subseq str l (min (1+ r)
		     (length str))))

;; --------------------------------------------------------

(defun str-length (node)
  ;; length of the substring in the arc
  (declare (type suffix-node node))
  (+ (- (suffix-node.end node)
	(suffix-node.start node))
     1))

;; --------------------------------------------------------

(defun ref-length (l r)
  (+ (- r l) 1))

(defconstant infinity 100)

;; --------------------------------------------------------

;; Different with Ukkonen’s original program, I didn’t use sentinel
;; node. The reference passed in is (node,(l,i), the active point is
;; (node,(l,i - 1)) actually, we passed the active point to branch()
;; function. If it is end point, branch() function will return true as
;; the first element in the result. we then terminate the loop
;; immediately. Otherwise, branch() function will return the node
;; which need to branch out a new leaf as the second element in the
;; result. The program then create the new leaf, set it as open pair,
;; and then go up along with suffix link. The prev variable first
;; point to a dummy node, this can simplify the logic, and it used to
;; record the position along the boundary path. by the end of the
;; loop, we’ll finish the last updating of the suffix link and return
;; the end point. Since the end point is always in form of (node, (l,
;; i-1)), only (node, l) is returned.
(defun ukkonen-update (tree node l i)
  #+debug-simple-stree
  (format t "update: ~a; ~a ~a~%" node l i)
  (let ((c    (suffix-tree.char tree i)) ;  current char
        (prev (make-ukk-node)))          ;  dummy init
    (loop named worker do
         (multiple-value-bind (finish p)
             (branch tree node l (- i 1) c)
           (when finish
             (return-from worker))
           (add-child-node tree p i infinity)
           (setf (ukk-node.suffix prev) p)
           (setf prev p)
           ;;  go up along suffix link
           (multiple-value-setq (node l)
             (canonize tree (ukk-node.suffix node) l (- i 1)))))
    ;; end loop

    (setf (ukk-node.suffix prev) node)
    (values node l)))

(defun branch (tree node l r c)
  "Function branch is used to test if a position is the end point
and turn the implicit node to explicit node if necessary.  Because
sentinel node is not used, the special case is handled in the first
if-clause."

  #+debug-simple-stree
  (format t "branch: ~a; ~a ~a ~a~%" node l r c)
  (if (<= (ref-length l r) 0)
      (if node
	  (return-from branch (values (suffix-node.get-child node c) node))
	  ;; else:
	  (return-from branch (values T (suffix-tree.root tree))))
      ;; else:
      (let* ((node1 (suffix-node.get-child node (suffix-tree.char tree l)))
	     (l1 (suffix-node.start node1))
	     (r1 (suffix-node.end   node1))
	     (pos (+ l1 (ref-length l r))))

	(if (char= (suffix-tree.char tree pos) c)
	    (return-from branch (values T node))
	    ;; else:
	    (progn
	      ;; todo make sure that
	      (let ((branch-node (add-child-node tree node l1 (- pos 1))))
		(setf (suffix-node.start node1) pos)
		(setf (suffix-node.end node1) r1)
		(insert-child-node tree branch-node node1)
		(return-from branch (values nil branch-node))))))))

;; --------------------------------------------------------

;; The canonize() function helps to convert a reference pair to
;; canonical reference pair.
(defun canonize (tree node l r)
  #+debug-simple-stree
  (format t "canonize: ~a; ~a ~a~%" node l r)
  (unless node
    (if (<= (ref-length l r) 0)
        (return-from canonize (values nil l))
        ;; else:
        (return-from canonize (canonize tree (suffix-tree.root tree) (1+ l) r))))
  (loop named worker while (<= l r) do	;  str_ref is not empty
       (let* ((child (suffix-node.get-child node (suffix-tree.char tree l)))
              (l1 (suffix-node.start child))
              (r1 (suffix-node.end   child)))
         (if (>= (- r l)
                 (- r1 l1))
             (progn
               (incf l (+ (- r1 l1) 1))
               (setf node child))
             (return-from worker))))
  (values node l))

(defun build-suffix-tree-ukkonen (str)
  "Build a Suffix tree for the given string STR using Ukkonen's
algorithm.

Ukkonen's algorithm takes O(m) time to build a suffix tree where m is
the given string length.

Ukkonen's algorithm is an on-line algorithm and can operate on a
stream of characters, adding one character at a time.

TODO"

  ;; Ukkonen's algorithm builds an implicit suffix tree I_i for each
  ;; prefix S[1..i] of the given string. Based on the implicit suffix
  ;; tree I_m it builds a true suffix tree for string S of length
  ;; m. This whole operation happens in O(m) time.

  ;; The main entry for Ukkonen’s algorithm is implemented as the
  ;; following.

  ;; In the main entry, we initialize the tree and let the node points
  ;; to the root, at this time point, the active point is (root,ε),
  ;; which is (root, (0, -1)) in Python. we pass the active point to
  ;; update() function in a loop from the left most index to the right
  ;; most index of the string. Inside the loop, update() function
  ;; returns the end point, and we need convert it to canonical
  ;; reference pair for the next time update.

  (declare (type simple-string str))

  (let* ((tree (make-suffix-tree :str str
				 :root (make-ukk-node)))
	 (node (suffix-tree.root tree))
	 (l 0))
    (loop :for i :from 0 :below (length str)
       :do (progn
	     (multiple-value-setq (node l)
	       (ukkonen-update tree node l i))
	     (multiple-value-setq (node l)
	       (canonize tree node l i))
	     #+debug-simple-stree
	     (with-open-file (out (format nil "stage_~a.dot" i)
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	       ;; for i in stage_*.dot ; do dot -Tpng -o $i.png $i ; done
	       (print-suffix-tree-for-gv tree
					 :stream out
					 :label (format nil "додано {~a}"
							(subseq str 0 i))))))
    tree))

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
