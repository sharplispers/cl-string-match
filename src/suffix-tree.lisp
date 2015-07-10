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
;; (push :debug-suffix-tree *features*)

;; --------------------------------------------------------

(defstruct (suffix-tree (:print-function suffix-tree-printer))
  "ROOT is a SUFFIX-NODE with default values. It is intended to keep
references to its children. ROOT is just a simple placeholder for the
actual nodes as its children."

  (str "" :type simple-string)
  (root)
  (nodes-count 0 :type fixnum))

;; --------------------------------------------------------

(defstruct (suffix-node (:print-function suffix-node-printer))
  "Documentation"
  (start 0 :type fixnum)
  (end   0 :type fixnum)
  (parent   nil)
  (children nil)
  ;; suffix
  (id    0 :type fixnum))

;; --------------------------------------------------------

(defun suffix-tree-printer (obj stream depth)
  (declare (ignore depth)
	   (type suffix-tree obj))

  (format stream "#<suffix-tree str: ~s has-root: ~a children:~%  ~{ ~a~% ~}>"
	  (suffix-tree-str obj)
          (not (null (suffix-tree-root obj)))
	  (suffix-node-children (suffix-tree-root obj))))

;; --------------------------------------------------------

(defun suffix-node-printer (obj stream depth)
  (declare (ignore depth)
	   (type suffix-node obj))
  (format stream "#<node ~a start: ~a; end: ~a; children: ~{~a~} >"
          (suffix-node-id obj)
	  (suffix-node-start obj)
	  (suffix-node-end obj)
	  (suffix-node-children obj)))

;; --------------------------------------------------------

(defun suffix-tree-char (tree i)
  (declare (type suffix-tree tree))
  "Return the char that is located in the string at index i."
  (char (suffix-tree-str tree) i))

;; --------------------------------------------------------

(defun suffix-tree-walk (tree function)
  "Applies the given FUNCTION to every node in the given TREE."
  (labels ((walker (node)
             (funcall function node)
             (suffix-node-map-over-children node #'walker)))
    (walker (suffix-tree-root tree))))

;; --------------------------------------------------------

(defun suffix-tree-equals (tree-a tree-b)
  "Checks all the nodes in the given trees and returns T if trees are
  equal or NIL otherwise."

  (suffix-node-equals (suffix-tree-root tree-a)
                      (suffix-tree-root tree-b)))

;; --------------------------------------------------------

(defun suffix-node-equals (node-a node-b)
  (let ((result (or (and (null node-a)
			 (null node-b))
		    (and (= (suffix-node-start node-a)
			    (suffix-node-start node-b))
			 (= (suffix-node-end node-a)
			    (suffix-node-end node-b))
			 (tree-equal (sort (copy-seq (suffix-node-children node-a)) #'< :key #'suffix-node-start)
				     (sort (copy-seq (suffix-node-children node-b)) #'< :key #'suffix-node-start)
				     :test #'suffix-node-equals)))))
    #+debug-suffix-tree
    (unless result
      (format t "nodes: ~a and ~a mismatch~%" node-a node-b))
    result))

;; --------------------------------------------------------

(defun suffix-tree-build-from-sexp (str sexp)
  "Build the suffix tree from the given sexp representation. Sexp is
in form (begin end (children))."

  (let ((tree (make-suffix-tree :str str :root (make-ukk-node))))
    (labels ((convert-node (expr)
               (make-ukk-node :start (first expr)
                              :end (second expr)
                              :children (map 'list #'convert-node (caddr expr))
                              :id (incf (suffix-tree-nodes-count tree)))))
      (setf (suffix-node-children (suffix-tree-root tree))
            (map 'list #'convert-node sexp)))
    tree))

;; --------------------------------------------------------

(defun suffix-node-leafp (node)
  "Retruns T is the given node is a leaf (has no children), return NIL
otherwise."
  (= (length (suffix-node-children node)) 0))

;; --------------------------------------------------------

(defun suffix-node-map-over-children (node function)
  "Applies the given function to every child of the given node."
  (map nil function (suffix-node-children node)))

;; --------------------------------------------------------

(defun suffix-node-str (tree node)
  "Returns the string that corresponds to the edge pointing to this
node."
  (declare (type suffix-tree tree)
           (type suffix-node node))

  #+debug-suffix-tree
  (when (< (suffix-node-end node)
           (suffix-node-start node))
    (error "invalid node: ~a~%" node))

  (subseq (suffix-tree-str tree)
          (suffix-node-start node)
          (min (+ (suffix-node-end node) 1)
               (length (suffix-tree-str tree)))))

;; --------------------------------------------------------

(defun print-suffix-tree-for-gv (tree &key (stream *standard-output*) (label nil))
  "Print the given suffix TREE for the Graphviz visualization in the
dot format."

  (labels ((PRINT-NODE (node)
	     (dolist (child (suffix-node-children node))
	       (format stream "n~a [label=\"[~a;~a)\" ];~% n~a-> n~a [ label=~S ];~%"
		       (suffix-node-id node)
		       (suffix-node-start node)
		       (suffix-node-end node)
		       (suffix-node-id node)
		       (suffix-node-id child)
		       (suffix-node-str tree child))
	       (format stream "n~a [label=\"[~a;~a)\"];~%"
		       (suffix-node-id child)
		       (suffix-node-start child)
		       (suffix-node-end child))
	       (print-node child))))
    (format stream "digraph suffix_tree {~%")
    (print-node (suffix-tree-root tree))
    (when label
      (format stream "label=~S~%" label))
    (format stream "~&}~%")))

;; --------------------------------------------------------

(defun print-suffix-tree-for-gv-pretty (tree &key (stream *standard-output*) (label nil))
  "Alternative representation of the suffix tree. Based on java
implementation from: http://pastie.org/5925812"

  (format stream "digraph {~%")
  (format stream "rankdir = LR;~%")
  (format stream "edge [arrowsize=0.4,fontsize=10];~%")
  (suffix-tree-walk tree
                    #'(lambda (node)
                        (if (suffix-node-leafp node)
                            ;; print leaf node
                            (format stream "node~a [label=\"\",shape=point];~%" (suffix-node-id node))
                            ;; print internal node
                            (progn
                              (format stream "node~a [label=\"\",style=filled,fillcolor=lightgrey,shape=circle,width=.07,height=.07];~%" (suffix-node-id node))
                              (suffix-node-map-over-children node
                                                             #'(lambda (child)
                                                                 (format stream "node~a -> node~a [label=\"~a (~a, ~a)\",weight=3];~%"
                                                                         (suffix-node-id node)
                                                                         (suffix-node-id child)
                                                                         (suffix-node-str tree child)
                                                                         (suffix-node-start child)
                                                                         (if (= (suffix-node-end child) +infinity+)
									     "w"
									     (suffix-node-end child)))))))
                        (when (and (ukk-node-p node)
				   (ukk-node-suffix node))
                          ;; suffix link connection
                          (format stream "node~a -> node~a [label=\"\",weight=1,style=dotted]"
                                  (suffix-node-id node)
                                  (suffix-node-id (ukk-node-suffix node))))))
  (when label
    (format stream "label=~S~%" label))
  (format stream "}~%"))

;; --------------------------------------------------------

(defun print-suffix-tree-in-file (tree fname &optional (label ""))
  (with-open-file (out fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    ;; for i in stage_*.dot ; do dot -Tpng -o $i.png $i ; done
    (print-suffix-tree-for-gv-pretty tree
                                     :stream out
                                     :label label)))

;; --------------------------------------------------------
;; ALGORITHM IMPLEMENTATIONS
;; --------------------------------------------------------

(defun suffix-node-add-child (tree node start end)
  "Adds a child node to the given NODE. Depending on the type of the
given node creates either Ukkonens suffix tree or simple suffix tree
node."

  (declare (type suffix-tree tree)
	   (type suffix-node node))

  (let ((child-node (if (ukk-node-p node)
                        (make-ukk-node :start start
                                       :end end
                                       :parent node
                                       :id (incf (suffix-tree-nodes-count tree)))
                        (make-suffix-node :start start
                                          :end end
                                          :parent node
                                          :id (incf (suffix-tree-nodes-count tree))))))
    (suffix-node-insert-child tree node child-node)
    child-node))

;; --------------------------------------------------------

(defun suffix-node-insert-child (tree node child-node)
  "Adds a child node to the given NODE."

  (declare (type suffix-tree tree)
	   (type suffix-node node)
	   (type suffix-node child-node))

  (setf (suffix-node-children node)
	(remove child-node (suffix-node-children node)
		:test #'(lambda (x y)
			  (char= (suffix-tree-char tree (suffix-node-start x))
				 (suffix-tree-char tree (suffix-node-start y))))))

  #+debug-suffix-tree
  (when (< (suffix-node-end child-node)
           (suffix-node-start child-node))
    (error "inserting invalid node"))

  (push child-node (suffix-node-children node)))

;; --------------------------------------------------------

(defun suffix-node-get-child (tree node c)
  "Given the NODE lookup a child node that starts with the given char
C in the given suffix tree TREE."

  (loop :with str = (suffix-tree-str tree)
     :for child :in (suffix-node-children node)
     :when (char= c
		  (char str (suffix-node-start child)))
     :return child))

;; --------------------------------------------------------
;;
;; SIMPLE SUFFIX TREE CONSTRUCTION ALGORITHM IMPLEMENTATION
;;
;; --------------------------------------------------------

(defun split-node (tree old-node start end pos)
  "Split the given NODE at position POS within the node."
  ;; old-node will be trimmed till position K and will remain
  ;; referenced from its parents. Its children will be inherited by a
  ;; new-node that will keep the part of the prefix that did not match
  ;; with the new prefix

  #+debug-suffix-tree
  (format t "split-node: ~a ~a~%" old-node pos)
  (let ((new-node (make-suffix-node :start (+ (suffix-node-start old-node)
                                              pos)
                                    :end (suffix-node-end old-node)
                                    :children (suffix-node-children old-node)
                                    :id (incf (suffix-tree-nodes-count tree)))))
    (setf (suffix-node-children old-node) (list new-node)
	  (suffix-node-end old-node) (+ (suffix-node-start old-node)
                                        (- pos 1)))
    (suffix-node-add-child tree old-node start end)))

;; --------------------------------------------------------

(defun add-suffix-simple (tree start end)
  "Adds a suffix of a string STR designated by its START and END to
the suffix tree TREE."

  (declare (type suffix-tree tree)
	   (type fixnum start)
	   (type fixnum end))

  #+debug-suffix-tree
  (format t "~a: ADD-SUFFIX-SIMPLE: start=~a end=~a suf=~a~%" start start end
	  (subseq (suffix-tree-str tree) start end))

  (loop :named outer
     :with current-node = (suffix-tree-root tree)
     :with tree-str = (suffix-tree-str tree)
     :with suffix-pos = start		; position of a current char
     :with child = (suffix-node-get-child tree current-node (char tree-str suffix-pos))
     :with child-pos = 0
     :while (< suffix-pos end) :do
     (if child
	 ;; then: find the longest path from the root with label
	 ;; matching the _prefix_ of the given suffix
	 (if (< (- suffix-pos start)
                (str-length child))
	     ;; current char is still within the current node,
	     ;; check whether it matches with the path
	     (if (char= (char tree-str suffix-pos)
			(char tree-str (+ (suffix-node-start child)
                                          child-pos)))
		 ;; the chars match, continue the cycle
		 (progn
                   (incf child-pos)
		   (incf suffix-pos))
		 ;; chars do not match, split the current node into
		 ;; one that matches, and the one that does not
		 (progn
		   (split-node tree child suffix-pos end child-pos)
		   (return-from outer)))
	     ;; suffix is longer than the current arc, try to find
	     ;; a child node following the path from the current child
	     (progn
	       (setf current-node child
		     child (suffix-node-get-child tree
                                                  current-node
                                                  (char tree-str suffix-pos))
                     child-pos 0)))

	 ;; else: current-node has no appropriate children, prefix
	 ;; path ends here and must be extended to accomodate the
	 ;; suffix
	 (progn
	   (suffix-node-add-child tree current-node suffix-pos end)
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
	 #+debug-suffix-tree
	 (progn
	   (format t "complete stage ~a~%" i)
	   (print-suffix-tree-in-file tree
				      (format nil "stage_~a.dot" i)
				      (format nil "Simple: added {~a}"
					      (subseq str i str-len))))))
    tree))

;; --------------------------------------------------------
;;
;; UKKONEN'S ALGORITHM IMPLEMENTATION
;;
;; --------------------------------------------------------

(defstruct (ukk-node (:include suffix-node))
  "Ukkonen's algorithm relies on the suffix link technique. Some other
 algorithms might also rely on it but the naive suffix tree algorithm
 does not require it.

 This structure extends the standard suffix tree node structure with
 the suffix link slot."
  (suffix nil))

;; --------------------------------------------------------

(defun stbstr (str l r)
  "The +infinity+ is defined as the largest number (standard constant
MOST-POSITIVE-FIXNUM), if the right index exceed to the length of the
list, the result is from left to the end of the list."
  (subseq str l (min (1+ r)
		     (length str))))

;; --------------------------------------------------------

(defun ref-length (l r)
  (declare (type fixnum l)
           (type fixnum r))
  (+ (- r l) 1))

;; --------------------------------------------------------

(defun str-length (node)
  "Length of the substring in the arc."
  (declare (type suffix-node node))
  (ref-length (suffix-node-start node)
              (suffix-node-end node)))

;; --------------------------------------------------------

(define-constant +infinity+ MOST-POSITIVE-FIXNUM)

;; --------------------------------------------------------

(defun ukkonen-update (tree node l i)
  #+debug-suffix-tree
  (format t "update: ~a ~a~%" l i)
  (let ((c    (suffix-tree-char tree i)) ;  current char
        (prev (make-ukk-node)))          ;  dummy init
    (loop :named worker :do
       (multiple-value-bind (finish p)
           (branch tree node l (- i 1) c)
         (when finish
           (return-from worker))
         (suffix-node-add-child tree p i +infinity+)
         (setf (ukk-node-suffix prev) p)
         (setf prev p)
         ;;  go up along suffix link
         (multiple-value-setq (node l)
           (canonize tree (ukk-node-suffix node) l (- i 1)))))
    ;; end loop

    (setf (ukk-node-suffix prev) node)
    (return-from ukkonen-update (values node l))))

;; --------------------------------------------------------

(defun branch (tree node l r c)
  "Function branch is used to test if a position is the end point
and turn the implicit node to explicit node if necessary.  Because
sentinel node is not used, the special case is handled in the first
if-clause."

  #+debug-suffix-tree
  (format t "branch: ~a ~a ~a~%" l r c)
  (if (<= (ref-length l r) 0)
      (if (null node)
	  (return-from branch (values T (suffix-tree-root tree)))
	  ;; else
          (return-from branch (values (not (null (suffix-node-get-child tree node c)))
                                      node)))
      ;; else
      (let* ((node1 (suffix-node-get-child tree node (suffix-tree-char tree l)))
	     (l1 (suffix-node-start node1))
	     (r1 (suffix-node-end   node1))
	     (pos (+ l1 (ref-length l r))))

	(if (char= (suffix-tree-char tree pos) c)
	    (return-from branch (values T node))
	    ;; else
	    (progn
              (let ((branch-node (suffix-node-add-child tree node l1 (- pos 1))))

                (setf (suffix-node-start node1) pos)
                (setf (suffix-node-end   node1) r1)
                (suffix-node-insert-child tree branch-node node1)

                (return-from branch (values nil branch-node))))))))

;; --------------------------------------------------------

;; The canonize() function helps to convert a reference pair to
;; canonical reference pair.
(defun canonize (tree node l r)
  #+debug-suffix-tree
  (format t "canonize: ~a ~a ~a~%" (not (null node)) l r)
  (unless node
    (if (<= (ref-length l r) 0)
        (return-from canonize (values nil l))
        ;; else:
        (return-from canonize (canonize tree (suffix-tree-root tree) (1+ l) r))))
  (loop :named worker :while (<= l r) :do ;  str_ref is not empty
     (let* ((child (suffix-node-get-child tree node (suffix-tree-char tree l)))
            (l1 (suffix-node-start child))
            (r1 (suffix-node-end   child)))
       (if (>= (- r l)
               (- r1 l1))
           (progn
             (incf l (+ (- r1 l1) 1))
             (setf node child))
           ;; else
           (return-from worker))))

  (values node l))

;; --------------------------------------------------------

(defun build-suffix-tree-ukkonen (str)
  "Build a Suffix tree for the given string STR using Ukkonen's
algorithm.

Ukkonen's algorithm takes O(m) time to build a suffix tree where m is
the given string length.

Ukkonen's algorithm is an on-line algorithm and can operate on a
stream of characters, adding one character at a time."

  (declare (type simple-string str))

  (let* ((tree (make-suffix-tree :str str
				 :root (make-ukk-node)))
	 (node (suffix-tree-root tree)) ; initial active point
	 (l 0))
    (loop :for i :from 0 :below (length str) :do
       (progn
         #+debug-suffix-tree
         (format t "=====================~%stage: ~a {~a}~%" i (subseq (suffix-tree-str tree) 0 (1+ i)))
         (multiple-value-setq (node l)
           (ukkonen-update tree node l i))
         (multiple-value-setq (node l)
           (canonize tree node l i))
         #+debug-suffix-tree
         (print-suffix-tree-in-file tree
                                    (format nil "stage_~a.dot" i)
                                    (format nil "Ukkonen: added {~a}"
                                            (subseq str 0 (1+ i))))))
    tree))

;; --------------------------------------------------------

(defun build-suffix-tree-mccreight (str)
  "Build a Suffix tree for the given string STR using McCreight's
algorithm.

McCreight's algorithm takes O(n) time to build a suffix tree where n
is the given string length.

TODO"
  (declare (ignore str))

  )

;; EOF
