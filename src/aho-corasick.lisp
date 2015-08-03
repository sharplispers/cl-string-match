;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2015, Victor Anyakin <anyakinvictor@yahoo.com>
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

(defstruct (trie-node
	     (:print-function trie-node-printer))
  "Each node of a trie contains a list of child nodes, a label (the
letter) and a mark (some value attributed to the matching string)."

  (id 0 :type fixnum) ; numeric node identifier, nodes counter in the root
  children	      ; this is essentially a goto transition
  label		      ; character attributed to this node
  mark		      ; this is essentially an output function
  (fail nil :type (or null trie-node))	; fail transition
  (depth 0 :type fixnum)
  )

;; --------------------------------------------------------

(defun trie-node-printer (obj stream depth)
  "We have to avoid standard Lisp printer because of the FAIL links
that turn our tree into a network with cycles, plunging the default
printer into an infinite loop."

  (declare (ignore depth))
  (check-type obj trie-node)

  (format stream "#<trie-node ~a label: ~a mark: ~a depth: ~a>"
	  (trie-node-id obj)
	  (trie-node-label obj)
          (trie-node-mark obj)
	  (trie-node-depth obj)))

;; --------------------------------------------------------

(defun trie-add-child (trie label mark &key (id 0) (constructor #'make-trie-node))
  "Add a child node to the given node with the given label and mark.

Constructor can be either MAKE-TRIE-NODE or any other structure
constructor derieved from the TRIE-NODE struct."

  (declare #.*standard-optimize-settings*
	   (function constructor))

  (let ((child (funcall constructor
			:id id
			:label label
			:mark mark
			:depth (+ (trie-node-depth trie) 1)
			:children (make-hash-table))))
    (setf (gethash label (trie-node-children trie))
	  child)
    child))

;; --------------------------------------------------------

(defun trie-add-keyword (trie kw idx &key (constructor #'make-trie-node))
  ;; Starting at the root, follow the path labeled by chars
  ;; of Pi
  ;;
  ;; If the path ends before Pi, continue it by adding new
  ;; edges and nodes for the remaining characters of Pi
  ;;
  ;; Store identifier i of Pi at the terminal node of the
  ;; path
  (declare #.*standard-optimize-settings*)
  (check-type kw simple-string)
  (loop
     :with node = trie
     :for c :across kw
     :for child-node = (trie-find-child node c)
     :do (if (null child-node)
	     ;; found a place where the path ends, add new node here
	     (setf node (trie-add-child node c nil
					:id (trie-node-id trie)
					:constructor constructor)
		   (trie-node-id trie) (+ 1 (trie-node-id trie)))
	     ;; the path continues further
	     (setf node child-node))
     ;; store the keyword index
     :finally (setf (trie-node-mark node) idx)))

;; --------------------------------------------------------

(defmacro map-trie-children ((parent child) &body body)
  "Perform the given BODY on the children of the given PARENT. The
child node is bound to the CHILD variable."
  (let ((key (gensym)))
    `(maphash
      #'(lambda (,key ,child)
	  (declare (ignore ,key))
	  ,@body)
      (trie-node-children ,parent))))

;; --------------------------------------------------------

(defun trie-traverse-dfo (trie handler)
  "Traverse the trie in the Depth-First-Order and call the given
handler function on each node.."
  (check-type handler function)

  (let ((stack nil))
    (push trie stack)
    (iter
      (while stack)
      (for node = (pop stack))
      (funcall handler node)
      (map-trie-children (node child)
	(push child stack)))))

;; --------------------------------------------------------

(defun trie-traverse-bfo (trie handler)
  "Traverse the trie in the Breadth-First-Order and call the given
handler function on each node.."
  (check-type handler function)

  (let ((queue (make-instance 'jpl-queues:unbounded-fifo-queue)))
    (jpl-queues:enqueue trie queue)
    (iter
      (until (jpl-queues:empty? queue))
      (for node = (jpl-queues:dequeue queue))
      (funcall handler node)
      (map-trie-children (node child)
	(jpl-queues:enqueue child queue)))))

;; --------------------------------------------------------

(defun trie-dump-dot (trie &key (stream *standard-output*))
  "Dumps a textual representation of the Trie useful for making a
graphical plot with Graphviz to the given stream."

  (format stream "digraph finite_state_machine {
rankdir=LR;
size=\"8,5\"
node [shape = circle];~%")

  (trie-traverse-bfo
   trie
   #'(lambda (node)
       (if (trie-node-mark node)
	   (format stream "nd_~a [label=\"~a\",shape=doublecircle];~%" (trie-node-id node) (trie-node-id node))
	   (format stream "nd_~a [label=\"~a\",shape=circle];~%" (trie-node-id node) (trie-node-id node)))
       (when (trie-node-fail node)
	 (format stream "nd_~a -> nd_~a [style=dashed,weight=1,arrowhead=open,constraint=false,penwidth=0.6];~%"
		 (trie-node-id node)
		 (trie-node-id (trie-node-fail node))))
       (map-trie-children (node child)
	 (format stream "nd_~a -> nd_~a [label=\"~a\"];~%"
		 (trie-node-id node)
		 (trie-node-id child)
		 (trie-node-label child)))))

  (format stream "}~%"))

;; --------------------------------------------------------

(defun trie-dump-dot-file (trie fname)
  "Dumps output of the TRIE-DUMP-DOT function to the file with the
specified file name."

  (with-open-file (out fname
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (trie-dump-dot trie :stream out)))

;; --------------------------------------------------------

(defun trie-pprint (trie &key (padding 0) root (stream *standard-output*))
  "Traverse the given trie and pretty-print it to the given stream.

ROOT is the root node (optional)."
  ;; (declare #.*standard-optimize-settings*)
  (when trie
    (format stream "~&~v@TNode[~A] ~A ~A; depth: [~a] fail: [~a]"
	    padding
	    (trie-node-id trie)
	    (trie-node-label trie)
	    (trie-node-mark trie)
	    (trie-node-depth trie)
	    (if (eq (trie-node-fail trie) root)
		"ROOT"
		(if (eq trie (trie-node-fail trie))
		    "SELF" ; we might not have the root node initially
		    (if (trie-node-fail trie)
			(trie-node-id (trie-node-fail trie))
			"NONE"))))
    (when (trie-node-children trie)
      (maphash #'(lambda (key val)
		   (declare (ignore key))
		   (trie-pprint val
				:padding (+ padding 3)
				:stream stream
				:root (if root root trie)))
	       (trie-node-children trie)))))

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
  "Returns T if the given TRIE contains the given string S."

  (declare #.*standard-optimize-settings*)
  (loop
     :for c :across s
     :for node := (trie-find-child trie c) :then (trie-find-child node c)
     :while node
     :finally (return
                (when node
                  (trie-node-mark node)))))

;; --------------------------------------------------------

(defun trie-contains-prefix (trie s)
  "Checks if the given TRIE contains some prefix of the given string S.

Returns the length of the matched prefix."

  (declare #.*standard-optimize-settings*)
  (loop
     :with matched
     :for depth :from 0
     :for c :across s
     :for node := (trie-find-child trie c) :then (trie-find-child node c)
     :while node
     :when (trie-node-mark node) :do (setf matched (1+ depth))
     :finally (return matched)))

;; --------------------------------------------------------

(defun trie-build (patterns &key (constructor #'make-trie-node))
  "Builds a Trie based on the given list of patterns."
  (declare #.*standard-optimize-settings*)
  (check-type patterns list)

  (let ((trie (empty-trie)))
    (loop :for pat :in patterns
       :count pat :into idx :do
       (progn
	 (trie-add-keyword trie pat (- idx 1) :constructor constructor)))
    trie))

;; --------------------------------------------------------

(defun compute-failure-function (trie)
  "Given a trie calculate failure transitions for its nodes.

Modifies nodes.

Traverses the trie in the breadth-first-order (BFO)"

  (declare #.*standard-optimize-settings*)

  (let ((queue (make-instance 'jpl-queues:unbounded-fifo-queue))
	(root trie))

    ;; root fails to itself
    (setf (trie-node-fail root) root)

    ;; immediate children of the root also fail to the root
    (map-trie-children (root child)
      (setf (trie-node-fail child) root)
      (jpl-queues:enqueue child queue))

    ;; deal with the rest: the main while loop computes the set of
    ;; states of depth d from the set of states of depth d-1
    (iter
      (until (jpl-queues:empty? queue))
      ;; let r be the next state in queue
      (for r = (jpl-queues:dequeue queue))
      ;; map children of this r
      (map-trie-children (r child)
	;; child is S
	(let* ((state (trie-node-fail r))  ; state <- f(r),
	       (a (trie-node-label child)) ; g(r,a)=s
	       ;; now we can deal with the fail transition:
	       ;; g(state,a)=fail. however, original algorithm relies on
	       ;; the fact that the root node goes forward to the root
	       ;; node in cycles for every char that is not its
	       ;; child. This is feasible if forward transitions/child
	       ;; nodes are stored in a fixed table but with large
	       ;; alphabets/hashmaps/treemaps this is not so
	       ;; easy. Therefore we additionally check if we reached the
	       ;; root and use it as a sort of default fallback node.
	       (fail-node
		(iter
		  (until (trie-find-child state a))
		  ;; state <- f(state)
		  (when (eq state (trie-node-fail state))
		    ;; avoid infinite cycle in the root node
		    (leave state))
		  (setf state (trie-node-fail state))
		  ;; executed only if we are leaving via until
		  (finally (return (trie-find-child state a))))))

	  ;; to preserve BFO: queue <- queue U {s}
	  (jpl-queues:enqueue child queue)
	  ;; f(s) <- g(state, a)
	  (setf (trie-node-fail child)
		fail-node)

	  ;; output(s) <- output(s) U output(f(s))

	  ;; IMPORTANT NOTE: we don't handle concatenation of outputs
	  ;; - our focus is matching just one pattern. Unlike the
	  ;; original algorithm that will output the string that is
	  ;; matched by all patterns.
	  #+ignore
	  (setf (trie-node-mark child)
		(concatenate 'list
			     (trie-node-mark child)
			     (trie-node-mark fail-node))))))

    ;; done, return reference to the trie
    trie))

;; --------------------------------------------------------

;; (initialize-ac '("atatata" "tatat" "acgatat"))
;; (initialize-ac '("announce" "annual" "annually"))

(defun initialize-ac (patterns)
  "Returns a Trie that is used to look for the given patterns in the
text. It can deal either with a single pattern or a list of patterns."

  (declare #.*standard-optimize-settings*)
  (let ((trie (trie-build (if (listp patterns)
			      patterns
			      (list patterns)))))
    (compute-failure-function trie)))

;; --------------------------------------------------------

(defun search-ac (trie txt &key greedy)
  "Looks for patterns that are indexed in the given trie and returns two values:
start position of the first matching pattern and its index."

  (declare #.*standard-optimize-settings*)
  (check-type trie trie-node)
  (check-type txt simple-string)

  (iter
    (with match-len = 0)
    (with node = trie)
    (for c in-string txt)
    (for pos from 0 below (length txt))
    (for new-node first (trie-find-child trie c) then (trie-find-child node c))
    (if new-node
	;; we've got a forward transition
	(progn
	  (setf node new-node)
	  (when (trie-node-mark node)
	    ;; if this node has a mark this means that we've
	    ;; reached an end of some pattern
	    ;; (format t "~a ~a~%" (- pos match-len) (trie-node-mark node))
	    (if greedy
		(collect (list pos (trie-node-mark node)))
		(return (values (- pos match-len) (trie-node-mark node)))))
	  (incf match-len))
	;; perform a fail transition
	(progn
	  (setf node (trie-node-fail node)
		;; and update index of the last non-matching
		;; character
		match-len 0)))))

;; --------------------------------------------------------

(defun string-contains-ac (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence."

  (declare #.*standard-optimize-settings*)
  (check-type pat simple-string)
  (check-type txt simple-string)

  (if (= (length pat) 0)
      0
      (let* ((trie (trie-build (list pat)))
	     (res
	      (loop
		 :for c :across txt
		 :for j :from 1 :to (length txt)
		 :for node = (trie-find-child trie c) :then (trie-find-child node c)
		 :do (if node
			 (if (trie-node-mark node)
			     (return (- j (length pat))))
			 (setf node (trie-find-child trie c)))
		 :unless node :do (setf node trie))))
	(if res res nil))))

;; --------------------------------------------------------
;; TABULAR IMPLEMENTATION OF THE Aho-Corasick DFA
;; --------------------------------------------------------

(define-constant +ac-alphabet+ 256)

(defstruct tabac
  ;; initial state
  (start -1 :type fixnum)
  ;; transitions
  (trans nil :type (or null (simple-array (or null (simple-array fixnum)))))
  ;; final states
  (final (make-array 0 :element-type 'fixnum)
	 :type (simple-array fixnum))
  ;; array that stores corresponding pattern length
  (match-len (make-array 0 :element-type 'fixnum)
	     :type (simple-array fixnum)))

;; --------------------------------------------------------

(defun trie->tabular-ac (trie)
  "Gets a trie and transforms it into a table-based DFA for the
Aho-Corasick automata."

  (let* ((nodes-count (trie-node-id trie))
	 (idx (make-tabac
	       :start nodes-count
	       :match-len (make-array (+ nodes-count 1)
				      :initial-element -1
				      :element-type 'fixnum)
	       :trans (make-array (+ nodes-count 1)
				  :initial-element nil
				  :element-type '(or null (simple-array fixnum)))
	       :final (make-array (+ nodes-count 1)
				  :initial-element -1
				  :element-type 'fixnum))))
    (trie-traverse-dfo
     trie
     #'(lambda (node)
	 (when (trie-node-mark node)
	   ;; this node is a final transition
	   (setf (aref (tabac-final idx)
		       (trie-node-id node))
		 (trie-node-mark node)
		 (aref (tabac-match-len idx)
		       (trie-node-id node))
		 (trie-node-depth node)))
	 ;; initialize transitions from this node with the failure
	 ;; transition
	 (let ((trans (make-array +ac-alphabet+
				  :initial-element (trie-node-id (trie-node-fail node))
				  :element-type 'fixnum)))
	   (map-trie-children (node child)
	     (setf (aref trans (char-code (trie-node-label child)))
		   (trie-node-id child)))
	   (setf (aref (tabac-trans idx) (trie-node-id node))
		 trans))))
    idx))

;; --------------------------------------------------------

(declaim (inline tabac-transition)
	 (ftype (function (tabac
			   fixnum
			   base-char)
			  fixnum) tabac-transition))
(defun tabac-transition (idx state c)
  "Returns a new state based on the given char from the current state."
  (the fixnum (aref (aref (tabac-trans idx) state)
		    (char-code c))))

;; --------------------------------------------------------

(defun initialize-tabac (patterns)
  "Returns a tabular FDA that is used to look for the given patterns
in the text. It can deal either with a single pattern or a list of
patterns."

  (declare #.*standard-optimize-settings*)
  (trie->tabular-ac (initialize-ac patterns)))

;; --------------------------------------------------------

(defun search-tabac (idx txt)
  ;; this function generates 9 warnings when compiled on SBCL
  (declare #.*standard-optimize-settings*)
  (check-type idx tabac)
  (check-type txt simple-string)

  (iter
    (declare (iterate:declare-variables))
    (for c in-string txt)
    (declare (type base-char c))
    (for (the fixnum pos) from 0 below (the fixnum (length txt)))
    (for (the fixnum state)
      first (tabac-transition idx (tabac-start idx) c)
      then (tabac-transition idx state c))
    (declare (type fixnum state)
	     (type fixnum pos))
    (if (= state (the fixnum (tabac-start idx)))
	(progn
	  (setf state (the fixnum (tabac-transition idx state c)))))
    (when (>= (the fixnum (aref (tabac-final idx) state))
	      0)
      (return (values (the fixnum (+ 1 (- pos (aref (tabac-match-len idx) state))))
		      (the fixnum (aref (tabac-final idx) state)))))))

;; --------------------------------------------------------

(defun string-contains-tabac (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence."

  (declare #.*standard-optimize-settings*)
  (check-type pat simple-string)
  (check-type txt simple-string)

  (when (= 0 (length pat))
    (return-from string-contains-tabac 0))

  (when (= 0 (length txt))
    (return-from string-contains-tabac nil))

  (search-tabac (initialize-tabac pat) txt))

;; EOF
