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

(in-package :cl-string-match)

;; --------------------------------------------------------

(defstruct (trie-data)
  children
  label
  mark)

(defun traverse-trie (trie &optional (padding 0))
  ""
  (when trie
    (format t "~&~v@TData: ~A ~A" padding (trie-data-label trie)
	    (trie-data-mark trie))
    (when (trie-data-children trie)
      (format t "~&~v@T  Children: ~%" padding)
      (map nil #'(lambda (x)
		   (traverse-trie x (+ padding 3)))
	   (trie-data-children trie)))
    (format t "~%")))

;; --------------------------------------------------------

(defun find-trie-child (trie label)
  ;;(format t "find-trie-child: ~a ~a~%" (trie-data-label trie)  label)
  (let ((node (find-if #'(lambda (x)
			   (eql (trie-data-label x) label))
		       (trie-data-children trie))))
    ;;(format t "find-trie-child: found: ~a~%" node)
    node))

(defun trie-contains (trie s)
  "Returns T if the given Trie contains the given string."

  (loop
     :for c :across s
     :for node = trie :then (find-trie-child node c)
     :while node
     :finally (return
		(when node
		  (trie-data-mark node)))))

;; --------------------------------------------------------

(defun build-trie (patterns)
  "Builds a Trie based on the given list of patterns."
  (labels ((EMPTY-TRIE ()
	     (make-trie-data :children nil :label nil :mark nil))

	   (ADD-TRIE-CHILD (trie label mark)
	     ;; (format t "add-trie-child: ~a ~a ~a~%" (trie-data-label trie) label mark)
	     (let ((child (make-trie-data :label label
					  :mark mark
					  :children nil)))
	       (push child (trie-data-children trie))
	       child))

	   (ADD-KEYWORD (trie kw idx)
	     ;; Starting at the root, follow the path labeled by chars
	     ;; of Pi
	     ;;
	     ;; If the path ends before Pi, continue it by adding new
	     ;; edges and nodes for the remaining characters of Pi
	     ;;
	     ;; Store identifier i of Pi at the terminal node of the
	     ;; path
	     (loop
		:with node = trie
		:for c :across kw
		:for child-node = (find-trie-child node c)
		:do (if (null child-node)
			;; found a place where the path ends, add new node here
			(setf node (add-trie-child node c nil))
			;; the path continues further
			(setf node child-node))
		;; store the keyword index
		:finally (setf (trie-data-mark node) idx))

	     ))

    (let ((trie (empty-trie)))
      (loop :for pat :in patterns
	 :count pat :into idx :do
	 (progn
	   ;; (format t "~%~%adding kw: ~a~%" pat)
	   (traverse-trie trie)
	   (add-keyword trie pat idx)))
      trie)))

;; --------------------------------------------------------

(defun string-contains-ac (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence."

  (let ((trie (build-trie (list pat))))
    (loop
       :for c :across txt
       :for j :from 1 :to (length txt)
       :for node = (find-trie-child trie c) :then (find-trie-child node c)
       :do (if node
	       (if (trie-data-mark node)
		   (return (- j (length pat))))
	       (setf node (find-trie-child trie c)))
       :unless node :do (setf node trie))))

;; --------------------------------------------------------

(defun test-trie ()
  (let ((trie (build-trie '("he" "she" "his" "hers"))))
    (format t "~%~%~a~%" trie)
    (traverse-trie trie)

    (format t "~&trie-contains: ~a ~a~%" "she" (trie-contains trie "she"))
    (format t "~&trie-contains: ~a ~a~%" "from" (trie-contains trie "from"))

    (format t "~&string-contains-ac: ~a~%" (string-contains-ac trie "from his m"))
    (format t "~&string-contains-ac: ~a~%" (string-contains-ac trie "from her his m"))
    (format t "~&string-contains-ac: ~a~%" (string-contains-ac trie "from hor hos m"))))

;; EOF