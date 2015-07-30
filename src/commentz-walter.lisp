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

;; TODO: Commentz-Walter algorithm implementation
;; 
;; devoted to the backward pattern matching of a finite set of
;; patterns. It uses:
;;
;; * multi bad character shift (MBCS, see Section 8.2.1),
;; * multi good suffix shift (MGSS, see Section 8.2.2), and
;; * reduced multi good prefix shift (RMGPS, see Section 8.2.3.2).
;;
;; The basic principle of CW algorithm is based on the selection of
;; the longer shift from MBCS and MGSS shifts.
;;
;; Replace shift step:
;;
;; I := I + 1;
;; by statement
;; I := I + min(LMIN, min(RMGPS[I], max(MBCS[TEXT[I + J]], MGSS[J])));
;;
;; where RMGPS is reduced multi good prefix shift, MBCS is multi bad char-
;; acter shift and MGSS is multi good suffix shift.

(in-package :cl-string-match)

;; --------------------------------------------------------

(defstruct (cw)
  lmin ;; minimal length of a pattern
  bad-shift ;; bad character shift table
  good-shift ;;
  trie)

;; --------------------------------------------------------

(defstruct (cw-node (:include trie-node))
  ;; we "inherit" properties: children label mark from the trie-node
  ;; depth ;; node depth (number of nodes from the root to this node)
  )

;; --------------------------------------------------------

;; using example from ijsrp-july-2012-101.pdf:
;;
;; (initialize-cw '("arch" "search" "ear" "chart"))
;;
;; from "Efficient Algorithms for Multiple Pattern Matching - M.A. Sridhar TR661.pdf"
;;
;; (initialize-cw '("cdabcd" "eabcdaec" "deccec"))
(defun initialize-cw (patterns)
  "Initialize Commentz-Walter automata for the given set of patterns."
  ;; it is not optimal in the current version, just making it
  ;; work. besides it is intended to run once
  (let ((idx (make-cw)))
    (setf (cw-trie idx)
	  (trie-build (map 'list #'reverse patterns) :constructor #'make-cw-node))
    (setf (cw-lmin idx)
	  (loop for pat in patterns minimize (length pat)))

    #+ignore
    (loop :for j :from 0 :below pat-len
       :do (setf (aref (bm-right bm)
		       (hash-bm (char pat j)))
		 j))

    idx))

;; --------------------------------------------------------

(defun search-cw (idx txt)
  "Looks for patterns defined in the index in the txt.

Basic algorithm is simple: start from the root node (end of key
suffix) and go backwards in text till we reach the leaf node (start of
key). If we can't reach the leaf, reset to the root node and shift
forward in text. Repeat.

Commentz-Walter improves this basic algorithm by pre-computing values
for the forward shift in text upon character mismatch."
  (check-type idx cw)
  (check-type txt simple-string)

  (let ((trie (cw-trie idx)))
    (loop
       ;; our position within text
       :with txt-pos = (cw-lmin idx)
       :while (< txt-pos (length txt)) :do
       (loop
	  ;; we are going from right to left during match check
	  :for key-pos :from 0 :below (length txt)
	  ;; c is the current char
	  :for c = (char txt (- txt-pos key-pos)) :then (char txt (- txt-pos key-pos))
	  ;; next trie node is a child of the current node or root
	  ;; matching current char c
	  :for node = (trie-find-child trie c) :then (trie-find-child node c)
	  ;; :do (format t "txt-pos=~a key-pos=~a c=~a node=~a~%" txt-pos key-pos c node)
	  :do (if node
		  ;; there is a matching node
		  (if (trie-node-mark node)
		      ;; if this node has a mark this means that we've
		      ;; reached an end of some pattern, therefore we
		      ;; return a successful match
		      (return-from search-cw
			(values (- txt-pos key-pos)
				(trie-node-mark node)))
		      ;; matching node is still within the word,
		      ;; progress within the key in the next cycle
		      )
		  ;; since there is no matching node we reset our
		  ;; search and push our text position by skip amount
		  ;; of characters (one char at the moment)
		  (setf txt-pos (+ txt-pos 1)))
	  :while node))))

;; --------------------------------------------------------

(defun string-contains-cw (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence."
)

;; EOF
