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

;;; Shift-OR single pattern search algorithm implementation
;;
;; Used
;;
;;  http://www-igm.univ-mlv.fr/~lecroq/string/node6.html#SECTION0060
;;
;; As the blueprint for this implementation

(in-package :cl-string-match)

;; --------------------------------------------------------

(deftype sor-ub32 ()
  '(signed-byte 64))

;; --------------------------------------------------------

(define-constant +sor-alphabet+ 256 :documentation
  "Size of the alphabet for the Shift-OR algorithm implementation.")

;; --------------------------------------------------------

(defstruct (sor
	     (:print-function sor-printer))
  "Index for the Shift-OR search operation."
  (cidx)	; bit array identifying positions of characters in the
		; pattern: cidx[c]=bit-mapped positions of char c

  (lim #x0 :type sor-ub32)		;
  (pat-len 0 :type fixnum)		; pattern length
  )

;; --------------------------------------------------------

(defun sor-printer (obj stream depth)
  "Make dump of the SOR structure more human-readable: in the CIDX
print characters and their positions, decipher LIM."

  (declare (ignore depth)
	   (type sor obj))
  (format stream "#S<sor cidx: ~a lim: ~a>"
	  (iter
	    (for i from 0 below (length (sor-cidx obj)))
	    (when (/= (elt (sor-cidx obj) i) -1)
	      (collect (cons (code-char i)
			     (elt (sor-cidx obj) i)))))
	  (sor-lim obj)))

;; --------------------------------------------------------

(defun initialize-sor (pat)
  (let ((idx (make-sor :cidx (make-array +sor-alphabet+
					 :initial-element (lognot 0)
					 :element-type 'sor-ub32 )
		       :pat-len (length pat)))
	(marker #x1))
    (declare (type sor-ub32 marker)
	     (optimize safety debug))

    (iter
      (for i from 0 below (length pat))
      (for cc = (char-code (char pat i)))
      ;; S[x[i]] &= ~j
      (setf (elt (sor-cidx idx) cc)
	    (the sor-ub32 (logand (the sor-ub32 (elt (sor-cidx idx) cc))
				  (the sor-ub32 (lognot marker)))))
      ;; lim |= j
      (setf (sor-lim idx)
	    (logior (sor-lim idx)
		    marker))
      ;; j <<= 1
      (setf marker (ash marker 1)))

    ;;lim = ~(lim>>1)
    (setf (sor-lim idx)
	  (lognot (ash (sor-lim idx) -1)))
    idx))

;; --------------------------------------------------------

(defun search-sor (idx txt  &key (start2 0) (end2 nil))
  (declare (type simple-string txt)
	   (type fixnum start2)
	   (type (or fixnum null) end2)
	   #.*standard-optimize-settings*)

  (when (= 0 (sor-pat-len idx))
    (return-from search-sor 0))
  (when (= 0 (length txt))
    (return-from search-sor nil))

  (iter (with state = (the sor-ub32 (lognot 0)))
	(for j from start2 below (if end2 end2 (length txt)))
	(setf state
	      (logior (the sor-ub32 (ash state 1))
		      (elt (sor-cidx idx)
			   (char-code (char txt j)))))
	(when (< state (sor-lim idx))
	  (return (+ (- j (sor-pat-len idx)) 1)))))

;; --------------------------------------------------------

(defun string-contains-sor (pat txt)
  (declare (type simple-string pat)
	   (type simple-string txt)
	   #.*standard-optimize-settings*)

  (when (= 0 (length pat))
    (return-from string-contains-sor 0))

  (when (= 0 (length txt))
    (return-from string-contains-sor nil))

  (search-sor (initialize-sor pat) txt))

;; EOF
