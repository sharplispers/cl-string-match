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

(in-package :cl-string-match)

;; --------------------------------------------------------

(deftype sor-ub32 ()
  '(unsigned-byte 32))

;; --------------------------------------------------------

(define-constant +sor-alphabet+ 256 :documentation
  "Size of the alphabet for the Shift-OR algorithm implementation.")

;; --------------------------------------------------------

(defstruct sor
  "Index for the Shift-OR search operation."
  (cidx)	; bit array identifying positions of characters in the
		; pattern: cidx[c]=bit-mapped positions of char c

  (lim #x0 :type sor-ub32)		;
  )

;; --------------------------------------------------------

(defun initialize-sor (pat)
  (let ((idx (make-sor :cidx (make-array +sor-alphabet+
					 :initial-element #xFFFFFFFF
					 :element-type 'sor-ub32 )))
	(marker #x1))
    (declare (type sor-ub32 marker))

    (iter
      (for i from 0 below (length pat))
      (for cc = (char-code (char pat i)))
      (format t "~a: ~a, ~a; m: ~a~%" i (char pat i) cc (lognot marker))
      (setf (elt (sor-cidx idx) cc)
	    (the sor-ub32 (lognot marker)))
      (ash marker 1)
      )
    idx))

;; --------------------------------------------------------

(defun search-sor (pat txt)
  )

;; EOF