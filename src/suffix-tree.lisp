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

;; based on description in StackOverflow question:
;;
;; http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english

(in-package :cl-string-match)

;; --------------------------------------------------------

(defstruct (suffix-tree (:conc-name suffix-tree.)
			(:print-function suffix-tree-printer))
  )

(defun suffix-tree-printer (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t :identity t)))

;; --------------------------------------------------------

(defstruct (suffix-node (:conc-name suffix-node.)
			(:print-function suffx-node-printer))
  )

(defun suffix-node-printer (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t :identity t)))

;; --------------------------------------------------------

(defun build-suffix-tree (str)
  "Build a Suffix tree for the given string STR using Ukkonen's
  algorithm."

  )

;; EOF
