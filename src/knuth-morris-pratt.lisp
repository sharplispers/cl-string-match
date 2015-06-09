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


;; The following implementation is based on algorithm described in:
;;
;; Algorithm described in: Chapter 5, p. 768 in
;;  “Algorithms”, Robert Sedgewick and Kevin Wayne. 4th
;;
;; Based on implementation by:
;;  “Knuth-Morris-Pratt vs. Boyer–Moore in LISP.” by ZBR
;;   http://www.ioremap.net/archive/other/lisp/optimized-bm-kmp-string-test.lisp

(in-package :cl-string-match)

;; --------------------------------------------------------

(defstruct kmp
  (pat     "" :type simple-string)
  (pat-len 0  :type fixnum)
  (table   (make-array 0 :element-type 'fixnum)
	   :type (simple-array fixnum)))

;; --------------------------------------------------------

(defun initialize-kmp (pat)
  (declare (type simple-string pat)
	   #.*standard-optimize-settings*)

  (let* ((idx (make-kmp
	       :pat pat
	       :pat-len (length pat)
	       :table (make-array (length pat)
				  :element-type 'fixnum)))
	 (pos 2)
	 (cnd 0))
    (declare (fixnum pos cnd))

    (setf (aref (kmp-table idx) 0) -1)

    (when (> (length pat) 1)
      (setf (aref (kmp-table idx) 1) 0))

    (do ()
	((>= pos (length pat)))
      (cond
	((char= (char pat (1- pos))
		(char pat cnd))
	 (setf (aref (kmp-table idx) pos)
	       (1+ cnd))
	 (incf pos)
	 (incf cnd))

	((> cnd 0)
	 (setf cnd (aref (kmp-table idx) cnd)))

	(t
	 (setf (aref (kmp-table idx) pos) 0)
	 (incf pos))))
    idx))

;; --------------------------------------------------------

(defun search-kmp (idx txt)
  (declare (type simple-string txt)
	   #.*standard-optimize-settings*)

  (when (= (kmp-pat-len idx) 0)
    (return-from search-kmp 0))
  (when (= 0 (length txt))
    (return-from search-kmp nil))

  (let* ((m 0)
	 (i 0)
	 (txt-len (length txt)))
    (declare (fixnum m i txt-len))

    (do ((w (aref (kmp-pat idx) i)
	    (aref (kmp-pat idx) i))
	 (s (char txt (+ m i))))
	((>= (+ m i) txt-len))

      (cond
	((char= w s)
	 (incf i)
	 (when (= i (kmp-pat-len idx))
	   (return-from search-kmp m)))
	(t
	 (let ((ti (elt (kmp-table idx) i)))
	   (setf m (- (+ m i) ti))
	   (when (> i 0)
	     (setf i ti)))))

      (let ((mi (+ m i)))
        (when (< mi txt-len)
          (setf s (elt txt mi))))))
  NIL)

;; --------------------------------------------------------

(defun string-contains-kmp (pat txt)
  (declare (type string pat)
	   (type string txt)
	   #.*standard-optimize-settings*)

  (when (= 0 (length pat))
    (return-from string-contains-kmp 0))
  (when (= 0 (length txt))
    (return-from string-contains-kmp nil))
  (search-kmp (initialize-kmp pat) txt))


;; EOF
