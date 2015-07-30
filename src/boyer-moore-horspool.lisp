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


;;; Boyer-Moore-Horspool algorithm
;;; simplification of the Boyer-Moore algorithm;

;;; * preprocessing phase in O(m+s) time and O(s) space complexity;
;;; * searching phase in O(mn) time complexity;
;;; * the average number of comparisons for one text character is between 1/s and 2/(s+1).

;; implementation based on the description from the book
;;
;; "Exact String Matching Algorithms" by Christian Charras and Thierry Lecroq
;;
;; http://www-igm.univ-mlv.fr/~lecroq/string/node18.html#SECTION00180
;;
;; It uses only the "Bad character skip" rule, and does not use the
;; "Good suffix rule"

(in-package :cl-string-match)

;; --------------------------------------------------------

(defmacro define-bmh-matcher (variant-tag
                              &key
			      (key-get 'char)
			      (key-code 'char-code)
			      (key-cmp= 'char=)
			      (empty-pat "")
			      (alphabet-size char-code-limit)
			      (data-type 'simple-string))

  (let* ((index-name           (format-name "BMH~a" variant-tag))
	 (initialize-name      (format-name "INITIALIZE-BMH~a" variant-tag))
	 (search-name          (format-name "SEARCH-BMH~a" variant-tag))
	 (matcher-name         (format-name "STRING-CONTAINS-BMH~a" variant-tag))
	 (make-index           (format-name "MAKE-~a" index-name))
	 (the-bad-char-skip    (format-name "~a-BAD-CHAR-SKIP" index-name))
	 (the-pat              (format-name "~a-PAT" index-name))
	 (the-pat-len          (format-name "~a-PAT-LEN" index-name)))
    `(progn

       ;; --------------------------------------------------------
       ;; Member variables for storing precomputed pattern data
       (defstruct ,index-name
	 (bad-char-skip #() :type (simple-array fixnum (*)))
	 (pat ,empty-pat :type ,data-type)
	 (pat-len 0 :type fixnum))

       ;; --------------------------------------------------------

       (defun ,initialize-name (pat)
	 "Preprocess the needle.
Initialize the table to default value."

	 (declare #.*standard-optimize-settings*)
	 (check-type pat ,data-type)
	 ;; When a character is encountered that does not occur in the
	 ;; needle, we can safely skip ahead for the whole length of the
	 ;; needle.
	 (let ((idx
		(,make-index
		 :pat pat
		 :pat-len (length pat)
		 :bad-char-skip
		 (make-array ,alphabet-size
			     :element-type 'fixnum
			     :initial-element (the fixnum (length pat))))))

	   (loop
	      :for k :from 0 :below (- (length pat) 1) :do
	      (setf (aref (,the-bad-char-skip idx)
			  (,key-code (,key-get pat k)))
		    (- (length pat) k 1)))
	   idx))

       ;; --------------------------------------------------------

       (defun ,search-name (idx txt &key (start2 0) (end2 nil))
	 "Search for pattern defined in the IDX in TXT."

	 (declare #.*standard-optimize-settings*)
	 (check-type idx ,index-name)
	 (check-type txt ,data-type)
	 (check-type start2 fixnum)
	 (check-type end2 (or null fixnum))
	 (when (= 0 (,the-pat-len idx))
	   (return-from ,search-name 0))
	 (when (= 0 (length txt))
	   (return-from ,search-name nil))

	 (loop
	    :with m fixnum = (,the-pat-len idx)
	    :with n fixnum = (if end2 end2 (length txt))
	    :with j fixnum = start2
	    ;; Search the haystack, while the needle can still be within it.
	    :while (<= j (- n m))
	    :do (let ((c (,key-get txt (- (+ j m) 1))))

		  (when (,key-cmp= c (,key-get (,the-pat idx) (- m 1)))
		    (loop :for i fixnum :from 0 :below m
		       :while (,key-cmp= (,key-get (,the-pat idx) i)
					 (,key-get txt (+ i j)))
		       :finally
		       (when (= i m)
			 (return-from ,search-name j))))

		  (incf j (aref (,the-bad-char-skip idx)
				(,key-code c))))))

       ;; --------------------------------------------------------

       (defun ,matcher-name (pat txt &key (start2 0) (end2 nil))
	 (declare #.*standard-optimize-settings*)
	 (check-type pat ,data-type)
	 (check-type txt ,data-type)
	 (check-type start2 fixnum)
	 (check-type end2 (or null fixnum))
	 (,search-name (,initialize-name pat) txt
		       :start2 start2 :end2 end2)))))

;; --------------------------------------------------------

(define-bmh-matcher "")

;; The following set of BMH matchers operate on strings that contain
;; characters in the range 0-256 (single-byte or octet). Therefore,
;; the skip array in the index is not equal to the CHAR-CODE-LIMIT
;; that is huge for Lisp implementations with Unicode support, but has
;; a fixed size of 256 cells
(define-bmh-matcher "8"
		    :empty-pat ""
                    :key-code ascii-char-code
                    :alphabet-size ub-char-code-limit)

(export 'bmh8)
(export 'bmh8-p)
(export 'bmh8-pat)
(export 'initialize-bmh8)
(export 'search-bmh8)
(export 'string-contains-bmh8)

#|
(defun report-bmh-idx (idx)
  "Report skip values for the pattern in the BMH index."
  (loop :for c :across (bmh-pat idx)
     :do (format t "~a: ~a~%" c (aref (bmh-bad-char-skip idx)
				      (char-code c)))))

(defun search-tbmh (idx txt)
  )

void TUNEDBM(char *x, int m, char *y, int n) {
   int j, k, shift, bmBc[ASIZE];

   /* Preprocessing */
   preBmBc(x, m, bmBc);
   shift = bmBc[x[m - 1]];
   bmBc[x[m - 1]] = 0;
   memset(y + n, x[m - 1], m);

   /* Searching */
   j = 0;
   while (j < n) {
      k = bmBc[y[j + m -1]];
      while (k !=  0) {
         j += k; k = bmBc[y[j + m -1]];
         j += k; k = bmBc[y[j + m -1]];
         j += k; k = bmBc[y[j + m -1]];
      }
      if (memcmp(x, y + j, m - 1) == 0 && j < n)
         OUTPUT(j);
      j += shift;                          /* shift */
   }
}
;; EOF



|#
