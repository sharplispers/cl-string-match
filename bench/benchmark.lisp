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


;; simple benchmarks for the string search functions to evaluate their
;; performance relative to the standard SEARCH routine
;;
;; lx86cl --load "benchmark.lisp" --eval "(run-benchmarks)"

;; based on: Performing Lisp: Measure and Explore by Kenneth R. Anderson
;; see also: http://openmap.bbn.com/~kanderso/performance/

(defparameter *times* 1000000)

(defparameter needle "abcdef")
(defparameter haystack "abcdeabcdeabcdeabcdeabcdeabcdeabcdefabcdeabcdeabcdeabcdeabcdeabcde")

(ql:quickload "cl-string-match")

;; --------------------------------------------------------

(defun timer (N function &rest args)
  (declare (fixnum N) (function function))
  (time
   (dotimes (i N)
     (declare (fixnum i))
     (apply function args))))

;; --------------------------------------------------------

(defun run-search ()
  (format t "~%Benchmarking standard system SEARCH~%")
  (timer *times* #'search needle haystack))

;; --------------------------------------------------------

(defun run-brute-force ()
  (format t "~%Benchmarking BRUTE FORCE~%")
  (timer *times* #'sm:string-contains-brute needle haystack))

;; --------------------------------------------------------

(defun run-boyer-moore ()
  (format t "~%Benchmarking BOYER MOORE simple~%")
  (timer *times* #'sm:string-contains-bm needle haystack)

  (format t "~%Benchmarking BOYER MOORE with index~%")
  (let ((idx (sm:initialize-bm needle)))
    (timer *times* #'sm:search-bm idx haystack)))

;; --------------------------------------------------------

(defun run-rabin-karp ()
  (format t "~%Benchmarking RABIN KARP simple~%")
  (timer *times* #'sm:string-contains-rk needle haystack)

  (format t "~%Benchmarking RABIN KARP with index~%")
  (let ((idx (sm:initialize-rk needle)))
    (timer *times* #'sm:search-rk idx haystack)))

;; --------------------------------------------------------

(defun run-benchmarks ()
  (run-search)
  (run-brute-force)
  (run-boyer-moore)
  (run-rabin-karp))

(format t "Eval: (run-benchmarks) to run all benchmarks~%")