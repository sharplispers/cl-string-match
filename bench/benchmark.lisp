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
;; lx86cl --load "benchmark.lisp" --eval "(run-benchmarks)" --eval "(quit)"

;; based on: Performing Lisp: Measure and Explore by Kenneth R. Anderson
;; see also: http://openmap.bbn.com/~kanderso/performance/
;;
;; See also: http://clisp.hg.sourceforge.net/hgweb/clisp/clisp/file/tip/benchmarks/run-all.lisp

(ql:quickload "cl-string-match")
(ql:quickload "cl-ppcre")

;; --------------------------------------------------------

(defconstant +times+ (* 10 1000))

;; some algorithms process needle from the end to start, other
;; algorithms go in the opposite direction. The mismatching symbol is
;; put into the middle to make it work in both cases
(defparameter needles '("abc_de"
			"abcde_abcde"
			"abcdeabc_deabcde"
			"abcdeabcde_abcdeabcde"
			"abcdeabcdeab_cdeabcdeabcde"
			"abcdeabcdeabcde_abcdeabcdeabcde"))

(defparameter haystack "abcdeabcdeabcdeabcdeabcdeabcdeabcdefabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcde")

(defconstant +bm-simple+ nil
  "Whether to benchmark implementations including index generation.")

;; --------------------------------------------------------

(defparameter log-file "benchmark.log")

;; --------------------------------------------------------

(defun log-msg (msg)
  (with-open-file (out log-file
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (write-string msg out)))

;; --------------------------------------------------------

(defun log-title (title)
  (log-msg (format nil "~%~%\"~a\"~%" title)))

;; --------------------------------------------------------

(defun bm-timer (len function &rest args)
  "Timer function that measures duration of the test function
execution multiple times.

The timer is responsible for pretty-printing benchmark information and
duration to the log file so that it can be later used to produce a
chart.

Returns: the amount of elapsed time."

  (declare (function function))

  (let ((start (get-internal-run-time))
	ret
	elapsed)
    (loop :repeat +times+ :do (setq ret (apply function args)))
    (setq elapsed (/ (- (get-internal-run-time) start)
                     (float internal-time-units-per-second 1d0)))
    (log-msg (format nil "~a	~3$~%" len elapsed))
    elapsed))


;; --------------------------------------------------------

(defun run-search ()
  (log-msg (format nil "\"System SEARCH\"~%"))
  (dolist (needle needles)
    (bm-timer (length needle)
	      #'search needle haystack)))

;; --------------------------------------------------------

(defun run-brute-force ()
  (log-title "BRUTE FORCE")
  (dolist (needle needles)
    (bm-timer (length needle)
	      #'sm:string-contains-brute needle haystack)))

;; --------------------------------------------------------

(defun run-boyer-moore ()
  (when +bm-simple+
    (log-title "BOYER MOORE simple")
    (dolist (needle needles)
      (bm-timer (length needle)
		#'sm:string-contains-bm needle haystack)))

  (log-title "BOYER MOORE with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-bm needle)))
      (bm-timer (length needle)
		#'sm:search-bm idx haystack))))

;; --------------------------------------------------------

(defun run-boyer-horspool ()
  (when +bm-simple+
    (log-title "BOYER MOORE HORSPOOL simple")
    (dolist (needle needles)
      (bm-timer (length needle)
		#'sm:string-contains-bmh needle haystack)))

  (log-title "BOYER MOORE HORSPOOL with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-bmh needle)))
      (bm-timer (length needle)
		#'sm:search-bmh idx haystack))))

;; --------------------------------------------------------

(defun run-boyer-horspool8 ()
  (when +bm-simple+
    (log-title "BOYER MOORE HORSPOOL-8 simple")
    (dolist (needle needles)
      (bm-timer (length needle)
		#'sm:string-contains-bmh8 needle haystack)))

  (log-title "BOYER MOORE HORSPOOL-8 with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-bmh8 needle)))
      (bm-timer (length needle)
		#'sm:search-bmh8 idx haystack))))

;; --------------------------------------------------------

(defun run-rabin-karp ()
  (when +bm-simple+
    (log-title "RABIN KARP simple")
    (dolist (needle needles)
      (bm-timer (length needle)
		#'sm:string-contains-rk needle haystack)))

  (log-title "RABIN KARP with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-rk needle)))
      (bm-timer (length needle)
		#'sm:search-rk idx haystack))))

;; --------------------------------------------------------

(defun run-knuth-morris-pratt ()
  (log-title "KNUTH-MORRIS-PRATT with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-kmp needle)))
      (bm-timer (length needle)
		#'sm:search-kmp idx haystack))))

;; --------------------------------------------------------

(defun run-aho-corasick ()
  (when +bm-simple+
    (log-title "AHO-CORASICK simple")
    (dolist (needle needles)
      (bm-timer
       (length needle)
       #'sm:string-contains-ac needle haystack)))

  (log-title "AHO-CORASICK with index")
  (dolist (needle needles)
    (let ((idx (sm:initialize-ac needle)))
      (bm-timer
       (length needle)
       #'sm:search-ac idx haystack))))

;; --------------------------------------------------------

(defun collect-needles (needle)
  "Generates a list of needles that replace the underscore character
with 20 different characters."

  (loop :with kw-count = 0
     :for c :from (char-code #\A) :to (+ (char-code #\A) 20)
     :for new-needle = (copy-seq needle)
     :do (setf (aref new-needle (position #\_ needle)) (code-char c))
     :collect new-needle
     :do (incf kw-count)))

;; --------------------------------------------------------

(defun run-aho-corasick-many ()
  (log-title (format nil "AHO-CORASICK with 20 keywords index"))
  (dolist (needle needles)
    (let* ((idx (sm:initialize-ac (collect-needles needle))))
      (bm-timer
       (length needle)
       #'sm:search-ac idx haystack))))

;; --------------------------------------------------------

(defun run-ppcre ()
  (log-title (format nil "PPCRE with index"))
  (dolist (needle needles)
    (let* ((idx (ppcre:create-scanner needle)))
      (bm-timer
       (length needle)
       #'ppcre:scan idx haystack))))

;; --------------------------------------------------------

(defun run-ppcre-many ()
  (log-title (format nil "PPCRE with 20 keywords index"))
  (dolist (needle needles)
    (let* ((idx (ppcre:create-scanner `(:alternation ,@(collect-needles needle)))))
      (bm-timer
       (length needle)
       #'ppcre:scan idx haystack))))

;; --------------------------------------------------------

(defun run-benchmarks ()
  (with-open-file (out log-file
		       :direction :output
		       :if-exists :supersede)
    (declare (ignore out)))

  (run-search)
  (run-brute-force)
  (run-boyer-moore)
  (run-boyer-horspool)
  (run-boyer-horspool8)
  (run-rabin-karp)
  (run-knuth-morris-pratt)
  (run-aho-corasick)
  (run-aho-corasick-many)
  (run-ppcre)
  (run-ppcre-many))

(format t "Eval: (run-benchmarks) to run all benchmarks~%")

;; --------------------------------------------------------
;; Random haystack and the needle benchmarks
;; --------------------------------------------------------
;; 
;; To run random benchmarks first compile the source file and execute
;; the following command:
;; 
;; sbcl --load benchmark --eval '(rnd-run)' --eval '(quit)'

(defparameter *alphabet*
  (concatenate 'list
	       ;; upper-case characters
	       (loop :for c :from 65 :to 90 :collect (code-char c))
	       ;; lower-case characters
	       (loop :for c :from 97 :to 122 :collect (code-char c))
	       ;; digits and different symbols
	       (loop :for c :from 33 :to 64 :collect (code-char c)))
  "List of characters that are used to construct random needles and
the haystack. We give priority to the upper case characters and the
lowest priority to different symbols and numbers.")

;; --------------------------------------------------------

(defun fill-random-string (str alphabet-size)
  "Fills the given string with random characters from obtained from
the master *ALPHABET* limited by the given ALPHABET-SIZE."

  (loop :for i :from 0 :below (length str)
     :do (setf (char str i) (nth (random alphabet-size) *alphabet*)))
  str)

;; --------------------------------------------------------

(defparameter random-alphabet-size 12)

(defparameter random-needles
  '())

(defparameter random-haystack
  (make-string 1024))


(defun fill-random-needles ()
  (setf random-needles
	(loop :for n :from 1 :to 17 :collect
	   (fill-random-string (make-string (+ 5 (* n 2))) random-alphabet-size))))

(defun fill-random-haystack ()
  (setf random-haystack
	(fill-random-string random-haystack random-alphabet-size)))

(defun rnd-run ()
  (log-title "Random needles and haystacks with index")

  (let ((sys-times '())
	(bf-times '())
	(bm-times '())
	(bmh-times '())
	(bmh8-times '())
	(rk-times '())
	(kmp-times '()))
    (fill-random-needles)
    (fill-random-haystack)
    ;; (format t "haystack: ~a~%" random-haystack)
    (dolist (needle random-needles)
      (format t "processing needle ~a" (length needle))
      (force-output)
      (let ((sys-time 0.0d0)
	    (bf-time 0.0d0)
	    (bm-time 0.0d0)
	    (bmh-time 0.0d0)
	    (bmh8-time 0.0d0)
	    (rk-time 0.0d0)
	    (kmp-time 0.0d0))

	(dotimes (n 10)
	  (fill-random-string needle random-alphabet-size)
	  ;; (format t "needle: ~a~%" needle)
	  (let ((bm-idx (sm:initialize-bm needle))
		(bmh-idx (sm:initialize-bmh needle))
		(bmh8-idx (sm:initialize-bmh8 needle))
		(rk-idx (sm:initialize-rk needle))
		(kmp-idx (sm:initialize-kmp needle)))
	    
	    (incf sys-time (bm-timer (length needle)
				     #'search needle random-haystack))
	    (incf bf-time (bm-timer (length needle)
				    #'sm:string-contains-brute needle random-haystack))
	    (incf bm-time (bm-timer (length needle)
				    #'sm:search-bm bm-idx random-haystack))
	    (incf bmh-time (bm-timer (length needle)
				     #'sm:search-bmh bmh-idx random-haystack))
	    (incf bmh8-time (bm-timer (length needle)
				      #'sm:search-bmh8 bmh8-idx random-haystack))
	    (incf rk-time (bm-timer (length needle)
				    #'sm:search-rk rk-idx random-haystack))
	    (incf kmp-time (bm-timer (length needle)
				     #'sm:search-kmp kmp-idx random-haystack)))
	  (format t ".")
	  (force-output))
	(push sys-time sys-times)
	(push bf-time bf-times)
	(push bm-time bm-times)
	(push bmh-time bmh-times)
	(push bmh8-time bmh8-times)
	(push rk-time rk-times)
	(push kmp-time kmp-times))
      (format t "~%"))

    (with-open-file (out "random.log"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "\# l sys bf bm bmh bmh8 rk kmp~%")
      (map nil
	   #'(lambda (n sys bf bm bmh bmh8 rk kmp)
	       (format out "~a ~,2f ~,2f ~,2f ~,2f ~,2f ~,2f ~,2f~%" (length n)
		       sys bf bm bmh bmh8 rk kmp))
	   random-needles sys-times bf-times bm-times bmh-times bmh8-times rk-times kmp-times))

    ) )