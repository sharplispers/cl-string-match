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

(eval-when (:compile-toplevel :load-toplevel :execute)
 (ql:quickload "cl-string-match")
 (ql:quickload "cl-ppcre"))

;; --------------------------------------------------------

(defconstant +times+ (* 10 100))

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
    (loop :repeat +times+ :do (setf ret (apply function args)))
    (setf elapsed (/ (- (get-internal-run-time) start)
                     (float internal-time-units-per-second 1d0)))
    ;; (log-msg (format nil "~a	~3$~%" len elapsed))
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
  #+ignore
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
;; Count matches of a pattern in a long file.
;;
;; Download the file/book from the Project Gutenberg:
;;
;; wget --no-check-certificate https://www.gutenberg.org/ebooks/135.txt.utf-8
;;
;; In this case we are using Les Miserables as it is one of the
;; longest texts available for free and with 3.5MB in size it still
;; can be loaded into memory.
;;
;; And then run the benchmark:
;;
;; lx86cl --load "benchmark.lisp" --eval "(run-count-matches)" --eval "(quit)"
;;
;; Or (after compiling the sources):
;;
;; sbcl --noinform --load "benchmark.fasl" --eval "(run-count-matches)" --quit

(defun count-sub (pat &key (predicate nil))
  "Count all occurences of pat in the given str. Code adopted from
Rosetta Code:

http://rosettacode.org/wiki/Count_occurrences_of_a_substring#Common_Lisp"

  (loop with z = 0 with s = 0 while s do
       (when (setf s (funcall predicate s))
	 (incf z) (incf s (length pat)))
     finally (return z)))

;; --------------------------------------------------------

(defun run-count-matches (&key (fname "135.txt.utf-8"))
  (with-open-file (in fname :direction :input)
    (let* ((pat "strength")
	   (str (make-string (file-length in)))
	   (idx (sm:initialize-bmh pat))
	   (bm 0)
	   (st 0))

      ;; slurp book contents into memory
      (read-sequence str in)

      ;; now benchmark the time required to find all matches of pat in
      ;; this string
      (format t "time to count for BM: ~a~%"
	      (bm-timer (length pat)
			#'(lambda ()
			    (setf bm
				  (count-sub pat :predicate #'(lambda (s)
								(sm:search-bmh idx str :start2 s)))))))
      (format t "time to count for ST: ~a~%"
	      (bm-timer (length pat)
			#'(lambda ()
			    (setf st
				  (count-sub pat :predicate #'(lambda (s)
								(search pat str :start2 s)))))))
      (format t "matches found: bm=~a; st=~a~%" bm st)
      )))

;; --------------------------------------------------------
;; Random haystack and the needle benchmarks
;; --------------------------------------------------------
;;
;; To run random benchmarks first compile the source file and execute
;; the following command:
;;
;; sbcl --load benchmark.fasl --eval '(rnd-run)' --eval '(quit)'
;; lx86cl --load benchmark --eval '(rnd-run)' --eval '(quit)'
;; ecl -load benchmark.fas -eval '(rnd-run)' -eval '(quit)'

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

  (let ((start (get-internal-run-time)) ; to measure total time per wall-clock
	elapsed				; will be set in the end
	(sys-times '())
	(bf-times '())
	(bm-times '())
	(bmh-times '())
	(bmh8-times '())
	(rk-times '())
	(kmp-times '())
	(sor-times '())
	(times 10))

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
	    (kmp-time 0.0d0)
	    (sor-time 0.0d0))

	(dotimes (n times)
	  ;; in order to better average performance it is possible to
	  ;; re-generate a needle on every attempt, but this makes
	  ;; multistring performance comparison with AC more difficult
	  (fill-random-string needle random-alphabet-size)
	  ;; (format t "needle: ~a~%" needle)
	  (let ((bm-idx (sm:initialize-bm needle))
		(bmh-idx (sm:initialize-bmh needle))
		(bmh8-idx (sm:initialize-bmh8 needle))
		(rk-idx (sm:initialize-rk needle))
		(kmp-idx (sm:initialize-kmp needle))
		(sor-idx nil
		 #+ignore
		 (sm:initialize-sor needle)))

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
				     #'sm:search-kmp kmp-idx random-haystack))
	    (incf sor-time 0.0
		  #+ignore
		  (bm-timer (length needle)
				     #'sm:search-sor sor-idx random-haystack)))
	  (format t ".")
	  (force-output))
	(push sys-time sys-times)
	(push bf-time bf-times)
	(push bm-time bm-times)
	(push bmh-time bmh-times)
	(push bmh8-time bmh8-times)
	(push rk-time rk-times)
	(push kmp-time kmp-times)
	(push sor-time sor-times))
      (format t "~%"))

    (with-open-file (out "random.log"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "\# l sys bf bm bmh bmh8 rk kmp sor~%")
      (map nil
	   #'(lambda (n sys bf bm bmh bmh8 rk kmp sor)
	       (format out "~a ~,2f ~,2f ~,2f ~,2f ~,2f ~,2f ~,2f ~,2f~%" (length n)
		       sys bf bm bmh bmh8 rk kmp sor))
	   random-needles sys-times bf-times bm-times bmh-times bmh8-times rk-times kmp-times sor-times))

    ;; Aho-Corasick
    (let ((ac-idx (sm:initialize-ac random-needles))
	  (ac-time 0.0d0)
	  (tabac-idx (sm:initialize-tabac random-needles))
	  (tabac-time 0.0d0))

      ;; since we search for all needles at once, we have to perform
      ;; our test only TIMES times.
      (dotimes (n times)
	(incf ac-time (bm-timer (length random-needles)
				#'sm:search-ac ac-idx random-haystack)))
      (dotimes (n times)
	(incf tabac-time (bm-timer (length random-needles)
				   #'sm:search-tabac tabac-idx random-haystack)))

      (format t "AC complete in: ~,2f seconds~%" ac-time)
      (format t "TABAC complete in: ~,2f seconds~%" tabac-time)
      (format t "BMH complete in: ~,2f seconds~%"
	      (loop :for el :in bmh-times
		 :sum el)))

    (setq elapsed (/ (- (get-internal-run-time) start)
                     (float internal-time-units-per-second 1d0)))
    (format t "Benchmarks complete in: ~,2f seconds~%" elapsed)

    ))

;; EOF
