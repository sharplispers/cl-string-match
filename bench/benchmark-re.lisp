;;; benchmark regular expressions engine and compare its performance
;;; with other regular engine implementations for Common Lisp,
;;; CL-PPCRE is of primary interest as a de-facto standard

;;; Running:
;;;
;;; lx86cl --load "benchmark-re.lisp" --eval "(run-re-benchmarks)" --eval "(quit)"
;;; sbcl --load "benchmark-re.lisp" --eval "(run-re-benchmarks)" --quit
;;; 
;;; Or after compiling with sbcl.comile:
;;;
;;; sbcl --noinform --load "benchmark-re.fasl" --eval "(run-re-benchmarks)" --quit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-string-match)
  (ql:quickload :cl-ppcre)
  (ql:quickload :regex))

(defparameter +times+ 1)

(defun bm-timer (function &rest args)
  "Timer function that measures duration of the test function
execution multiple times.

The timer is responsible for pretty-printing benchmark information and
duration to the log file so that it can be later used to produce a
chart.

Returns: the amount of elapsed time."

  (declare (function function))

  (let ((start (get-internal-run-time))
	elapsed)
    (loop :repeat +times+ :do (apply function args))
    (setq elapsed (/ (- (get-internal-run-time) start)
                     (float internal-time-units-per-second 1d0)))
    elapsed))


;;; This benchmark is derived from the programming languages
;;; benchmark, patmatch:1t and patmatch:2t benchmarks.
;;;
;;; http://attractivechaos.github.io/plb/
;;;
;;; We are also using the same data file:
;;;
;;; http://people.unipmn.it/manzini/lightweight/corpus/howto.bz2

(defparameter *expressions*
  '("([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/?[^ ]*)"
    "([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/?[^ ]*)|([^ @]+)@([^ @]+)"))


;; like in the original benchmark we copy the file to the /dev/shm/
;; directory to put it into the RAM and avoid I/O overhead
;;
;; run
;; iconv -t utf8//IGNORE <howto >/dev/shm/howto
;; to filter out invalid sequences that are not accepted by SBCL reader
(defparameter *fname* "/dev/shm/howto")


(defun run-pre-benchmark ()
  "Portable RE bencmark."
  (dolist (e *expressions*)
    (let ((re (sm:compile-re e)))
      (with-open-file (in *fname* :direction :input)
	(format t "found: ~a matches~%"
		(loop for line = (read-line in nil)
		   while line
		   when (sm:find-re re line)
		   count line))))))


(defun run-ppcre-benchmark ()
  "CL-PPCRE benchmark."
  (dolist (e *expressions*)
    (let ((re (ppcre:create-scanner e)))
      (with-open-file (in *fname* :direction :input)
	(format t "found: ~a matches~%"
		(loop for line = (read-line in nil)
		   while line
		   when (ppcre:scan re line)
		   count line))))))

(defun run-regex-benchmark ()
  "CLAWK/regexp benchmark. "
  (dolist (e *expressions*)
    (let ((re (regex:compile-str e)))
      (with-open-file (in *fname* :direction :input)
	(format t "found: ~a matches~%"
		(loop for line = (read-line in nil)
		   while line
		   when (regex:scan-str re line)
		   count line))))))


(defun run-re-benchmarks ()
  (format t "pre complete in: ~a seconds~%ppcre complete in: ~a seconds~%regex complete in: ~a seconds~%"
	  (bm-timer #'run-pre-benchmark)
	  (bm-timer #'run-ppcre-benchmark)
	  (bm-timer #'run-regex-benchmark)))
