;;; Test the ASCII strings implementation

;; --------------------------------------------------------
;; some tests
;; --------------------------------------------------------

(ql:quickload :ascii-strings)

(defun test-ub-read-line ()
  (with-open-file (in "test.txt"
                      :direction :input
                      :element-type 'ub-char)
    (loop :with reader = (ascii:make-ub-line-reader :stream in)
       :for i :from 0 :below 10
       :for line = (ascii:ub-read-line reader)
       :while line
       :do (format t "[~a]: ~a~%" i (ascii:ub-to-string line)))))

(defun test-ub-count-lines (fname)
  (with-open-file (in fname
                      :direction :input
                      :element-type 'ascii:ub-char)
    (loop :with reader = (ascii:make-ub-line-reader :stream in)
       :for i :from 0
       :for line = (ascii:ub-read-line reader)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))

(defun test-count-lines (fname)
  (with-open-file (in fname
                      :direction :input)
    (loop
       :for i :from 0
       :for line = (read-line in nil)
       :while line
       :finally (format t "file {~a} contains ~a lines~%" fname i))))


(defun test-simple-strings ()
  "Test simple string operations"
  (let* ((string1 (ascii:string-to-ub "abcdefgh123"))
         (string2 (ascii:string-to-ub "ABCDEFGH123"))
         (string1.1 (ascii:ub-subseq string1 1))
         (string1.1.1 (ascii:ub-subseq string1.1 1)))
         
    (assert (= (ascii:ub-char string1 1)
               (ascii:ub-char string1.1 0)))

    (assert (= (ascii:ub-char string1 2)
               (ascii:ub-char string1.1.1 0)))

    (assert (= (ascii:ub-char string1.1 1)
               (ascii:ub-char string1.1.1 0)))

    (assert (= (char-code #\c)
               (ascii:ub-char string1.1.1 0)))
    ) )
