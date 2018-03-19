;;; Fibonacci strings

(in-package :cl-string-match)

;; --------------------------------------------------------


(defun gen-fibstring (n s0 s1)
  "A simple generation of Fibonacci strings (or words)."
  (let ((fib 
  (iter
    (with fib = "")
    (for i from a below n)
    (setf fib
)
)
