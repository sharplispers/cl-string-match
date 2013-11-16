;;; -*- package: CL-STRING-MATCH-TEST; Syntax: Common-lisp; Base: 10 -*-


;; Running tests from the command line:
;; 
;; sbcl --load smoke.lisp --eval '(test:run)' --quit
;; lx86cl --load smoke.lisp --eval '(test:run)' --eval '(quit)'

;; --------------------------------------------------------


(in-package :cl-string-match-test)

(setq *print-failures* t)

;; --------------------------------------------------------

(defparameter *funcs*
  '(sm:string-contains-brute
    sm:string-contains-bm
    sm:string-contains-bmh
    sm:string-contains-rk
    sm:string-contains-kmp
    sm:string-contains-ac))

;; --------------------------------------------------------

(defmacro run-assertions (val needle haystack)
  `(progn ,@(loop :for func :in *funcs*
	       :collect `(assert-equal ,val (,func ,needle ,haystack)))))

;; --------------------------------------------------------

(define-test basic-test
  (run-assertions 0 "a" "a--")
  (run-assertions 1 "a" "-a-")
  (run-assertions 2 "a" "--a")
  (run-assertions nil "a" "-b-"))

;; --------------------------------------------------------

(define-test str-test
  (run-assertions 0 "abc" "abcab_")
  (run-assertions 1 "abc" "_abcab_")
  (run-assertions 2 "abc" "ababc"))

;; --------------------------------------------------------

(define-test ac-test
    ;; test Aho-Corasick implementation how it deals with multiple
    ;; patterns search

    (let ((trie (initialize-ac '("he" "she" "his" "hers"))))
      (assert-equal 0 (search-ac trie "she"))
      (assert-equal 1 (search-ac trie "_she"))
      (assert-equal nil (search-ac trie "_sh_"))
      
      (multiple-value-bind (pos idx)
	  (search-ac trie "___his")
	(assert-equal 3 pos)
	(assert-equal 2 idx))
      (multiple-value-bind (pos idx)
	  (search-ac trie "___h_s")
	(assert-equal nil pos)
	(assert-equal nil idx))))

;; --------------------------------------------------------

(define-test ukk-tree
    (build-suffix-tree-ukkonen "cocoa$")
  (build-suffix-tree-ukkonen "banana$")

ana in banana : 2
an in banana : 2
anan in banana : 1
nana in banana : 1
ananan in banana : 0

    )

;; --------------------------------------------------------

(defun run ()
  (lisp-unit:run-all-tests :cl-string-match-test))

;; EOF