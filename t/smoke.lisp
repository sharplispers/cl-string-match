;;; -*- package: CL-STRING-MATCH-TEST; Syntax: Common-lisp; Base: 10 -*-


;; Running tests from the command line:
;;
;; sbcl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(test:run)' --quit
;; lx86cl --eval '(ql:quickload "CL-STRING-MATCH-TEST")' --eval '(test:run)' --eval '(quit)'

;; --------------------------------------------------------


(in-package :cl-string-match-test)

(setq *print-failures* t)

;; --------------------------------------------------------

(defparameter *funcs*
  '(sm:string-contains-brute
    sm:string-contains-bm
    sm:string-contains-bmh
    sm:string-contains-bmh8
    sm:string-contains-rk
    sm:string-contains-kmp
    sm:string-contains-ac))

;; --------------------------------------------------------

(defmacro run-assertions (val needle haystack)
  `(progn ,@(loop :for func :in *funcs*
	       :collect `(assert-equal ,val (,func ,needle ,haystack)))))

;; --------------------------------------------------------

(define-test basic-test
    ;; empty sequences
  (run-assertions 0   "" "")
  (run-assertions nil "a" "")
  ;; one-letter sequences
  (run-assertions nil "a" "b")
  (run-assertions 0   "a" "a")
  ;; longer sequences
  (run-assertions 0 "a" "a--")
  (run-assertions 1 "a" "-a-")
  (run-assertions 2 "a" "--a")
  (run-assertions nil "a" "-b-"))

;; --------------------------------------------------------

(define-test str-test
  (run-assertions 0 "abc" "abcab_")
  (run-assertions 1 "abc" "_abcab_")
  (run-assertions 2 "abc" "ababc")
  (run-assertions 5 "GCAGAGAG" "GCATCGCAGAGAGTATACAGTACG")
  (run-assertions 14 "abracadabra" "abacadabrabracabracadabrabrabracad")
  (run-assertions 8 "rab" "abacadabrabracabracadabrabrabracad")
  (run-assertions nil "bcara" "abacadabrabracabracadabrabrabracad")
  (run-assertions 23 "rabrabracad" "abacadabrabracabracadabrabrabracad")
  (run-assertions 0 "abacad" "abacadabrabracabracadabrabrabracad")
  )

;; --------------------------------------------------------

(define-test window-test
  "Test how well the start and begin windows over the search pattern
and the text works."

  (assert-equal 1 (sm:string-contains-brute "-abc-" "_abcab_"
                                            :start1 1 :end1 3
                                            :start2 1 :end2 5)))

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

(define-test tree-test
  "Test generic tree operations."
  (let* ((tree (sm:make-suffix-tree :str "cacao"
                                    :root (sm:make-ukk-node)))
         (first-child (sm:suffix-node.add-child tree (sm:suffix-tree.root tree)
                                                0 sm:+infinity+)))

    (assert-equal (sm:suffix-tree.str tree)
                  (sm:suffix-node.str tree first-child))))

;; --------------------------------------------------------

(define-test simple-tree
  "Test simple (naive) suffix tree construction implementation."
  (let ((banana-tree (sm:suffix-tree.build-from-sexp
                      "banana$"
                      `((6 7)
                        (0 7)
                        (1 1 ((6 7)
                              (2 3 ((6 7)
                                    (4 7)))))
                        (2 3 ((6 7)
                              (4 7))))))

        (cacao-tree (sm:suffix-tree.build-from-sexp
                     "cacao"
                     `((4 5)
                       (0 1 ((2 5)
                             (4 5)))
                       (1 1 ((2 5)
                             (4 5))))))

        (mississippi-tree (sm:suffix-tree.build-from-sexp
                           "mississippi"
                           `((0 11)
                             (1 1 ((2 4 ((5 11)
                                         (8 11)))
                                   (8 11)))
                             (2 2 ((3 4 ((8 11)
                                         (5 11)))
                                   (4 4 ((8 11)
                                         (5 11)))))
                             (8 8 ((10 11)
                                   (9 11))))))

        (cdddcdc-tree (sm:suffix-tree.build-from-sexp
                       "cdddcdc"
                       `((0 1 ((6 7)
                               (2 7)))
                         (1 1 ((4 7)
                               (2 2 ((4 7)
                                     (3 7)))))))))

    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-simple "banana$")
                                        banana-tree))
    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-simple "cacao")
                                        cacao-tree))
    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-simple "mississippi")
                                        mississippi-tree))
    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-simple "cdddcdc")
                                        cdddcdc-tree))))

;; --------------------------------------------------------

(define-test ukk-tree
  "Test Ukkonens suffix tree construction implementation."

  (let ((banana-tree (sm:suffix-tree.build-from-sexp
                      "banana$"
                      `((6 ,sm:+infinity+)
                        (0 ,sm:+infinity+)
                        (1 1 ((6 ,sm:+infinity+)
                              (2 3 ((6 ,sm:+infinity+)
                                    (4 ,sm:+infinity+)))))
                        (2 3 ((6 ,sm:+infinity+)
                              (4 ,sm:+infinity+))))))

        (cacao-tree (sm:suffix-tree.build-from-sexp
                     "cacao"
                     `((4 ,sm:+infinity+)
                       (0 1 ((2 ,sm:+infinity+)
                             (4 ,sm:+infinity+)))
                       (1 1 ((2 ,sm:+infinity+)
                             (4 ,sm:+infinity+))))))

        (mississippi-tree (sm:suffix-tree.build-from-sexp
                           "mississippi"
                           `((0 ,sm:+infinity+)
                             (1 1 ((2 4 ((5 ,sm:+infinity+)
                                         (8 ,sm:+infinity+)))
                                   (8 ,sm:+infinity+)))
                             (2 2 ((3 4 ((8 ,sm:+infinity+)
                                         (5 ,sm:+infinity+)))
                                   (4 4 ((8 ,sm:+infinity+)
                                         (5 ,sm:+infinity+)))))
                             (8 8 ((10 ,sm:+infinity+)
                                   (9 ,sm:+infinity+))))))

        (cdddcdc-tree (sm:suffix-tree.build-from-sexp
                       "cdddcdc"
                       `((0 1 ((6 ,sm:+infinity+)
                               (2 ,sm:+infinity+)))
                         (1 1 ((4 ,sm:+infinity+)
                               (2 2 ((4 ,sm:+infinity+)
                                     (3 ,sm:+infinity+)))))))))

    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "banana$")
                                        banana-tree))

    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "cacao")
                                        cacao-tree))

    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "mississippi")
                                        mississippi-tree))

    (assert-true (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "cdddcdc")
                                        cdddcdc-tree))

    ;; now let's verfiy that suffix-tree.equals does not produce false
    ;; positives
    (assert-false (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "cacao")
                                         banana-tree))

    (assert-false (sm:suffix-tree.equals (sm:build-suffix-tree-ukkonen "banana")
                                         mississippi-tree))

    ;; ana in banana : 2
    ;; an in banana : 2
    ;; anan in banana : 1
    ;; nana in banana : 1
    ;; ananan in banana : 0

    ))

;; --------------------------------------------------------

(defun run ()
  (lisp-unit:run-tests :all (find-package 'cl-string-match-test)))

;; EOF
