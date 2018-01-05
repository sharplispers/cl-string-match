;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2017, 2018 Victor Anyakin <anyakinvictor@yahoo.com> All
;; rights reserved.

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

;;; Aho-Corasick algorithm implementation based on its "interpreted"
;;; version in the aho-corasick.lisp file. However, here the search
;;; function is created at the run-time and compiled using "compile"
;;; Lisp function.
;;
;; Based on description how to create compiled functions on the fly by
;; Peter Seibel:
;; http://gigamonkeys.wordpress.com/2007/07/27/compiling-queries-without-eval/
;;
;; Bitbucket ticket: #40

(in-package :cl-string-match)

;; --------------------------------------------------------

(defsection @aho-corasick-compiled-section (:title "Compiled to native code")
  "There is a yet another way to implement the Aho-Corasick algorithm:
translate the DFA defined by the Trie into a computer program and
compile it to machine code."
  (trie->compiled-ac function)
  (search-compiled-ac function)
  (string-contains-cac function))

;; --------------------------------------------------------

(defun make-lambda-ac (trie)
  "Writes lambda function performing search over the given trie.

That function can later be compiled into native code and
funcall-ed. The generated function accepts a single string and returns
matching mark from the given trie.

Generated function is essentially a giant tagbody with jumps
dispatched in case forms depending on the current character."

  (let ((tags nil)
        (dispatches nil)
        (str (gensym "arg"))
        (strio (gensym "strio"))
        (main-block (gensym "main-block")))

    ;; create a dictionary binding node-ids to tag symbols
    (trie-traverse-bfo
     trie
     #'(lambda (node)
         (push `(,(trie-node-id node) ,(gensym "node"))
               tags)))

    (labels ((node-id->tag (node)
               (second (assoc (trie-node-id node) tags)))

             (dispatch-node (node)
               (let ((cases nil))
                 (do-trie-children
                     (node child)
                   (push
                    `(,(trie-node-label child)
                       ;; forward transition, consume a character
                       (read-char ,strio nil :eof)
                       (go ,(node-id->tag child)))
                    cases))
                 cases)))

      ;; fill tagbody with dispatches
      (trie-traverse-dfo
       trie
       #'(lambda (node)
           (push (node-id->tag node) dispatches) ; tag label
           (if (trie-node-mark node)
               (push
                `(return-from ,main-block
                   ;; successfull match: return beginning of the
                   ;; matching pattern in the string and the mark
                   (values
                    (- (file-position ,strio) ,(trie-node-depth node))
                    (list ,@(trie-node-mark node))))
                dispatches)
               (push
                `(case (peek-char nil ,strio nil :eof)
                   ,@(dispatch-node node)
                   (:eof (return-from ,main-block nil))
                   ;; when nothing matches, fallback to the failing
                   ;; transition
                   (otherwise
                    ;; normal nodes don't consume chars by themselves,
                    ;; only upon a forward transition, but the root
                    ;; node fails on itself, therefore fail transition
                    ;; must consume a character to step over the text
                    ,(unless (trie-node-label node)
                             `(read-char ,strio nil :eof))
                    (go ,(node-id->tag (trie-node-fail node)))))
                dispatches)))))

    ;; assemble it all into one piece
    `(lambda (,str)
       (block ,main-block
         (with-input-from-string (,strio ,str)
           (tagbody ,@(reverse dispatches)))))))

;; --------------------------------------------------------

(defun trie->compiled-ac (trie)
  "Returns a compiled function for the given Aho-Corasick trie.

Should be used together with the INITIALIZE-AC function:

    (trie->compiled-ac (initialize-ac '(\"abc\")))
    ;; => #<FUNCTION (LAMBDA (#:|arg1315|)) {100468194B}>

Compiled function then can be used with the SEARCH-COMPILED-AC
function to search for matches in the text."

  (check-type trie trie-node)
  (let ((f (make-lambda-ac trie)))
    ;; (format t "~%~%function:~%~a~%~%" f)
    (compile nil f)))

;; --------------------------------------------------------

(defun search-compiled-ac (search-function txt)
  "Using a compiled trie, given as the SEARCH-FUNCTION, look for
matches in the given text TXT.

Returns start of the matching fragment and the matching mark from the
given trie."

  (funcall search-function txt))

;; --------------------------------------------------------

(defun string-contains-cac (pat txt)
  "Looks for the given pattern in the text and returns index of the
first occurence. Uses compiled Aho-Corasick search function over a
trie."

  (declare #.*standard-optimize-settings*)
  (check-type pat simple-string)
  (check-type txt simple-string)

  (cond
    ((= 0 (length pat))
     0)
    ((= 0 (length txt))
     nil)
    (t
     (search-compiled-ac (trie->compiled-ac (initialize-ac pat))
                         txt))))

;; EOF
