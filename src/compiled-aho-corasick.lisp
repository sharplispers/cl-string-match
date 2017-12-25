;;; -*- package: CL-STRING-MATCH; Syntax: Common-lisp; Base: 10 -*-

;; Copyright (c) 2017, Victor Anyakin <anyakinvictor@yahoo.com> All
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
        (main-block (gensym "main-block"))
        (match-len (gensym "match-len")))

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
                 (map-trie-children
                     (node child)
                   (push
                    `(,(trie-node-label child)
                       (incf ,match-len)
                       (go ,(node-id->tag child)))
                    cases))
                 cases)))

      ;; fill tagbody with dispatches
      (trie-traverse-dfo
       trie
       #'(lambda (node)
           (push (node-id->tag node) dispatches)
           (if (trie-node-mark node)
               (push `(return-from ,main-block
                        ;; successfull match: return beginning of the
                        ;; matching pattern in the string and the mark
                        (values
                         (- (file-position ,strio) ,match-len)
                         ,(trie-node-mark node)))
                     dispatches)
               (push
                `(case (read-char ,strio nil #\Nul)
                   ,@(dispatch-node node)
                   (#\Nul (return-from ,main-block nil))
                   ;; when nothing matches, fallback to the failing
                   ;; transition, reset match-len counter
                   (otherwise
                    (setf ,match-len 0)
                    (go ,(node-id->tag (trie-node-fail node)))))
                dispatches)))))

    ;; assemble it all into one place
    `(lambda (,str)
       (block ,main-block
         (let ((,match-len 0))
           (with-input-from-string (,strio ,str)
             (tagbody ,@(reverse dispatches))))))))

;; --------------------------------------------------------

(defun trie->compiled-ac (trie)
  "Returns a compiled function for the given Aho-Corasick trie."
  (check-type trie trie-node)

  (compile nil (make-lambda-ac trie)))

;; --------------------------------------------------------

(defun search-compiled-ac (search-function txt)
  (funcall search-function txt))

;; EOF
