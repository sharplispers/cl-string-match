
;; run:
;;
;; sbcl --load mkapi

(ql:quickload "atdoc")
(ql:quickload "cl-string-match")

(atdoc:generate-html-documentation
 '(:cl-string-match)
 "./"
 :index-title "Lisp Subsequence Search API reference"
 :heading "Subsequence search"
 :single-page-p t			;optional
 :include-internal-symbols-p nil)


;; EOF