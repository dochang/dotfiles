;;; Clojure Mode
;; [[http://www.emacswiki.org/emacs/ClojureMode]]
;; [[https://github.com/clojure-emacs/clojure-mode]]

(defun $clojure-mode-hook ()
  ($lisp-mode-common-hook))

(req-package clojure-mode
  :hook (clojure-mode . $clojure-mode-hook))
