;;; Groovy Mode
;; - [[http://groovy.codehaus.org/Emacs+Groovy+Mode]]
;; - [[https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes]]

(defun $groovy-mode-hook ()
  ($camel-case-mode 1))

(req-package groovy-mode
  ;; Also edit gradle files by `groovy-mode'.
  :mode ("\\.gradle\\'" . groovy-mode)
  :hook (groovy-mode . $groovy-mode-hook))
