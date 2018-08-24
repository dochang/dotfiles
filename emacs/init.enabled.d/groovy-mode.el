;;; Groovy Mode
;; - [[http://groovy.codehaus.org/Emacs+Groovy+Mode]]
;; - [[https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes]]

(defun $groovy-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1)
  (groovy-electric-mode 1))

(req-package groovy-mode
  ;; Also edit gradle files by `groovy-mode'.
  :mode ("\\.gradle\\'" . groovy-mode)
  :init
  (add-hook 'groovy-mode-hook '$groovy-mode-hook))
