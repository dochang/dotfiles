;;; Go Mode
;; [[http://golang.org/misc/emacs/]]

(defun $go-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1)
  (setq indent-tabs-mode t))

(req-package go-mode
  :init
  (add-hook 'go-mode-hook '$go-mode-hook))
