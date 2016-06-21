;;; Slim Mode
;; [[https://github.com/slim-template/emacs-slim]]

(defun $slim-mode-hook ()
  ($prog-mode-hook*))

(req-package slim-mode
  :init
  (add-hook 'slim-mode-hook '$slim-mode-hook))
