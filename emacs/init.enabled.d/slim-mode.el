;;; Slim Mode
;; [[https://github.com/slim-template/emacs-slim]]

(defun $slim-mode-hook ()
  ($run-prog-mode-hook))

(req-package slim-mode
  :hook (slim-mode . $slim-mode-hook))
