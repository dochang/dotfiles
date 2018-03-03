;;; Dockerfile Mode
;; [[https://github.com/spotify/dockerfile-mode]]

(defun $dockerfile-mode-hook ()
  ($prog-mode-hook*)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1))

(req-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :init
  (add-hook 'dockerfile-mode-hook '$dockerfile-mode-hook))
