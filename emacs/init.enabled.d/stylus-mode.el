;;; Stylus Mode
;; [[https://github.com/brianc/jade-mode]]

(defun $stylus-mode-hook ()
  ($run-prog-mode-hook)
  (rainbow-mode 1))

(req-package stylus-mode
  :hook (stylus-mode . $stylus-mode-hook))
