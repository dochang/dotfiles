;;; Stylus Mode
;; [[https://github.com/brianc/jade-mode]]

(defun $stylus-mode-hook ()
  ($run-prog-mode-hook))

(req-package stylus-mode
  :hook (stylus-mode . $stylus-mode-hook))
