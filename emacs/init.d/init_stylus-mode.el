;;; Stylus Mode
;; [[https://github.com/brianc/jade-mode]]

(defun $stylus-mode-hook ()
  ($prog-mode-hook*)
  (rainbow-mode 1))

(req-package stylus-mode
  :init
  (add-hook 'stylus-mode-hook '$stylus-mode-hook))
