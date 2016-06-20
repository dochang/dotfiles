;;; Jade Mode
;; [[https://github.com/brianc/jade-mode]]

(defun $jade-mode-hook ()
  ($prog-mode-hook*))

(req-package jade-mode
  :init
  (add-hook 'jade-mode-hook '$jade-mode-hook))
