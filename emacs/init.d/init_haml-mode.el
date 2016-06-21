;;; Haml Mode
;; [[https://github.com/nex3/haml-mode]]

(defun $haml-mode-hook ()
  ($prog-mode-hook*))

(req-package haml-mode
  :init
  (add-hook 'haml-mode-hook '$haml-mode-hook))
