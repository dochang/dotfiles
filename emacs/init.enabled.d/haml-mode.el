;;; Haml Mode
;; [[https://github.com/nex3/haml-mode]]

(defun $haml-mode-hook ()
  ($run-prog-mode-hook))

(req-package haml-mode
  :hook (haml-mode . $haml-mode-hook))
