(defun $typescript-mode-hook ()
  ($js-mode-common-hook))

(req-package typescript-mode
  :hook (typescript-mode . $typescript-mode-hook))
