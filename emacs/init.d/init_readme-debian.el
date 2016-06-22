;;; README.Debian Mode

(defun $readme-debian-mode-hook ()
  ($prog-mode-hook*)
  (remove-hook 'write-contents-functions 'readme-debian-update-timestamp t))

(req-package readme-debian
  :loader :path
  :init
  (add-hook 'readme-debian-mode-hook '$readme-debian-mode-hook))