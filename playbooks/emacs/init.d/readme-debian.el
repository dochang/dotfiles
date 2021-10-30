;;; README.Debian Mode

(defun $readme-debian-mode-hook ()
  ($run-prog-mode-hook)
  (remove-hook 'write-contents-functions 'readme-debian-update-timestamp t))

(req-package readme-debian
  :ensure (dpkg-dev-el :pin :external)
  :hook (readme-debian-mode . $readme-debian-mode-hook))
