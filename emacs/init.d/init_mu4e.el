(defun $mu4e-compose-mode-hook ()
  (save-excursion
    (let ((bcc-addr (message-field-value "From")))
      (message-add-header (format "Bcc: %s" bcc-addr)))))

(req-package mu4e
  :loader :path
  :init
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/"))
  (add-hook 'mu4e-compose-mode-hook '$mu4e-compose-mode-hook))
