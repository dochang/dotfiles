;;; auto-package-update
(define-advice apu--package-out-of-date-p (:around (fn &rest r) unavailable-packages-return-nil)
  "Non package.el packages (e.g. el-get packages) and Nonupgradeable packages
  are NOT up-to-date and NOT out-of-date."
  (let ((pkg (car r)))
    (and (package-installed-p pkg)
         (cadr (assq pkg package-archive-contents))
         (funcall fn pkg))))

(req-package auto-package-update
  :hook (emacs-startup . auto-package-update-maybe))
