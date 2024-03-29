(req-package quelpa
  ;; force: Eval the form before `req-package-finish'.
  :force t
  ;; demand: Load the package when the form loaded.
  :demand t
  ;; ensure: Ensure the package installed when the form loaded.
  :ensure t

  ;; :hook (emacs-startup . quelpa-upgrade-all-maybe)
  ;;
  ;; Don't run `quelpa-upgrade-all-maybe' at startup.  I don't want to wait for
  ;; it at startup.

  :init

  (setq quelpa-upgrade-interval 7)
  ;; Be consistent with `auto-package-update-interval' but don't use it
  ;; directly since it may not be set at this moment.

  (setq quelpa-upgrade-p nil)
  ;; If `quelpa-upgrade-p' is `t', Emacs tries to upgrade quelpa packages when
  ;; the `use-package' macro is evaluated.  This causes Emacs connects to
  ;; remote sites every time it starts.  We can't wait for it.  Upgrade quelpa
  ;; packages manually please.

  (setq quelpa-self-upgrade-p t)
  ;; Upgrade quelpa itself when doing a `quelpa-upgrade-all'.

  (setq quelpa-update-melpa-p nil)
  ;; Do not update MELPA git repo every time quelpa initialized.  Only update
  ;; the repo on `quelpa-upgrade' or `quelpa-self-upgrade'.

  )

(define-advice quelpa-build--build-single-file-package (:around (fn &rest r) dont-update-time-stamp-and-copyright)
  "This function runs `before-save-hook'.  Since installing package is a
background operation, we must skip the hooks which modify the file and keep the
file unmodified.

The call stack:

`quelpa'
-> `quelpa-build--build-single-file-package'
-> `write-file'
-> `save-buffer'
-> `basic-save-buffer'
-> `before-save-hook'
"
  (let ((time-stamp-active nil)
        (copyright-update nil))
    (apply fn r)))
