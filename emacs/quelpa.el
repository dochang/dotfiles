(use-package quelpa
  :demand t
  :ensure t
  :init
  (setq quelpa-upgrade-p nil)
  ;; If `quelpa-upgrade-p' is `t', Emacs tries to upgrade quelpa packages when
  ;; the `use-package' macro is evaluated.  This causes Emacs connects to
  ;; remote sites every time it starts.  We can't wait for it.  Upgrade quelpa
  ;; packages manually please.
  (setq quelpa-update-melpa-p nil))

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
