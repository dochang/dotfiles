;;; Info

(defun $Info-mode-hook ()
  (scroll-lock-mode 1))

(defvar **default-Info-default-directory-list** Info-default-directory-list)

(req-package info

  :hook (Info-mode . $Info-mode-hook)

  :init

  ;; Put "~/local/share/info" before other dirs.
  ;;
  ;; Why not use `Info-directory-list'?  Because it is `nil' after info
  ;; loaded.  `info-initialize' initializes it based on
  ;; `Info-default-directory-list'.
  (setq Info-default-directory-list
        (cons (expand-file-name "~/local/share/info/")
              **default-Info-default-directory-list**)))
