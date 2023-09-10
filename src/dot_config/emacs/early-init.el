(load (locate-user-emacs-file "vars"))
(load (locate-user-emacs-file "packages"))


(define-advice guix-emacs-find-autoloads (:before-while (directory) skip-nonexist)
  (file-directory-p directory))
;; `guix-emacs-find-autoloads' will raise an error on a nonexisting directory.
;; This will break initialization.  Ignore them.
