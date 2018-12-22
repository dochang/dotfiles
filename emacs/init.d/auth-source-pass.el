;; auth-source-pass has been included in Emacs 26.  But we want to use the
;; latest version.
;;
;; https://github.com/DamienCassou/auth-password-store/blob/382faa2612f876f16bd53497d332148b98f840a8/README.md#installing

(req-package auth-source-pass
  :hook (emacs-startup . auth-source-pass-enable))
