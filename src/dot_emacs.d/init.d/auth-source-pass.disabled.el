;; Although we want to use the latest version, auth-source-pass depends on
;; Emacs 27.1 since [1].  Thus we have to load the builtin version which has
;; been included in Emacs 26 [2].  On Emacs 25 or below, it's not possible to
;; install it unfortunately.
;;
;; [1]: https://github.com/DamienCassou/auth-source-pass/commit/aa09e77551e1572b9331755459b557abf4d01ac3
;; [2]: https://github.com/DamienCassou/auth-source-pass/commit/3de8bbb51054f495f0363a3121f287b15e0d9049


(cond ((version< emacs-version "26.1")
       (req-package auth-source-pass
         :ensure auth-source-pass
         :hook (emacs-startup . auth-source-pass-enable)))
      ((version< emacs-version "27.1")
       (req-package auth-source-pass
         :ensure (auth-source-pass :pin :built-in)
         :hook (emacs-startup . auth-source-pass-enable)))
      (t
       (req-package auth-source-pass
         :ensure auth-source-pass
         :hook (emacs-startup . auth-source-pass-enable))))
