(eval-and-compile
  (require 'use-package))

(use-package load-dir
  :demand t
  :ensure t
  :init
  (setq load-dirs (locate-user-emacs-file "init.d")))
