;;; Sh Mode

(defun $sh-mode-hook ()
  (cond ((string-match "[.]zsh\\>" buffer-file-name)
         (sh-set-shell "zsh")))
  (setq indent-tabs-mode t))

(req-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)

  :bind (:map sh-mode-map
         ("C-j" . reindent-then-newline-and-indent))

  :hook (sh-mode . $sh-mode-hook)

  :custom
  ;; Indentation
  ;; [[https://keramida.wordpress.com/2008/08/08/tweaking-shell-script-indentation-in-gnu-emacs/]]
  (sh-indentation tab-width)
  (sh-basic-offset tab-width)
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  ;; Use `/bin/sh` for shell scripts.
  (sh-shell-file "/bin/sh")

  :config
  (setq sh-alias-alist (cons (cons 'zsh5 'zsh)
                             (assq-delete-all 'zsh5 sh-alias-alist)))
  ;; The default argument of zsh is `-f`.  In fact, zsh won't source all
  ;; rcfiles except `.zshenv` when zsh runs in non-interactive mode.  So no
  ;; need to put `-f` here.
  (setq sh-shell-arg (cons (cons 'zsh '())
                           (assq-delete-all 'zsh sh-shell-arg))))
