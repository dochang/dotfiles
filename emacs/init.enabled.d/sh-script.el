;;; Sh Mode

(defun $sh-mode-hook ()
  (cond ((string-match "[.]zsh\\>" buffer-file-name)
         (sh-set-shell "zsh")))
  ($prog-mode-hook*)
  (setq indent-tabs-mode t))

(req-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)

  :bind (:map sh-mode-map
         ("C-j" . reindent-then-newline-and-indent))

  :init
  ;; Indentation
  ;; [[https://keramida.wordpress.com/2008/08/08/tweaking-shell-script-indentation-in-gnu-emacs/]]
  (setq sh-indentation tab-width)
  (setq sh-basic-offset tab-width)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  ;; Use `/bin/sh` for shell scripts.
  (setq sh-shell-file "/bin/sh")
  (add-hook 'sh-mode-hook '$sh-mode-hook)

  :config
  (setq sh-alias-alist (cons (cons 'zsh5 'zsh)
                             (assq-delete-all 'zsh5 sh-alias-alist)))
  ;; The default argument of zsh is `-f`.  In fact, zsh won't source all
  ;; rcfiles except `.zshenv` when zsh runs in non-interactive mode.  So no
  ;; need to put `-f` here.
  (setq sh-shell-arg (cons (cons 'zsh '())
                           (assq-delete-all 'zsh sh-shell-arg))))
