;;; Sh Mode

(defun $sh-mode-hook ()
  (cond ((string-match "[.]zsh\\>" buffer-file-name)
         (sh-set-shell "zsh")))
  ($prog-mode-hook*)
  ;; Show Paren mode raises an error when typing `(`, `[' or `{`:
  ;;
  ;;     Error running timer `show-paren-function': (error "Lisp nesting exceeds `max-lisp-eval-depth'")
  ;;
  ;; Didn't get the reason now.  Disable it.  Since it's a global minor mode,
  ;; make the mode variable `show-paren-mode` buffer local first.
  (make-local-variable 'show-paren-mode)
  (show-paren-mode -1)
  (local-set-key "\C-j" 'reindent-then-newline-and-indent)
  (setq indent-tabs-mode t))

(req-package sh-script
  :loader :built-in

  :mode ("\\.zsh\\'" . sh-mode)

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
