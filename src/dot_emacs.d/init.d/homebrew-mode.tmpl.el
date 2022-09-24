(req-package homebrew-mode
  :hook (emacs-startup . global-homebrew-mode)
  :init
  (cond ((eq system-type 'gnu/linux)
         ;; Install Linuxbrew into /home/linuxbrew
         ;;
         ;; https://github.com/Linuxbrew/brew/issues/762
         (setq homebrew-prefix "{{ dotfiles_linuxbrew_prefix }}")
         (setq homebrew-cache-dir "~/.cache/Homebrew"))
        ((eq system-type 'darwin)
         (setq homebrew-prefix "/usr/local")
         (setq homebrew-cache-dir "~/Library/Caches/Homebrew/")))
  (when (and (not (file-exists-p homebrew-prefix))
             (executable-find "brew"))
    ;; Because Homebrew and Linuxbrew are written in Ruby and sometimes Ruby is
    ;; a bit slow, do not call "brew --prefix" and "brew --cache" every time.
    ;; Run external command only if Homebrew or Linuxbrew is in a non-standard
    ;; location.
    (setq homebrew-prefix (shell-command-to-string "brew --prefix"))
    (setq homebrew-cache-dir (shell-command-to-string "brew --cache")))
  (setq homebrew-executable (concat homebrew-prefix "/bin/brew"))
  (setq homebrew-default-args '("--verbose")))
