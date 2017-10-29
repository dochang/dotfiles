(req-package executable
  :init
  ;; https://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))
