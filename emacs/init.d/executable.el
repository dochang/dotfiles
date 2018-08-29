;; https://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave

(req-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))
