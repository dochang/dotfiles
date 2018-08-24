;; https://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave

(req-package executable
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))
