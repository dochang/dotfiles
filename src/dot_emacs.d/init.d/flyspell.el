;; Flyspell

(req-package flyspell
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :hook ((prog-mode . flyspell-prog-mode)
         ;; Enable Flyspell Prog Mode.  This invokes `(flyspell-mode 1)'.
         ;; Eval `(flyspell-mode -1)' to disable it.
         ((message-setup text-mode) . flyspell-mode))
  :init
  (setq flyspell-use-meta-tab nil))
