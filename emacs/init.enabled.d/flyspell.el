;; Flyspell

(req-package flyspell
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :hook ((message-setup text-mode) . flyspell-mode)
  :init
  (setq flyspell-use-meta-tab nil))
