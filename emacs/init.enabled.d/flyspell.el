;; Flyspell

(req-package flyspell
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :hook ((message-setup) . flyspell-mode)
  :init
  (setq flyspell-use-meta-tab nil))
