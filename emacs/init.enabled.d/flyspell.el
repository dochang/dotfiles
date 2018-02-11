;; Flyspell

(req-package flyspell
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :init
  (setq flyspell-use-meta-tab nil))
