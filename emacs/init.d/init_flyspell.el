;; Flyspell

(req-package flyspell
  :loader :built-in
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :init
  (setq flyspell-use-meta-tab nil))
