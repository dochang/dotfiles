;; Flyspell

(req-package flyspell
  :loader :built-in
  :init
  (setq flyspell-use-meta-tab nil)
  :config
  (define-key flyspell-mode-map "\M-\t" nil))
