;;; OfflineIMAP
;;; [[http://julien.danjou.info/projects/emacs-packages#offlineimap]]

(req-package offlineimap
  :init
  ;; Display the action as a text instead of a single symbol.
  (setq offlineimap-mode-line-style 'text))
