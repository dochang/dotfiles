(req-package visual-ascii-mode
  :hook ((help-mode helpful-mode) . visual-ascii-mode)
  :init
  (setq visual-ascii-mode-show-unicode t)
  (setq visual-ascii-mode-display-in-comment t)
  (setq visual-ascii-mode-show-unprintable-character t))
