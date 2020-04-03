(req-package visual-ascii-mode
  :hook ((help-mode helpful-mode prog-mode) . visual-ascii-mode)
  :custom
  (visual-ascii-mode-show-unicode t)
  (visual-ascii-mode-display-in-comment t)
  (visual-ascii-mode-show-unprintable-character t))
