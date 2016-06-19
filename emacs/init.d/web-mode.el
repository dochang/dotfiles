;;; Web Mode
;; [[http://web-mode.org/]]
;; [[https://github.com/fxbois/web-mode]]

(defun $web-mode-hook ()
  ($prog-mode-hook*)
  (emmet-mode 1)
  ($camel-case-mode 1))

(req-package web-mode
  :mode (;; Uncomment any of the following if needed.
         ;;
         ;; "\\.phtml\\'"
         ;; "\\.tpl\\.php\\'"
         ;; "\\.[agj]sp\\'"
         ;; "\\.as[cp]x\\'"
         ;; "\\.erb\\'"
         ;; "\\.mustache\\'"
         ;; "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.jsx\\'"
         "\\.js\\'")
  :init
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-comment-formats
        '(("java"       . "//")
          ("javascript" . "//")
          ("php"        . "//")))

  (add-hook 'web-mode-hook '$web-mode-hook))
