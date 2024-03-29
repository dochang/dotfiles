;;; Web Mode
;; [[http://web-mode.org/]]
;; [[https://github.com/fxbois/web-mode]]

(defun $web-mode-hook ()
  (cond ((and buffer-file-name
              (string-match "\\.jsx?\\'" buffer-file-name))
         ($js-mode-common-hook)))
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
         "\\.html?\\'")

  :hook (web-mode . $web-mode-hook)

  :init

  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-comment-formats
        '(("java"       . "//")
          ("javascript" . "//")
          ("php"        . "//")))
  (setq web-mode-enable-engine-detection t)
  (setq web-mode-enable-sql-detection t)

  :config

  (setq web-mode-engine-file-regexps
        (cons (cons "jinja" "\\.jinja2?\\'")
              web-mode-engine-file-regexps)))
