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
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-comment-formats
   '(("java"       . "//")
     ("javascript" . "//")
     ("php"        . "//"))))
