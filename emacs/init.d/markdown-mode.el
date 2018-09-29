;;; Markdown Mode
;; [[http://jblevins.org/projects/markdown-mode/]]

(defun $markdown-reset-auto-mode-alist ()
  ;; Remove all `markdown-mode' related entries inserted by autoload code in
  ;; `auto-mode-alist'.
  (setq auto-mode-alist
        (cons (cons "\\.\\(markdown\\|md\\|mdo?wn\\|mkdn?\\)\\'"
                    'markdown-mode)
              (rassq-delete-all 'markdown-mode auto-mode-alist))))

(defun $markdown-mode-hook ()
  ;; By default `markdown-mode' wants to handle `*.text' files (by autoloads),
  ;; but I want to open them by `text-mode'.
  ;;
  ;; Because loading autoloads is the final step of package initialization, any
  ;; configuration will be overriden.  This code has to be here rather than in
  ;; `eval-after-load'.
  (if ($file-name-match "\\.text\\'" buffer-file-name)
      (progn
        ($markdown-reset-auto-mode-alist)
        (set-auto-mode t))
    (setq indent-tabs-mode nil)))

(req-package markdown-mode
  :hook (markdown-mode . $markdown-mode-hook)
  :custom
  (markdown-unordered-list-item-prefix "  - "))
