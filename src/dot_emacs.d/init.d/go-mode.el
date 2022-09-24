;;; Go Mode
;; [[http://golang.org/misc/emacs/]]

(defun $go-mode-hook ()
  ($camel-case-mode 1)
  ;; (add-hook 'before-save-hook 'gofmt-before-save 'append 'local)
  (setq format-all-formatters '(("Go" goimports)))
  ;; Format source code by format-all.
  (set (make-local-variable 'company-backends) '(company-go)))

(req-package go-mode
  :bind (:map go-mode-map
         ("M-." . godef-jump)
         ("M-*" . pop-tag-mark))
  :hook (go-mode . $go-mode-hook)
  :init
  (cond ((executable-find "goimports")
         (setq gofmt-command "goimports"
               gofmt-args nil))
        (t
         (setq gofmt-command "gofmt"
               gofmt-args '("-s")))))
