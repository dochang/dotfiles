;;; Go Mode
;; [[http://golang.org/misc/emacs/]]

(defun $go-mode-hook ()
  ($camel-case-mode 1)
  ;; (add-hook 'before-save-hook '$gofmt-before-save 'append 'local)
  ;; Format source code by format-all.
  (set (make-local-variable 'company-backends) '(company-go)))

(defun $gofmt-before-save ()
  (interactive)
  (mapc (lambda (cmdline)
          (let ((gofmt-command (car cmdline))
                (gofmt-args (cdr cmdline)))
            (gofmt-before-save)))
        (cl-remove-if-not (lambda (cmdline)
                            (let ((gofmt-command (car cmdline)))
                              (executable-find gofmt-command)))
                          '(("goimports")
                            ("gofmt" "-s")))))

(req-package go-mode
  :bind (:map go-mode-map
         ("M-." . godef-jump)
         ("M-*" . pop-tag-mark))
  :hook (go-mode . $go-mode-hook)
  :init
  (setq gofmt-command
        (cond ((executable-find "goimports") "goimports")
              (t "gofmt"))))
