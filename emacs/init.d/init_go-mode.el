;;; Go Mode
;; [[http://golang.org/misc/emacs/]]

(defun $go-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1)
  (setq indent-tabs-mode t))

(defun $gofmt-before-save ()
  (interactive)
  (mapc (lambda (cmdline)
          (let ((gofmt-command (car cmdline))
                (gofmt-args (cdr cmdline)))
            (when (executable-find gofmt-command)
              (gofmt-before-save))))
        '(("goimports")
          ("gofmt" "-s"))))

(req-package go-mode
  :init
  (setq gofmt-command
        (cond ((executable-find "goimports") "goimports")
              (t "gofmt")))
  (add-hook 'before-save-hook '$gofmt-before-save)
  (add-hook 'go-mode-hook '$go-mode-hook))
