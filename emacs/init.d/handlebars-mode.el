;;; Handlebars Mode
;; [[https://github.com/danielevans/handlebars-mode]]

(defun $handlebars-mode-hook ()
  (emmet-mode 1))

(req-package handlebars-mode
  :init
  (add-hook 'handlebars-mode-hook '$handlebars-mode-hook))
