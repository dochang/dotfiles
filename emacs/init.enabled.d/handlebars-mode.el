;;; Handlebars Mode
;; [[https://github.com/danielevans/handlebars-mode]]

(defun $handlebars-mode-hook ()
  (emmet-mode 1))

(req-package handlebars-mode
  :hook (handlebars-mode . $handlebars-mode-hook))
