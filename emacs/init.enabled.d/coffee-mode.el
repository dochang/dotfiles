;;; Coffee Mode
;; [[http://ozmm.org/posts/coffee_mode.html]]
;; [[https://github.com/defunkt/coffee-mode]]

(defun $coffee-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1))

(req-package coffee-mode
  :init
  (setq coffee-tab-width 2)
  (add-hook 'coffee-mode-hook '$coffee-mode-hook))
