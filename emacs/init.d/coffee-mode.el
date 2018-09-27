;;; Coffee Mode
;; [[http://ozmm.org/posts/coffee_mode.html]]
;; [[https://github.com/defunkt/coffee-mode]]

(defun $coffee-mode-hook ()
  ($camel-case-mode 1))

(req-package coffee-mode
  :hook (coffee-mode . $coffee-mode-hook)
  :custom
  (coffee-tab-width 2))
