;;; PHP Mode
;; [[https://github.com/ejmr/php-mode]]

(defun $php-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1))

(req-package php-mode
  :mode (("\\.php[s345t]?\\'" . php-mode)
         ("\\.phtml\\'" . php-mode)
         ("Amkfile" . php-mode)
         ("\\.amk$" . php-mode))
  :init
  (add-hook 'php-mode-hook '$php-mode-hook))
