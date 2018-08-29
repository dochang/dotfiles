;;; PHP Mode
;; [[https://github.com/ejmr/php-mode]]

(defun $php-mode-hook ()
  ($camel-case-mode 1))

(req-package php-mode
  :mode (("\\.php[s345t]?\\'" . php-mode)
         ("\\.phtml\\'" . php-mode)
         ("Amkfile" . php-mode)
         ("\\.amk$" . php-mode))
  :hook (php-mode . $php-mode-hook))
