;;; JSON Mode

(defun $json-mode-hook ()
  ($prog-mode-hook*))

(req-package json-mode
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.babelrc\\'" . json-mode))
  :init
  (add-hook 'json-mode-hook '$json-mode-hook))
