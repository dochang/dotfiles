;;; JSON Mode

(defun $json-mode-hook ()
  ($prog-mode-hook*)
  ;; prettier doesn't support JSON.  Disable it.
  (prettier-js-mode -1))

(req-package json-mode
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.babelrc\\'" . json-mode))
  :init
  (add-hook 'json-mode-hook '$json-mode-hook))
