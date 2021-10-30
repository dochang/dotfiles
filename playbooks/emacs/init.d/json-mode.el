;;; JSON Mode

(defun $json-mode-hook ()
  ($run-prog-mode-hook)
  ;; prettier doesn't support JSON.  Disable it.
  (prettier-js-mode -1))

(req-package json-mode
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.babelrc\\'" . json-mode))
  :hook (json-mode . $json-mode-hook))
