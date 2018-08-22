(defun $lsp-css-enable ()
  (when (eq major-mode 'css-mode)
    ;; Only enable in strictly css-mode, not less-css-mode and scss-mode
    ;; (css-mode-hook fires for the two modes because they are derived from
    ;; css-mode)
    (lsp-css-enable)))

(req-package lsp-css
  :hook ((css-mode . $lsp-css-enable)
         (less-css-mode . lsp-less-enable)
         ((sass-mode scss-mode) . lsp-scss-enable)))
