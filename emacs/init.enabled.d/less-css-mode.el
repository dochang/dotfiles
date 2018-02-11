;;; LESS CSS Mode
;; [[http://www.emacswiki.org/emacs/LessCssMode]]
;; [[https://github.com/purcell/less-css-mode]]

(defun $less-css-mode-hook ()
  ;; No need to eval `$prog-mode-hook' if `less-css-mode' is derived from
  ;; `css-mode'.
  (unless (derived-mode-p 'css-mode)
    ($prog-mode-hook*))
  (rainbow-mode 1))

(req-package less-css-mode
  :init
  ;; Don't compile after saving.
  (setq less-css-compile-at-save nil)
  (add-hook 'less-css-mode-hook '$less-css-mode-hook))
