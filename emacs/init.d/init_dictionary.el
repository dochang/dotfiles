;;; Dictionary Mode
;; [[http://www.myrkr.in-berlin.de/dictionary/]]
;; [[https://github.com/myrkr/dictionary-el]]

(defun $dictionary-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package dictionary
  :bind (("C-c d s" . dictionary-search)
         ("C-c d m" . dictionary-match-words)
         ("C-c d d" . dictionary-lookup-definition)
         :map dictionary-mode-map
         ("DEL" . scroll-down))
  :init
  (setq dictionary-use-single-buffer t)
  (setq dictionary-tooltip-dictionary "!")
  (add-hook 'dictionary-mode-hook '$dictionary-mode-hook)
  :config
  (global-dictionary-tooltip-mode 1))
