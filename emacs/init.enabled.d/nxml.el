;;; nXML Mode

(defun $nxml-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1)
  (setq indent-tabs-mode nil))

(req-package nxml
  :init
  (add-hook 'nxml-mode-hook '$nxml-mode-hook)
  :config
  ;; Make nXML indentation variables safe as file local variables if
  ;; their values satisfy the predicate `integerp'.
  (put 'nxml-child-indent 'safe-local-variable 'integerp)
  (put 'nxml-outline-child-indent 'safe-local-variable 'integerp)
  (put 'nxml-attribute-indent 'safe-local-variable 'integerp))
