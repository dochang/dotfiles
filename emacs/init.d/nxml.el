;;; nXML Mode

(defun $nxml-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1))

(req-package nxml
  :hook (nxml-mode . $nxml-mode-hook)
  :config
  ;; Make nXML indentation variables safe as file local variables if
  ;; their values satisfy the predicate `integerp'.
  (put 'nxml-child-indent 'safe-local-variable 'integerp)
  (put 'nxml-outline-child-indent 'safe-local-variable 'integerp)
  (put 'nxml-attribute-indent 'safe-local-variable 'integerp))
