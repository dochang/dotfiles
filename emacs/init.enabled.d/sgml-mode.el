;;; SGML Mode

(defun $sgml-mode-hook ()
  (emmet-mode 1))

(req-package sgml-mode
  :init
  (add-hook 'sgml-mode-hook '$sgml-mode-hook))
