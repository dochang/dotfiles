;;; SGML Mode

(defun $sgml-mode-hook ()
  (emmet-mode 1))

(req-package sgml-mode
  :hook (sgml-mode . $sgml-mode-hook))
