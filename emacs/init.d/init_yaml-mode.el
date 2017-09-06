;;; YAML Mode

(defun $yaml-mode-hook ()
  ($prog-mode-hook*)
  ;; aggressive-indent-mode will break the indentation.  Disable it.
  (aggressive-indent-mode -1))

(req-package yaml-mode
  ;; Setup yaml-path
  ;;
  ;; `C-c C-p' is ok, because it's defined in a major mode keymap.
  :bind (:map yaml-mode-map
         ("C-c C-p" . yaml-path/path))
  :init
  (add-hook 'yaml-mode-hook '$yaml-mode-hook))
