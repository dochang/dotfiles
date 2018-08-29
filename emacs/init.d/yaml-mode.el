;;; YAML Mode

(defun $yaml-mode-hook ()
  ($run-prog-mode-hook))

(req-package yaml-mode
  :mode (("\\.yamllint\\'" . yaml-mode))
  ;; Setup yaml-path
  ;;
  ;; `C-c C-p' is ok, because it's defined in a major mode keymap.
  :bind (:map yaml-mode-map
         ("C-c C-p" . yaml-path/path))
  :hook (yaml-mode . $yaml-mode-hook))
