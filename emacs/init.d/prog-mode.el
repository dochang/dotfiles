;;; Prog Mode

(defun $prog-mode-hook ()
  ;; Do not insert tabs in indentation by default.
  ;;
  ;; NOTE: Setting `indent-tabs-mode' to `t' does NOT mean "pressing `TAB'
  ;; inserts a `\t'".
  (setq indent-tabs-mode nil))

(req-package prog-mode
  :ensure (prog-mode :pin :built-in)
  :hook (prog-mode . $prog-mode-hook))
