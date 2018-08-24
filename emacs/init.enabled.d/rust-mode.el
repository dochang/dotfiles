;;; Rust Mode
;; [[https://github.com/rust-lang/rust-mode]]

(defun $rust-mode-hook ()
  ($run-prog-mode-hook)
  (cargo-minor-mode 1))

(req-package rust-mode
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook '$rust-mode-hook))
