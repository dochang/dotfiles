;;; Rust Mode
;; [[https://github.com/rust-lang/rust-mode]]

(defun $rust-mode-hook ()
  ($prog-mode-hook*)
  (cargo-minor-mode 1))

(req-package rust-mode
  :init
  (add-hook 'rust-mode-hook '$rust-mode-hook))
