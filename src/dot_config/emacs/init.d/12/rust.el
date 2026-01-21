;;; Rust Mode
;; [[https://github.com/rust-lang/rust-mode]]

(setup (:package rust-mode)

  (with-eval-after-load 'rust-rustfmt

    (setopt rust-format-on-save t)

    )

  )
