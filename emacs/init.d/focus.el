(req-package focus
  :hook (((prog-mode text-mode) . focus-mode)
         ((org-mode sh-mode yaml-mode ruby-mode) . (lambda () (focus-mode -1))))
  :config
  (make-variable-buffer-local 'focus-current-thing)
  (make-variable-buffer-local 'focus-buffer)
  (make-variable-buffer-local 'focus-pre-overlay)
  (make-variable-buffer-local 'focus-post-overlay)
  (make-variable-buffer-local 'focus-read-only-blink-timer)
  ;; When the second time `focus-init' runs, the default values of
  ;; `focus-pre-overlay' and `focus-post-overlay' will be set to their buffer
  ;; local values.
  ;;
  ;; This is because of the misuse of `make-local-variable'.  Use
  ;; `make-variable-buffer-local' instead.
  )
