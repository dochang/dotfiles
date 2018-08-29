;;; todotxt
;; [[https://github.com/rpdillon/todotxt.el]]

(defun $todotxt-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil)
  (view-mode -1))

(req-package todotxt
  :commands (todotxt)
  :mode ("/todo.txt\\'" . todotxt-mode)
  :hook (todotxt-mode . $todotxt-mode-hook)
  :init
  (setq todotxt-file (expand-file-name "~/todo/todo.txt")))
