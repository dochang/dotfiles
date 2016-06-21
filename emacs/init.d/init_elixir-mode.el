;;; Elixir Mode
;; [[https://github.com/elixir-lang/emacs-elixir]]

(defun $elixir-mode-hook ()
  ($prog-mode-hook*))

(req-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook '$elixir-mode-hook))
