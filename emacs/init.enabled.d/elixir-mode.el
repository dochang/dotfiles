;;; Elixir Mode
;; [[https://github.com/elixir-lang/emacs-elixir]]

(defun $elixir-mode-hook ()
  ($run-prog-mode-hook))

(req-package elixir-mode
  :hook (elixir-mode . $elixir-mode-hook))
