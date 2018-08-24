;;; Erlang Mode
;; [[http://melpa.milkbox.net/#/erlang]]

(defun $erlang-mode-hook ()
  ($run-prog-mode-hook))

(req-package erlang
  :mode ("\\.erl$" . erlang-mode)
  :hook (erlang-mode . $erlang-mode-hook))
