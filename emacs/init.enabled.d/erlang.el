;;; Erlang Mode
;; [[http://melpa.milkbox.net/#/erlang]]

(defun $erlang-mode-hook ()
  ($run-prog-mode-hook))

(req-package erlang
  :mode ("\\.erl$" . erlang-mode)
  :init
  (add-hook 'erlang-mode-hook '$erlang-mode-hook))
