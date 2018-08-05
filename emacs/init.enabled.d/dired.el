;;; Dired Mode
(defun $dired-load-hook ()
  (define-key dired-mode-map "E" 'emms-play-dired)
  ;; use "K" to kill dired buffer.
  (unless (lookup-key dired-mode-map "K")
    (define-key dired-mode-map "K" 'kill-this-buffer))
  (setq dired-listing-switches "-lhA"
        dired-dwim-target t
        ;; `find-ls-option' defaults to '("-ls" . "-dilsb").  Because
        ;; `-ls' quotes non-printable characters in file names using
        ;; C-like backslash escapes, We have to pass custom parameters
        ;; to `ls' here.
        find-ls-option '("-exec ls -lhAd {} +" . "-lhAd")
        ;; `find-ls-subdir-switches' should default to the value of
        ;; `dired-listing-switches'.  DO NOT set it to `nil',
        ;; otherwise `dired-subdir-switches' in `*Find*' buffer is
        ;; `nil', then `(cdr find-ls-option)' will be used.
        find-ls-subdir-switches dired-listing-switches
        ;; `locate-ls-subdir-switches` should default to the value of
        ;; `dired-listing-switches'.
        locate-ls-subdir-switches dired-listing-switches)
  (require 'dired-x)
  (require 'git-annex)
  (require 'dired-filetype-face)
  ;; Load it after any other dired extensions, so that its key binding
  ;; overrides others.
  (require 'dired+)
  (dired-async-mode 1))

(defun $dired-mode-hook ()
  (dired-omit-mode -1))

(req-package dired
  :ensure nil
  :init
  (add-hook 'dired-mode-hook '$dired-mode-hook)
  :config
  ($dired-load-hook))
