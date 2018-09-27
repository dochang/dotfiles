;;; WoMan

;; How to enable `scroll-lock-mode'?
;;
;; Since `woman-mode' runs `Man-mode', we can enable
;; `scroll-lock-mode' in `Man-mode-hook'.

(defvar **default-woman-manpath**)

(req-package woman

  :custom

  ;; Use most of the frame width.
  ;; Override the value of `woman-fill-column'.
  ;; [[info:woman#Formatting%20Options]
  (woman-fill-frame t)

  ;; Don't use a dedicated frame for displaying woman mode.
  (woman-use-own-frame nil)

  ;; Unset `woman-locale' if locale is not "C".
  (woman-locale nil)

  :config
  (unless (boundp '**default-woman-path**)
    (setq **default-woman-path** woman-manpath))
  ;; Put "~/local/share/man" at the beginning of `woman-manpath'
  (setq woman-manpath
        (cons (cons (expand-file-name "~/local/bin")
                    (expand-file-name "~/local/share/man"))
              **default-woman-path**)))
