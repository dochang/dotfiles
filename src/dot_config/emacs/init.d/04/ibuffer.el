;;; IbufferMode
;;; [[http://www.emacswiki.org/emacs/IbufferMode]]
;;
;; * Ibuffer hooks
;;
;; ** `ibuffer-load-hook'
;;
;;    run once after ibuffer loaded.
;;
;; ** `ibuffer-mode-hook'
;;
;;    run when a buffer goes into `ibuffer-mode'.
;;
;; ** `ibuffer-hook'
;;
;;    run when executing `ibuffer' even if "*Ibuffer*" exists.

(setup (:package ibuffer)

  (defun $ibuffer-mode-hook ()
    ;; Do not wrap lines in `ibuffer-mode'.
    (visual-line-mode -1)
    (toggle-truncate-lines 1))

  (keymap-set ctl-x-map "C-b" #'ibuffer)

  (:with-mode (ibuffer-mode)
    (:hook $ibuffer-mode-hook)
    (:hook ibuffer-auto-mode)
    ;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because it should
    ;; run only once when the buffer created.
    )

  )
