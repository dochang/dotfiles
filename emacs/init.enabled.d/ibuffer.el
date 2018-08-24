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

;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because if
;; should run only once when the buffer created.
(defun $ibuffer-mode-hook ()
  (ibuffer-auto-mode 1))

(req-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . $ibuffer-mode-hook))
