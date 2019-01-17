;;; Ido Mode
;; [[http://www.emacswiki.org/emacs/InteractivelyDoThings]]
;; [[http://ergoemacs.org/emacs/emacs_icomplete_vs_ido.html]]
;; [[http://ergoemacs.org/emacs/emacs_iswitch_vs_ido.html]]

(req-package ido
  :custom
  (ido-confirm-unique-completion t)
  ;; Disable automatic file search in ido mode
  ;;
  ;; [[http://stackoverflow.com/a/18089076]]
  (ido-auto-merge-work-directories-length -1))
