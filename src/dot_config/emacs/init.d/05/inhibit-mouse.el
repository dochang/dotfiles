(defun $inhibit-mouse-mode-hook ()
  (when (bound-and-true-p inhibit-mouse-mode)
    (when (fboundp 'context-menu-mode)
      (context-menu-mode -1))
    ;; We have to disable `context-menu-mode' even if `inhibit-mouse-mode' is
    ;; enabled, because `context-menu-mode' would move the mouse even it's
    ;; disabled.
    (unless (and (eq window-system 'mac)
                 (bound-and-true-p mac-carbon-version-string))
      ;; Exclude macOS Carbon environments where pixel scrolling is natively
      ;; supported and does not require explicit activation.
      (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode -1)))))

(setup (:package inhibit-mouse)

  (add-hook 'inhibit-mouse-mode-hook #'$inhibit-mouse-mode-hook)

  (cond ((daemonp)
         (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode))
        (t
         (add-hook 'emacs-startup-hook #'inhibit-mouse-mode)))

  (with-eval-after-load 'inhibit-mouse

    (setopt inhibit-mouse-adjust-mouse-highlight t)

    (setopt inhibit-mouse-adjust-show-help-function t)

    )

  )
