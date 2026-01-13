(defun $xdg-appmenu-terminal-runner (app command)
  (let ((eat-kill-buffer-on-exit t))
    (xdg-appmenu-terminal-runner-eat app command)))

(setup xdg-appmenu

  (unless (package-installed-p 'xdg-appmenu)
    (package-vc-install '(xdg-appmenu :url "https://codeberg.org/akib/emacs-xdg-appmenu")))

  (with-eval-after-load 'xdg-appmenu

    (setopt xdg-appmenu-terminal-runner '$xdg-appmenu-terminal-runner)

    )

  )
