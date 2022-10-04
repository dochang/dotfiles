;; Configure guix-profiles only if emacs-guix is installed, otherwise
;; `guix-pulled-profile' will be undefined.
(when (locate-library "guix-profiles")
  (req-package guix-profiles
    :ensure (guix :pin :external)

    :config

    ;; https://guix.gnu.org/manual/en/html_node/The-Perfect-Setup.html
    (let ((guix-dir (expand-file-name "share/guile/site/3.0" guix-pulled-profile)))
      (with-eval-after-load 'geiser-guile
        (add-to-list 'geiser-guile-load-path guix-dir))
      (with-eval-after-load 'yasnippet
        (add-to-list 'yas-snippet-dirs (expand-file-name "etc/snippets" guix-dir)))
      (load-file (expand-file-name "etc/copyright.el" guix-dir)))
    ;; `scheme-mode-hook' -> `guix-devel-mode' -> `guix-misc' -> `guix-profiles'

    ))
