;; How to add support for a language?
;;
;; 1. Ensure the non-ts mode installed
;;
;; 2. Ensure the ts mode installed
;;
;; 3. Ensure the grammar can be downloaded
;;
;; 4. Add the grammar recipe into `treesit-auto-recipe-list' or download it
;;    manually.

(when (>= emacs-major-version 29)
  ;; https://github.com/renzmann/treesit-auto/blob/569cece0c014cd828ff532b5912769dcd4b48e0b/README.md

  (setup (:package treesit-auto)

    (autoload 'global-treesit-auto-mode "treesit-auto")
    (add-hook 'emacs-startup-hook 'global-treesit-auto-mode)

    (:when-loaded

      (:option treesit-auto-install 'prompt)

      )

    )

  )
