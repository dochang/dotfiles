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

      (:option treesit-auto-install t)
      ;; Install tree-sitter grammars noninteractively.

      (setq treesit-auto-recipe-list
            (append
             (list
              (make-treesit-auto-recipe
               :lang 'elisp
               :ts-mode 'emacs-lisp-ts-mode
               :remap 'emacs-lisp-mode
               :url "https://github.com/Wilfred/tree-sitter-elisp"
               :ext "\\(?:\\.rcp\\|\\.el\\|Cask\\|Project\\.ede\\|[:/\\]\\..*\\(?:emacs\\|gnus\\|viper\\)\\|\\`\\..*emacs\\|[:/]_emacs\\)\\'")
              ;; https://github.com/Wilfred/tree-sitter-elisp
              )
             treesit-auto-recipe-list))

      (setq treesit-auto-langs
            (seq-map #'treesit-auto-recipe-lang treesit-auto-recipe-list))

      ;; Reset `global-treesit-auto-modes'.  Ensure that all modes in
      ;; `treesit-auto-recipe-list' will enable `global-treesit-auto-mode'.
      ;;
      ;; Why overwrite it?  Why?  When?  How?
      (setq global-treesit-auto-modes
            (let ((modes '()))
              (cl-loop for recipe in (treesit-auto--selected-recipes)
                       do (push (treesit-auto-recipe-ts-mode recipe) modes)
                       do (dolist (mode (ensure-list (treesit-auto-recipe-remap recipe)))
                            (push mode modes))
                       finally return modes)))

      )

    )

  )
