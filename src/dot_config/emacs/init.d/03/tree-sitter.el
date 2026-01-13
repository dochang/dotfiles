(unless (and (fboundp #'treesit-available-p)
             (treesit-available-p))
  ;; Use the built-in integration instead of this package on Emacs 29+.
  ;;
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/commit/d3eab879e9b0ccc9582113d81a93ad06f3c6bfb1
  ;; https://emacs-tree-sitter.github.io/

  (setup (:package tree-sitter))

  )
