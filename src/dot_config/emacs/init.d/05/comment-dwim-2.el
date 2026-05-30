(setup (:package comment-dwim-2)

  (keymap-global-set "<remap> <comment-dwim>" #'comment-dwim-2)
  ;; - Putting a remapping in a prefix keymap like `esc-map' typically has no
  ;;   effect, as such keymaps are not themselves active [1].
  ;;
  ;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Remapping-Commands.html
  ;;
  ;; - `:with-feature', `:with-map', `:bind-into' all imply `eval-after-load'.
  ;;   Do not use them.

  )
