(req-package lsp-ui-doc
  :ensure lsp-ui
  :init
  (setq lsp-ui-doc-use-childframe t)
  ;; Prefer to display documentation in a child-frame in graphical frames.
  ;;
  ;; lsp-ui uses overlay to display documentation at right side in current
  ;; frame.  If Emacs scales up, the line width will increase, that may cause
  ;; line wrapping.
  ;;
  ;; This problem does not occur in frames on text terminals because those
  ;; frames cannot scale and lsp-ui forces to use overlay in those frames.
  (setq lsp-ui-doc-position 'at-point)
  ;; (lsp-ui-doc-delay 2)
  ;; Default value (0.200 sec) is too fast.  Some major modes will call
  ;; formatter on `save-buffer'.  The formatter reformat my code entirely.
  ;; Since `lsp-on-change' saves the buffer, it will call `save-buffer' every
  ;; time I type in the buffer, even if the typing isn't completed.  This
  ;; breaks my code.  So, set `lsp-ui-doc-delay' to a bigger value.
  )
