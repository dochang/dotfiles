;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
(advice-add 'org-capture :after #'$org-save-all-org-buffers)

(defun $org-capture-file ()
  (completing-read "capture to: " (org-files-list)))

(setup org-capture
  (:package org)

  (keymap-global-set "C-c c" #'org-capture)
  ;; For user convenience.

  (add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers)

  (:when-loaded

    ;; Templates for the creation of new entries.
    (setopt org-capture-templates
            `(("*" "NOTE" entry (file ,#'$org-capture-file)
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("n" "NEXT" entry (file ,#'$org-capture-file)
               "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("t" "TODO" entry (file ,#'$org-capture-file)
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("o" "CLOCK CAPTURE")
              ("o*" "CLOCK NOTE" entry (clock)
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("on" "CLOCK NEXT" entry (clock)
               "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("ot" "CLOCK TODO" entry (clock)
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%a"
               :empty-lines 1)
              ("oi" "CLOCK PLAIN LIST ITEM" item (clock)
               nil)
              ("oc" "CLOCK CHECKBOX ITEM" checkitem (clock)
               nil)
              ("oT" "CLOCK TABLE LINE" table-line (clock)
               nil
               :empty-lines 1)
              ("o " "CLOCK PLAIN TEXT" plain (clock)
               "%?"
               :empty-lines 1)
              ("b" "Brain" plain (function org-brain-goto-end)
               "* %i%?" :empty-lines 1)))

    ;; Use the date at point when capturing from agendas.
    (setopt org-capture-use-agenda-date t)

    )

  )
