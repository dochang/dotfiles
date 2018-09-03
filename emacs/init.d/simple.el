(req-package simple
  :ensure nil

  :hook ((emacs-startup . column-number-mode)
         (emacs-startup . global-visual-line-mode)
         ;; Why do I need `global-visual-line-mode'?
         ;;
         ;; With `org-indent-mode' enabled, `truncate-lines' is nil and
         ;; `word-wrap' is nil, if you execute `previous-line' on a very long
         ;; line that is wrapped by Emacs in org-mode, `previous-line' will
         ;; signal a `beginning-of-buffer' error.
         ;;
         ;; The reason is during `previous-line' running, org-mode puts the
         ;; whole line in a temporary buffer.  The line is the only 1 line in
         ;; the buffer.  When you execute `previous-line' in the middle of the
         ;; 1st line of the buffer, `previous-line' will signal
         ;; `beginning-of-buffer'.
         ;;
         ;; The error is signaled from `line-move-visual' to `line-move', and
         ;; finally, to `previous-line'.
         ;;
         ;; In the body of `line-move-visual', see this part:
         ;;
         ;; #### 1 ####
         ;; (or (and (or (and (>= arg 0)
         ;;                   (>= (vertical-motion
         ;;                        (cons (or goal-column
         ;;                                  (if (consp temporary-goal-column)
         ;;                                      (car temporary-goal-column)
         ;;                                    temporary-goal-column))
         ;;                              arg))
         ;;                       arg))
         ;; #### 2 ####
         ;;              (and (< arg 0)
         ;;                   (<= (vertical-motion
         ;;                        (cons (or goal-column
         ;;                                  (if (consp temporary-goal-column)
         ;;                                      (car temporary-goal-column)
         ;;                                    temporary-goal-column))
         ;;                              arg))
         ;;                       arg)))
         ;; #### 3 ####
         ;;          (or (>= arg 0)
         ;;              (/= (point) opoint)
         ;;              ;; If the goal column lies on a display string,
         ;;              ;; `vertical-motion' advances the cursor to the end
         ;;              ;; of the string.  For arg < 0, this can cause the
         ;;              ;; cursor to get stuck.  (Bug#3020).
         ;;              (= (vertical-motion arg) arg)))
         ;; #### 4 ####
         ;;     (unless noerror
         ;;       (signal (if (< arg 0) 'beginning-of-buffer 'end-of-buffer)
         ;;               nil)))
         ;;
         ;; In #2, `arg' should be -1.  `vertical-motion' receives a cons like
         ;; `(n . -1)' where N is a positive integer, and should return -1.  So
         ;; the `and' form returns true.  The control flow should go to #3.
         ;;
         ;; But in org-mode, if `org-indent-mode' is enabled, `vertical-motion'
         ;; returns 0, this causes the `and' form returns false.  The control
         ;; flow will skip #3. Because `noerror' is nil, `line-move-visual'
         ;; signals `beginning-of-buffer' (since `arg' is -1).
         ;;
         ;; I don't know how to fix `vertical-motion'.  To solve this problem,
         ;; I have to set `word-wrap' to t.
         ;;
         ;; Since I have set `truncate-lines' to nil, if I also set `word-wrap'
         ;; to t, why not enable `visual-line-mode'?
         ;;
         ;; Note, enabling `visual-line-mode' will execute the following code:
         ;;
         ;; (set (make-local-variable 'line-move-visual) t)
         ;; (set (make-local-variable 'truncate-partial-width-windows) nil)
         ;; (setq truncate-lines nil)
         ;; (setq word-wrap t)
         ;;
         ;; See the definition of `visual-line-mode' for details.
         )

  :init

  ;; `compose-mail' will warn if `mail-self-blind' &
  ;; `compose-mail-user-agent-warnings' are both set to `t'.
  ;;
  ;; Suppress this warning.
  (setq compose-mail-user-agent-warnings nil)

  ;; Use `message-user-agent' for mail composition.
  ;; [[info:emacs#Mail%20Methods]]
  (setq mail-user-agent 'message-user-agent)

  ;; Don't delete trailing lines when calling `delete-trailing-whitespace' on
  ;; the entire buffer.
  (setq delete-trailing-lines nil)

  :config

  ;; Special Mode

  (unless (lookup-key special-mode-map "z")
    (define-key special-mode-map "z" 'kill-this-buffer))
  ;; Emacs has deleted `z' binding in GIT#0d4505d & GIT#82dffff .  We
  ;; restore it here.

  )
