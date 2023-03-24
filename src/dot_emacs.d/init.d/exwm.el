(defun $wm-name ()
  (shell-command-to-string
   "wmctrl -m 2>/dev/null | sed -n 's/Name: *\\(.*\\)/\\1/p'"))

(defun $wm-running-p ()
  (not (string= "" ($wm-name))))

(defvar **exwm-enabled** nil)

(defun $exwm-config ()
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  ;; (unless (get 'exwm-input-global-keys 'saved-value)
  ;;   (setq exwm-input-global-keys
  ;;         `(
  ;;           ;; 's-r': Reset (to line-mode).
  ;;           ([?\s-r] . exwm-reset)
  ;;           ;; 's-w': Switch workspace.
  ;;           ([?\s-w] . exwm-workspace-switch)
  ;;           ;; 's-&': Launch application.
  ;;           ([?\s-&] . (lambda (command)
  ;;                        (interactive (list (read-shell-command "$ ")))
  ;;                        (start-process-shell-command command nil command)))
  ;;           ;; 's-N': Switch to certain workspace.
  ;;           ,@(mapcar (lambda (i)
  ;;                       `(,(kbd (format "s-%d" i)) .
  ;;                         (lambda ()
  ;;                           (interactive)
  ;;                           (exwm-workspace-switch-create ,i))))
  ;;                     (number-sequence 0 9)))))
  ;; Line-editing shortcuts
  ;; (unless (get 'exwm-input-simulation-keys 'saved-value)
  ;;   (setq exwm-input-simulation-keys
  ;;         '(([?\C-b] . [left])
  ;;           ([?\C-f] . [right])
  ;;           ([?\C-p] . [up])
  ;;           ([?\C-n] . [down])
  ;;           ([?\C-a] . [home])
  ;;           ([?\C-e] . [end])
  ;;           ([?\M-v] . [prior])
  ;;           ([?\C-v] . [next])
  ;;           ([?\C-d] . [delete])
  ;;           ([?\C-k] . [S-end delete]))))
  ;; Enable EXWM
  (exwm-enable)
  ;; Other configurations
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (exwm-systemtray-enable))

(defun $exwm-mode-hook ()
  (setq-local vertical-scroll-bar nil)
  (setq-local horizontal-scroll-bar nil))

(defun $exwm-enable ()
  (when (and (bound-and-true-p **exwm-enabled**)
             (not ($wm-running-p)))
    (require 'exwm)
    (require 'exwm-config)
    (require 'exwm-systemtray)
    ($exwm-config)))

(req-package exwm
  :hook ((emacs-startup . $exwm-enable)
         (exwm-mode . $exwm-mode-hook)))
