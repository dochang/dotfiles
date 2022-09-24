;;; Rec Mode -- Major mode for viewing/editing rec files
;; https://www.gnu.org/software/recutils/
;; https://elpa.gnu.org/packages/rec-mode.html

(defun $rec-summary-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(defun $rec-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(defun $rec-edit-field-mode-hook ()
  ;; View mode overrides some key bindings.  Do not enable it.
  (set (make-local-variable 'view-read-only) nil))

(req-package rec-mode
  :hook ((rec-mode . $rec-mode-hook)
         (rec-summary-mode . $rec-summary-mode-hook)
         (rec-edit-field-mode . $rec-edit-field-mode-hook)))
