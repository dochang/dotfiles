(defun $emacs-startup-hook ()
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme))

(req-package emacs

  :hook ((emacs-startup . $emacs-startup-hook))

  :custom

  (user-full-name (or (getenv "NAME") "{{ dotfiles_emacs_name }}"))
  (user-mail-address (or (getenv "EMAIL") "{{ dotfiles_emacs_email }}"))

  ;; Disable auto save recovery record.
  ;; [[info:emacs#Recover]]
  (auto-save-list-file-prefix nil)

  ;; Set fill column to 79
  ;;
  ;; A line begins at column 0 in Emacs.  79 is the last column on an 80-width
  ;; screen.  Do not occupy column 79, leave it for filling.
  ;;
  ;; This setting will make Emacs fill the paragraphs at the bottom as is.
  (fill-column 79)

  ;; Disable `indent-tabs-mode'.
  ;;
  ;; Do not insert tabs in indentation by default.
  ;;
  ;; Also, `web-mode' changes indentation settings if `indent-tabs-mode' is
  ;; non-nil.  To prevent it, set the default value of `indent-tabs-mode' to
  ;; `nil'.
  ;;
  ;; NOTE: Setting `indent-tabs-mode' to `t' does NOT mean "pressing `TAB'
  ;; inserts a `\t'".
  (indent-tabs-mode nil)

  ;; Keep point at the same screen position after scrolling.
  ;; [[info:emacs#Scrolling]]
  (scroll-preserve-screen-position t)

  ;; [[info:emacs#Auto%20Scrolling][info:emacs#Auto Scrolling]]
  ;; [[https://www.emacswiki.org/emacs/SmoothScrolling]]
  ;; [[https://stackoverflow.com/q/3631220]]
  ;; [[https://www.emacswiki.org/emacs/Scrolling]]
  (scroll-margin 4)
  (scroll-conservatively scroll-margin)
  ;; Why not set it to a big number like 9999?  Because it will cause
  ;; `search-forward' doesn't recenter the point if it moves off screen.  I
  ;; found this problem when I tried to revert a dired buffer.
  ;;
  ;; The call stack:
  ;;
  ;; `revert-buffer'
  ;; -> `revert-buffer-function'
  ;; -> `dired-revert'
  ;; -> `dired-restore-positions'
  ;; -> `dired-goto-file'
  ;; -> `dired-goto-file-1'
  ;; -> `search-forward'

  ;; UI Configuration
  (cursor-type 'bar)
  (cursor-in-non-selected-windows nil)
  (indicate-buffer-boundaries 'left)
  (indicate-empty-lines t)

  (visible-bell t)
  (inhibit-startup-screen t)

  )

;; ----------------------------------------------------------------------------

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;; ----------------------------------------------------------------------------
