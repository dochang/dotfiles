(setup emacs

  (setopt user-full-name (or (getenv "NAME") (alist-get 'user-full-name **globals** "")))
  (setopt user-mail-address (or (getenv "EMAIL") (alist-get 'user-mail-address **globals** "")))

  ;; Disable auto save recovery record.
  ;; [[info:emacs#Recover]]
  (setopt auto-save-list-file-prefix nil)
  (setopt kill-buffer-delete-auto-save-files t)
  (setopt delete-auto-save-files t)

  ;; Set fill column to 78
  ;;
  ;; A line begins at column 0 in Emacs.  79 is the last column on an 80-width
  ;; screen.  Do not occupy column 79, leave it for filling.
  ;;
  ;; This setting will make Emacs fill the paragraphs at the bottom as is.
  ;;
  ;; Why 78?  Because I found that it will not break the text of MIT license
  ;; at [1].
  ;;
  ;; [1]: https://choosealicense.com/licenses/mit/
  (setopt fill-column 78)

  ;; Keep point at the same screen position after scrolling.
  ;; [[info:emacs#Scrolling]]
  (setopt scroll-preserve-screen-position t)

  ;; [[info:emacs#Auto%20Scrolling][info:emacs#Auto Scrolling]]
  ;; [[https://www.emacswiki.org/emacs/SmoothScrolling]]
  ;; [[https://stackoverflow.com/q/3631220]]
  ;; [[https://www.emacswiki.org/emacs/Scrolling]]
  (setopt scroll-margin 4)
  (setopt scroll-conservatively scroll-margin)
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
  (setopt cursor-type 'bar)
  (setopt cursor-in-non-selected-windows nil)
  (setopt indicate-buffer-boundaries 'left)
  (setopt indicate-empty-lines t)
  (setopt show-trailing-whitespace nil)
  ;; Don't highlight trailing whitespace for modes other than `prog-mode'.
  (setopt vertical-scroll-bar 'left)
  (setopt horizontal-scroll-bar 't)

  (setopt visible-bell t)
  (setopt inhibit-startup-screen nil)

  (setopt use-default-font-for-symbols nil)
  ;; Honor the fontsets.

  (setopt create-lockfiles t)
  ;; Explicitly enable lockfiles.

  (setopt completion-ignore-case t)
  (setopt read-buffer-completion-ignore-case t)
  ;; For `completion-ignore-case' and `read-buffer-completion-ignore-case',
  ;; ignore case.
  ;;
  ;; Because there are a lot of names of objects and internal buffers that I'm
  ;; not sure the cases in their names.

  )

;; Below is a sample paragraph to test filling.

;; ----------------------------------------------------------------------------

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ----------------------------------------------------------------------------
