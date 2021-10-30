(req-package dired+
  :after (dired dired-x git-annex dired-filetype-face)
  ;; Load it after any other dired extensions, so that its key binding
  ;; overrides others.
  :ensure (dired+ :pin :quelpa)
  :quelpa (dired+
           :fetcher url
           :url "https://raw.github.com/emacsmirror/emacswiki.org/master/dired+.el")
  ;; There are 2 git repos contains dired+, [emacswiki.org][1] &
  ;; [dired-plus][2].
  ;;
  ;; dired+ in emacswiki.org is newer.  But emacswiki.org is too big to clone.
  ;; We have to download the file.
  ;;
  ;; [1]: https://github.com/emacsmirror/emacswiki.org/blob/master/dired%2b.el
  ;; [2]: https://github.com/emacsmirror/dired-plus
  :init
  (require 'dired+))
