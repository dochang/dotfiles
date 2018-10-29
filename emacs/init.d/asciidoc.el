(defun $load-asciidoc (mode map)
  (eval-after-load mode
    `(progn
       (require 'asciidoc)
       (easy-menu-define
         asciidoc-global-menu ,map "" asciidoc-global-menuspec))))

(req-package asciidoc

  ;; :after (:any adoc-mode doc-mode)
  ;;
  ;; No need to specify `:after' since we call `eval-after-load' explicitly.

  :ensure (asciidoc :pin :quelpa)

  :quelpa (asciidoc :fetcher github :repo "metaperl/asciidoc-el")

  :init

  (mapc (lambda (args)
          (apply '$load-asciidoc args))
        '((doc-mode doc-mode-map)
          ;; Actually we don't use doc-mode.  But we configure it for backward
          ;; compatibility.
          (adoc-mode adoc-mode-map)))

  :config

  (define-key global-map
    (vector 'menu-bar (easy-menu-intern (car asciidoc-global-menuspec)))
    nil)

  )
