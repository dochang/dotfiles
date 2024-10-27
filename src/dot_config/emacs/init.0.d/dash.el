(setup dash

  ;; The following packages depend on dash, but it seems that those ELPA
  ;; packages are unable to load "external" packages.  That means dash has to
  ;; be installed from any ELPA source.
  ;;
  ;; - smartparens
  ;; - prism
  ;; - magit
  ;; - magit-section
  ;;
  ;; Do not check whether dash is `package-installed-p'.  An external package
  ;; is considered as "installed" but not "built-in".
  (unless ($package-user-installed-p 'dash)
    ($package-install-from-elpa 'dash))

  )
