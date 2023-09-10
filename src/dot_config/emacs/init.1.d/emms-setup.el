(setup emms-setup
  (:package emms)

  (:with-feature emms
    (:when-loaded
      (:require emms-setup)
      (emms-minimalistic)))

  )
