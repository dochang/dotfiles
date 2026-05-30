(setup (:package sly)

  (setq sly-replace-slime t)
  ;; Put this assignment outside `eval-after-load' because sly queries
  ;; `sly-replace-slime' before loading sly (e.g. upgrade).

  )
