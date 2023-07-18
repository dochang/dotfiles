local wezterm = require("wezterm")
local config = {}

config.use_ime = true
config.xim_im_name = "fcitx"
config.font = wezterm.font_with_fallback({
  {
    family = "Sarasa Fixed SC",
  },
  {
    family = "Noto Sans Mono CJK SC",
  },
  {
    family = "HanaMinB",
  },
  {
    family = "Noto Sans Mono",
  },
  {
    family = "DejaVu Sans Mono",
  },
})

return config
