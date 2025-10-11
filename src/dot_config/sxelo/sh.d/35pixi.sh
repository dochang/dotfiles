PIXI_HOME="${HOME}/.pixi"
PATH="${PIXI_HOME}/bin${PATH:+:}$PATH"

! command -v pixi >/dev/null || eval "$(pixi completion --shell "$SXELO")"
