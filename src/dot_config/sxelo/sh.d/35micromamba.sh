MAMBA_ROOT_PREFIX="${HOME}/micromamba"
! command -v micromamba >/dev/null || {
	eval "$(micromamba shell hook --shell "$SXELO" --root-prefix "$MAMBA_ROOT_PREFIX")"
}
