{% include "functions.sh" %}

: ${ENV:=~/.shrc}
export ENV

### Locale Settings
: ${LANG:=en_US.UTF-8}
export LANG

: ${EMACS_SERVER_FILE:=default}
export EMACS_SERVER_FILE
: ${EDITOR:={{ dotfiles_editor }}}
export EDITOR
: ${ALTERNATE_EDITOR:="emacs --daemon=default"}
export ALTERNATE_EDITOR
## [[info:emacs#emacsclient%20Options]]
##
## If the Emacs server is not running, an empty string makes
## emacsclient run `emacs --daemon' and try to connect to it.

: ${SSH_ASKPASS:=ssh-askpass}
export SSH_ASKPASS
: ${SUDO_ASKPASS:=/usr/bin/ssh-askpass}
export SUDO_ASKPASS
## sudo requires an absolute path to program for SUDO_ASKPASS.
: ${GIT_ASKPASS:=pass-askpass}
export GIT_ASKPASS
GPG_TTY=$(tty)
export GPG_TTY

unset MAILCHECK
# https://manpages.debian.org/stable/zsh-common/zshparam.1.en.html#PARAMETERS_USED_BY_THE_SHELL
# https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html
# https://man7.org/linux/man-pages/man1/dash.1.html#ENVIRONMENT

: ${EMAIL:={{ dotfiles_email }}}
export EMAIL
# For nullmailer
: ${MAILUSER:={{ dotfiles_mailuser }}}
export MAILUSER
: ${MAILHOST:={{ dotfiles_mailhost }}}
export MAILHOST

: ${MAILDIR:={{ dotfiles_maildir }}}
export MAILDIR

: ${LESS:=-Ri}
export LESS
## Without `-R`, less cannot handle some control characters correctly,
## such as colors.
: ${LESSCHARSET:=utf-8}
export LESSCHARSET

[ -z "$LESSOPEN" ] && [ -z "$LESSCLOSE" ] &&
	is_command lesspipe &&
	eval "$(lesspipe)"
## For less input pre-processor

# Do not verify the insecure directories for completions.
#
# https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/compfix.zsh
#
# `ZSH_DISABLE_COMPFIX` must be assigned in `.profile`.  When we launch zsh in
# an X terminal emulator, zsh may not be invoked with `-l`.  Since we rarely
# use zsh to launch X, we must ensure `ZSH_DISABLE_COMPFIX` defined in any
# shell besides zsh.
: ${ZSH_DISABLE_COMPFIX:=true}
export ZSH_DISABLE_COMPFIX

# For bash completions
#
# https://github.com/scop/bash-completion/blob/b12639a6becec13a0a2c06173ba40fb3bbe972e1/CHANGES#L1686
# https://github.com/scop/bash-completion/commit/c89dcbbd5510876f6304ef10806b00cc9fda19dc
# https://bugzilla.redhat.com/show_bug.cgi?id=1264094
: ${XDG_DATA_DIRS:=/usr/local/share:/usr/share}
export XDG_DATA_DIRS

# For ncurses terminal definitions
: ${TERMINFO_DIRS:=/usr/local/share/terminfo:/usr/share/terminfo}
export TERMINFO_DIRS

is_in_env() {
	eval "expr \":\$${2}:\" : \".*:${1}:\" > /dev/null"
}

prepend_to_env() {
	eval "$2=\"${1}\${${2}:+:\$${2}}\""
}

prepend_to_env_if_not_exist() {
	if ! is_in_env $1 $2; then
		eval "$2=\"${1}\${${2}:+:\$${2}}\""
	fi
}

append_to_env() {
	eval "$2=\"\${${2}:+\$${2}:}${1}\""
}

append_to_env_if_not_exist() {
	if ! is_in_env $1 $2; then
		eval "$2=\"\${${2}:+\$${2}:}${1}\""
	fi
}

export PATH
prepend_to_env ${HOME}/.local/bin PATH
prepend_to_env ${HOME}/local/bin PATH
append_to_env ${HOME}/bin PATH

: ${BROWSER:=browser.sh}
export BROWSER
: ${LEDGER_FILE:=${HOME}/org/ledger.dat}
export LEDGER_FILE

: ${no_proxy:="$(___no_proxy)"}
export no_proxy
# Do not use proxy for these hosts.

## For Emacs & IBus
# http://code.google.com/p/ibus/issues/detail?id=458
# https://bugs.launchpad.net/ubuntu/+source/emacs-snapshot/+bug/434730
# http://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=1646
# http://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=10867
# http://stackoverflow.com/questions/7513231/ibus-couldnt-receive-data-from-agent
: ${LC_CTYPE:=zh_CN.UTF-8}
export LC_CTYPE

: ${REPORTTIME:=1}
export REPORTTIME
## Print timing statistics for commands whose combined user and system
## execution times (in seconds) > 1.

## For Emacs
: ${EMACS_DIR:=$HOME/.emacs.d}
export EMACS_DIR
EMACSLOADPATH="$HOME/local/share/emacs/site-lisp:${EMACSLOADPATH}:/usr/share/org-mode/lisp:${EMACS_DIR}/site-lisp"
# Enable contributed extensions to org-mode on Debian
export EMACSLOADPATH
# Do not use `prepend_to_env` because `EMACSLOADPATH` may be empty.

## For fresh
# https://github.com/freshshell/fresh
case "$(get_shell)" in
bash | zsh | ksh)
	[ -r ~/.fresh/build/shell.sh ] && . ~/.fresh/build/shell.sh
	;;
*)
	# A temp fix for sh, dash, etc.
	__FRESH_BIN_PATH__=$HOME/bin
	expr ":$PATH:" : ".*:$__FRESH_BIN_PATH__:" >/dev/null || export PATH="$__FRESH_BIN_PATH__:$PATH"
	unset __FRESH_BIN_PATH__
	export FRESH_PATH="$HOME/.fresh"
	;;
esac

## For breach
# http://breach.cc/
: ${CHROME_DEVEL_SANDBOX:=/usr/lib/chromium/chrome-sandbox}
export CHROME_DEVEL_SANDBOX

## For Android SDK
: ${ANDROID_HOME:="${HOME}/opt/android-sdk"}
export ANDROID_HOME
prepend_to_env ${ANDROID_HOME}/tools PATH
prepend_to_env ${ANDROID_HOME}/tools/bin PATH
prepend_to_env ${ANDROID_HOME}/platform-tools PATH

## nix
# Expand HOME for Nix installer check.
if [ -e "{{ ansible_env.HOME }}/.nix-defexpr/channels_root" ]; then
	if [ -e "{{ ansible_env.HOME }}/.nix-profile/etc/profile.d/nix-daemon.sh" ]; then
		. "{{ ansible_env.HOME }}/.nix-profile/etc/profile.d/nix-daemon.sh"
	fi
fi
if [ -e "{{ ansible_env.HOME }}/.nix-defexpr/channels" ]; then
	if [ -e "{{ ansible_env.HOME }}/.nix-profile/etc/profile.d/nix.sh" ]; then
		. "{{ ansible_env.HOME }}/.nix-profile/etc/profile.d/nix.sh"
	fi
fi
# https://nixos.org/nix/manual/#idm140737322470304
: ${NIX_REMOTE:=daemon}
export NIX_REMOTE
is_command nix-env && {
	# : ${LOCALE_ARCHIVE:="$(nix-env --installed --no-name --out-path --query glibc-locales)/lib/locale/locale-archive"}
	: ${LOCALE_ARCHIVE:="$HOME/.nix-profile/lib/locale/locale-archive"}
	export LOCALE_ARCHIVE
	# https://unix.stackexchange.com/a/243189

	# For bash completions
	prepend_to_env ${HOME}/.nix-profile/share XDG_DATA_DIRS

	# For ncurses terminal definitions
	prepend_to_env ${HOME}/.nix-profile/share/terminfo TERMINFO_DIRS
	# https://github.com/NixOS/nixpkgs/issues/23402#issuecomment-284980534

	EMACSLOADPATH="$HOME/.nix-profile/share/emacs/site-lisp:$EMACSLOADPATH"
	# Do not use `prepend_to_env` because `EMACSLOADPATH` may be empty.
}

## whalebrew
: ${WHALEBREW_INSTALL_PATH:="${HOME}/bin"}
export WHALEBREW_INSTALL_PATH
prepend_to_env ${WHALEBREW_INSTALL_PATH} PATH

## For pass
: ${PASSWORD_STORE_DIR:={{ dotfiles_password_store_dir | default("${HOME}/.password-store") }}}
export PASSWORD_STORE_DIR
: ${PASSWORD_STORE_ENABLE_EXTENSIONS:=true}
export PASSWORD_STORE_ENABLE_EXTENSIONS

## For node-gyp
# https://github.com/nodejs/node-gyp/blob/v7.0.0/lib/process-release.js#L65-L67
# https://github.com/nodejs/node-gyp/blob/v7.0.0/lib/process-release.js#L76
: ${NODEJS_ORG_MIRROR:={{ dotfiles_nodejs_org_mirror | default("https://nodejs.org/dist") }}}
export NODEJS_ORG_MIRROR

## For node-build
: ${NODE_BUILD_MIRROR_URL:={{ dotfiles_node_build_mirror_url | default("https://nodejs.org/dist") }}}
export NODE_BUILD_MIRROR_URL

## For node-sass
# https://github.com/sass/node-sass/blob/v4.14.1/lib/extensions.js#L247
: ${SASS_BINARY_SITE:={{ dotfiles_sass_binary_site | default("https://github.com/sass/node-sass/releases/download") }}}
export SASS_BINARY_SITE

## Linuxbrew
# Install Linuxbrew into `/home/linuxbrew/`.
#
# https://github.com/Linuxbrew/brew/issues/762
: ${LINUXBREW_ROOT:="/home/linuxbrew/.linuxbrew"}
export LINUXBREW_ROOT
: ${HOMEBREW_BOTTLE_DOMAIN:={{ dotfiles_homebrew_bottle_domain | default("https://linuxbrew.bintray.com") }}}
export HOMEBREW_BOTTLE_DOMAIN
: ${HOMEBREW_BREW_GIT_REMOTE:={{ dotfiles_homebrew_brew_git_remote | default("https://github.com/Homebrew/brew") }}}
export HOMEBREW_BREW_GIT_REMOTE
: ${HOMEBREW_CORE_GIT_REMOTE:={{ dotfiles_homebrew_core_git_remote | default("https://github.com/Homebrew/linuxbrew-core") }}}
export HOMEBREW_CORE_GIT_REMOTE
# Enable `HOMEBREW_DEVELOPER`
#
# I have to enable this variable because I don't want to clone the taps as
# shallow.  I can pass `--full` but it's boring.
#
# https://github.com/Homebrew/brew/blob/1.7.6/Library/Homebrew/cmd/tap.rb#L62
export HOMEBREW_DEVELOPER=1
# Sometimes it is too slow to update Homebrew taps.  Only update taps manually.
export HOMEBREW_NO_AUTO_UPDATE=1
[ -x "${LINUXBREW_ROOT}/bin/brew" ] && eval "$("${LINUXBREW_ROOT}/bin/brew" shellenv)"
EMACSLOADPATH="${LINUXBREW_ROOT}/share/emacs/site-lisp:${EMACSLOADPATH}"
# Do not use `prepend_to_env` because `EMACSLOADPATH` may be empty.

# Setting `MANPATH` & `INFOPATH` does not work correctly if they are unset
# originally.  Unset them.
unset MANPATH
unset INFOPATH

## For Guix
: ${_GUIX_PROFILE:=${XDG_CONFIG_HOME:-$HOME/.config}/guix/current}
export _GUIX_PROFILE
: ${GUIX_PROFILE:=$HOME/.guix-profile}
export GUIX_PROFILE
: ${GUIX_LOCPATH:=${GUIX_PROFILE}/lib/locale}
export GUIX_LOCPATH
GUIX_PROFILE_BAK="${GUIX_PROFILE}"
GUIX_PROFILE=${_GUIX_PROFILE}
# Bind `GUIX_PROFILE` for sourcing `${_GUIX_PROFILE}/etc/profile`
if [ -f "${_GUIX_PROFILE}/etc/profile" ]; then
	. "${_GUIX_PROFILE}/etc/profile"
fi
GUIX_PROFILE="${GUIX_PROFILE_BAK}"
unset GUIX_PROFILE_BAK
# DO NOT source `/etc/profile.d/guix.sh` to push `${_GUIX_PROFILE}/bin` into
# `$PATH`.  That script reset `_GUIX_PROFILE` and `GUIX_PROFILE`.
INFOPATH="${_GUIX_PROFILE}/share/info:$INFOPATH"
# Do not use `prepend_to_env` because `INFOPATH` may be empty.
prepend_to_env ${GUIX_PROFILE}/share XDG_DATA_DIRS
# Since `/etc/profile.d/guix.sh` is not sourced and
# `${GUIX_PROFILE}/etc/profile` does not set `INFOPATH` and `XDG_DATA_DIRS`,
# set it here manually.
if [ -f "${GUIX_PROFILE}/etc/profile" ]; then
	. "${GUIX_PROFILE}/etc/profile"
fi
EMACSLOADPATH="${GUIX_PROFILE}/share/emacs/site-lisp:${EMACSLOADPATH}"
# Guix only set `EMACSLOADPATH` if Emacs is installed by Guix.  In case of
# using external Emacs, set `EMACSLOADPATH` manually.
#
# Also, do not use `prepend_to_env` because `EMACSLOADPATH` may be empty.

## For anyenv
: ${ANYENV_ROOT:="${HOME}/.anyenv"}
export ANYENV_ROOT
prepend_to_env ${ANYENV_ROOT}/bin PATH
is_command anyenv && eval "$(anyenv init - $(get_shell))"

## For python-build
: ${PYTHON_BUILD_MIRROR_URL:={{ dotfiles_python_build_mirror_url | default("https://pyenv.github.io/pythons") }}}
export PYTHON_BUILD_MIRROR_URL
: ${PYTHON_BUILD_MIRROR_URL_SKIP_CHECKSUM:={{ dotfiles_python_build_mirror_url_skip_checksum | default("") }}}
export PYTHON_BUILD_MIRROR_URL_SKIP_CHECKSUM

## For pyenv
is_command pyenv && {
	{ pyenv commands | grep -q virtualenv-init; } && {
		eval "$(pyenv virtualenv-init - $(get_shell))"
	}
	case "$(get_shell)" in
	bash | zsh | ksh)
		# virtualenvwrapper depends on pbr.
		! { pip list | grep '^pbr '; } >/dev/null 2>&1 || pyenv virtualenvwrapper 2>/dev/null
		;;
	esac
}

## For pipenv
# https://pipenv.pypa.io/en/latest/install/#virtualenv-mapping-caveat
export PIPENV_VENV_IN_PROJECT=1

## For rbenv-usergems
is_command rbenv && {
	{ rbenv commands | grep -q usergems-init; } && {
		eval "$(rbenv usergems-init - $(get_shell))"
	}
}

## For goenv
#
# goenv overrides `GOPATH` to avoid mixing multiple versions of golang packages
# at `GOPATH` when using different versions of golang.  To change `GOPATH`, do
# not set `GOPATH`, set `GOENV_GOPATH_PREFIX` instead.
#
# https://github.com/syndbg/goenv/blob/master/ENVIRONMENT_VARIABLES.md
# https://github.com/syndbg/goenv/issues/72#issuecomment-478011438

# goenv prepend `PATH` only if the shims is not in `PATH`.  But others prepend
# their paths unconditionally.  So `.profile` is invoked many times, goenv
# shims will be overridden by others.  Force to prepend it here.
PATH="$GOENV_ROOT/shims:$PATH"

## For Cask
prepend_to_env ${HOME}/.cask/bin PATH

## For cabal
prepend_to_env ${HOME}/.cabal/bin PATH

## For Go
: ${GOBIN:="${HOME}/bin"}
export GOBIN
prepend_to_env ${GOBIN} PATH
# Set global GOROOT
: ${GOROOT:="${GUIX_PROFILE}"}
export GOROOT
# Disable CGO by default.
#
# https://golang.org/cmd/cgo/
export CGO_ENABLED=0
# https://golang.org/cmd/go/#hdr-Vendor_Directories
export GO15VENDOREXPERIMENT=1
# Since Go 1.12 [1], we can always set `GO111MODULE=on`.
#
# [1]: https://golang.org/doc/go1.12#modules
export GO111MODULE=on
: ${GOPROXY:={{ dotfiles_goproxy | default("https://proxy.golang.org,direct") }}}
export GOPROXY
: ${GOSUMDB:={{ dotfiles_gosumdb | default("sum.golang.org") }}}
export GOSUMDB

## For Rust
# https://github.com/rust-lang-nursery/rustup.rs#environment-variables
: ${RUSTUP_HOME:="${HOME}/.rustup"}
export RUSTUP_HOME
: ${RUSTUP_DIST_SERVER:={{ dotfiles_rustup_dist_server | default("https://static.rust-lang.org") }}}
export RUSTUP_DIST_SERVER
: ${RUSTUP_UPDATE_ROOT:={{ dotfiles_rustup_update_root | default("https://static.rust-lang.org/rustup") }}}
export RUSTUP_UPDATE_ROOT
[ -r "${HOME}/.cargo/env" ] && . "${HOME}/.cargo/env"
: ${CARGO_HOME:="${HOME}/.cargo"}
export CARGO_HOME
if [ -n "${CARGO_INSTALL_ROOT}" ]; then
	prepend_to_env ${CARGO_INSTALL_ROOT}/bin PATH
elif [ -n "${CARGO_HOME}" ]; then
	prepend_to_env ${CARGO_HOME}/bin PATH
fi

## For scala
: ${SCALA_HOME:="${HOME}/opt/scala"}
export SCALA_HOME
prepend_to_env ${SCALA_HOME}/bin PATH

## For Groovy
: ${GROOVY_HOME:="${HOME}/opt/groovy"}
export GROOVY_HOME
prepend_to_env ${GROOVY_HOME}/bin PATH

## For Gradle
: ${GRADLE_HOME:="/opt/gradle"}
export GRADLE_HOME
prepend_to_env ${GRADLE_HOME}/bin PATH

## For Composer
: ${COMPOSER_HOME:="${HOME}/.composer"}
export COMPOSER_HOME
prepend_to_env ${COMPOSER_HOME}/vendor/bin PATH

## For Google Cloud SDK
# Google Cloud SDK must be installed into a user-writable location, such as
# `$HOME`.  See the comment of `googlecloudsdk_install_dir` in Google Cloud SDK
# install playbook for reason.
: ${CLOUDSDK_HOME:="${HOME}/opt/google-cloud-sdk"}
export CLOUDSDK_HOME
[ -r "${CLOUDSDK_HOME}/path.$(get_shell).inc" ] && . "${CLOUDSDK_HOME}/path.$(get_shell).inc"

## For yarn
if is_command yarn; then
	: ${YARN_GLOBAL_BIN:="$(yarn global bin)"}
else
	: ${YARN_GLOBAL_BIN:="${HOME}/.yarn/bin"}
fi
export YARN_GLOBAL_BIN
prepend_to_env ${YARN_GLOBAL_BIN} PATH

## For NPM
if is_command npm; then
	: ${NPM_PREFIX:="$(npm prefix -g)"}
else
	: ${NPM_PREFIX:="${HOME}/.npm-packages"}
fi
export NPM_PREFIX
prepend_to_env ${NPM_PREFIX}/bin PATH

## For [arduino-mk][1]
##
## [1]: https://github.com/sudar/Arduino-Makefile
: ${ARDUINO_DIR:="${LINUXBREW_ROOT}/opt/arduino"}
export ARDUINO_DIR
prepend_to_env ${ARDUINO_DIR} PATH

## For git-get
##
## https://github.com/homburg/git-get
## https://github.com/homburg/git-get#options
: ${GIT_GET_PATH:="${HOME}/src"}
export GIT_GET_PATH
: ${GIT_GET_HOST:="github.com"}
export GIT_GET_HOST
## https://github.com/pietvanzoen/git-get
## https://github.com/pietvanzoen/git-get#configuration
: ${GIT_PATH:="$HOME/src"}
export GIT_PATH
: ${GIT_GET_DEFAULT_PREFIX:="https://github.com/"}
export GIT_GET_DEFAULT_PREFIX
## https://github.com/grdl/git-get
## https://github.com/grdl/git-get#env-variables
: ${GITGET_ROOT:="$HOME/src"}
export GITGET_ROOT
: ${GITGET_HOST:=github.com}
export GITGET_HOST
: ${GITGET_SCHEME:=https}
export GITGET_SCHEME

## For [SDKMAN!][1]
##
## [1]: https://sdkman.io/
: ${SDKMAN_DIR:="${HOME}/.sdkman"}
export SDKMAN_DIR
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
case "$(get_shell)" in
bash | zsh)
	[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ] && . "$SDKMAN_DIR/bin/sdkman-init.sh"
	;;
esac

case "$(get_shell)" in
bash | zsh | ksh)

	## direnv
	is_command direnv && {
		eval "$(direnv hook $(get_shell))"
	}

	;;
esac

## For Xapian with mu
# https://github.com/djcb/mu/issues/544
export XAPIAN_CJK_NGRAM=1

## For wine
# Use win32 by default.  I use wine to run some old windows applications.  Some
# of them run on Windows 98.  Set `WINEARCH` to `win32` because wine64 does not
# support Windows 98.
#
# https://bbs.archlinux.org/viewtopic.php?pid=1511536#p1511536
# https://wiki.archlinux.org/index.php/Wine#WINEARCH
export WINEARCH=win32

# Suppress the warning "Couldn't register with accessibility bus"
# https://wiki.gnome.org/Accessibility/Documentation/GNOME2/Mechanics#GTK.2B-_and_Accessibility
# https://bugzilla.gnome.org/show_bug.cgi?id=563943#c0
# https://bugzilla.redhat.com/show_bug.cgi?id=1056820#c1
export NO_AT_BRIDGE=1

# WSL does not run systemd and dbus, do this check before importing env vars.
is_command systemctl && [ x"$(systemctl --user is-system-running)" != xoffline ] && {
	systemctl --user import-environment HOME PATH http_proxy https_proxy no_proxy
}

if [ x"$(get_shell)" = xsh ]; then
	if expr "$-" : '.*i' >/dev/null; then
		if [ -n "${ZSH_VERSION}" ]; then
			[ -z "${ENV}" ] || [ ! -r "${ENV}" ] || . "${ENV}"
		fi
	fi
fi
## If `sh` links to `zsh`, and is invoked as a "login, interactive"
## shell, it should `. $ENV`.

[ -r $HOME/.profile_local.sh ] && . $HOME/.profile_local.sh
