: ${ENV:=~/.shrc}
export ENV


### Locale Settings
: ${LANG:=en_US.UTF-8}
export LANG


: ${EDITOR:=editor.py}
export EDITOR
: ${ALTERNATE_EDITOR:=""}
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
GPG_TTY=$(tty)
export GPG_TTY


: ${EMAIL:={{ EMAIL }}}
export EMAIL


: ${LESS:=-Ri}
export LESS
## Without `-R`, less cannot handle some control characters correctly,
## such as colors.


[ -z "$LESSOPEN" -a -z "$LESSCLOSE" ] \
	&& which lesspipe >/dev/null 2>&1 \
	&& eval "$(lesspipe)"
## For less input pre-processor


is_in_env () {
	eval "expr \":\$${2}:\" : \".*:${1}:\" > /dev/null"
}

prepend_to_env () {
	if ! is_in_env $1 $2 ; then
		eval "$2=\"${1}\${${2}:+:\$${2}}\""
	fi
}

append_to_env () {
	if ! is_in_env $1 $2 ; then
		eval "$2=\"\${${2}:+\$${2}:}${1}\""
	fi
}

PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/games:/usr/games"
export PATH
prepend_to_env ${HOME}/.local/bin PATH
prepend_to_env ${HOME}/local/bin PATH
append_to_env ${HOME}/bin PATH

: ${BROWSER:=browser.sh}
export BROWSER
: ${LEDGER_FILE:=${HOME}/org/ledger.dat}
export LEDGER_FILE
: ${http_proxy:=http://127.0.0.1:8118}
export http_proxy
# Dropbox doesn't check `all_proxy`, use `http_proxy` instead.
: ${https_proxy:=$http_proxy}
export https_proxy
# Dropbox doesn't check `all_proxy` & `http_proxy` when accessing https URLs in
# `dropbox.py start -i`, use `https_proxy` instead.
#
# http://askubuntu.com/questions/69434/cant-complete-dropbox-installation-from-behind-proxy#comment-553429

___no_proxy () {
	printf "localhost,127.*.*.*,10.*.*.*,192.168.*.*"
	printf ",$(seq -f '172.%g.*.*' -s , 16 31)"
	# https://github.com/docker/docker/pull/10133
	# https://github.com/docker/docker/pull/9951
	# https://github.com/docker/docker/issues/10192
	# https://github.com/docker/docker/issues/10224
	printf ",/var/run/docker.sock"
	printf ",lvh.me,vcap.me,localtest.me"
	printf ",sinaapp.com,sinaimg.cn"
	printf ",youku.com,tudou.com,tudouui.com,tdimg.com"
	printf ",baidupcs.com,baidu.com,bdstatic.com,bdimg.com,tiebaimg.com"
	printf ",360.cn,yunpan.cn,sohu.com,sina.com.cn,douban.com,163.com"
	printf ",taobao.com,tmall.com,etao.com,tbcdn.cn,mmstat.com"
	printf ",alipay.com,alimama.com,aliyun.com,yunos.com,alicdn.com"
	printf ",jd.com,360buyimg.com,3.cn"
	printf ",z.cn,amazon.cn"
	printf ",acfun.*,bilibili.*,tucao.*"
	case "$(hostname --domain)" in
	genee.cn|geneegroup.com)
		printf ",genee.cn,geneegroup.com,genee.in"
		;;
	esac
}

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

## For fresh
# https://github.com/freshshell/fresh
case "$(get_shell)" in
bash|zsh|ksh)
	[ -r ~/.fresh/build/shell.sh ] && . ~/.fresh/build/shell.sh
	;;
*)
	# A temp fix for sh, dash, etc.
	__FRESH_BIN_PATH__=$HOME/bin; expr ":$PATH:" : ".*:$__FRESH_BIN_PATH__:" >/dev/null || export PATH="$__FRESH_BIN_PATH__:$PATH"; unset __FRESH_BIN_PATH__
	export FRESH_PATH="$HOME/.fresh"
	;;
esac

## For breach
# http://breach.cc/
: ${CHROME_DEVEL_SANDBOX:=/usr/lib/chromium/chrome-sandbox}
export CHROME_DEVEL_SANDBOX

## For Android SDK
: ${ANDROID_HOME:=/opt/android-sdk}
export ANDROID_HOME
prepend_to_env ${ANDROID_HOME}/tools PATH
prepend_to_env ${ANDROID_HOME}/platform-tools PATH

## Linuxbrew
: ${LINUXBREW_ROOT:="${HOME}/.linuxbrew"}
export LINUXBREW_ROOT
prepend_to_env ${LINUXBREW_ROOT}/sbin PATH
prepend_to_env ${LINUXBREW_ROOT}/bin PATH
prepend_to_env ${LINUXBREW_ROOT}/lib LD_LIBRARY_PATH
prepend_to_env ${LINUXBREW_ROOT}/share/man MANPATH
prepend_to_env ${LINUXBREW_ROOT}/share/info INFOPATH

## nix
# Expand HOME for Nix installer check.
if [ -e "{{ HOME }}/.nix-profile/etc/profile.d/nix.sh" ]; then
	. "{{ HOME }}/.nix-profile/etc/profile.d/nix.sh"
fi

## For anyenv
: ${ANYENV_ROOT:="${HOME}/.anyenv"}
export ANYENV_ROOT
prepend_to_env ${ANYENV_ROOT}/bin PATH
which anyenv >/dev/null 2>&1 && eval "$(anyenv init - $(get_shell))"

## For pyenv
which pyenv >/dev/null 2>&1 && {
	{ pyenv commands | grep virtualenv-init ; } >/dev/null 2>&1 || eval "$(pyenv virtualenv-init - $(get_shell))"
	case "$(get_shell)" in
	bash|zsh|ksh)
		# virtualenvwrapper depends on pbr.
		! { pip list | grep '^pbr ' ; } >/dev/null 2>&1 || pyenv virtualenvwrapper 2>/dev/null
		;;
	esac
}

## For Cask
prepend_to_env ${HOME}/.cask/bin PATH

## For cabal
prepend_to_env ${HOME}/.cabal/bin PATH

## For Go
: ${GOPATH:="${HOME}/go"}
export GOPATH
# https://github.com/golang/go/wiki/GOPATH
prepend_to_env $(echo $GOPATH | sed -e 's|:|/bin:|g ; s|$|/bin|g') PATH
# https://golang.org/cmd/go/#hdr-Vendor_Directories
export GO15VENDOREXPERIMENT=1

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

case "$(get_shell)" in
bash|zsh|ksh)

	## direnv
	which direnv >/dev/null 2>&1 && {
		eval "$(direnv hook $(get_shell))"
	}

	;;
esac

systemctl --user import-environment HOME PATH http_proxy https_proxy no_proxy


if [ x"$(get_shell)" = xsh ] ; then
	if expr "$-" : '.*i' > /dev/null ; then
		if [ -n "${ZSH_VERSION}" ] ; then
			[ -z "${ENV}" ] || [ ! -r "${ENV}" ] || . "${ENV}"
		fi
	fi
fi
## If `sh` links to `zsh`, and is invoked as a "login, interactive"
## shell, it should `. $ENV`.
