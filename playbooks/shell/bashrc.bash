# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

# https://github.com/scop/bash-completion#installation
if [ -r /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
fi

# sdkman-init.sh
#
# This comment is used to prevent sdkman install script from inserting init
# snippet.

#### BASH IT !!!!

# Path to the bash it configuration
: ${BASH_IT:=$HOME/.bash_it}
export BASH_IT

# Lock and Load a custom theme file
# location /.bash_it/themes/
: ${BASH_IT_THEME:='bobby'}
export BASH_IT_THEME

# Set this to false to turn off version control status checking within the prompt for all themes
: ${SCM_CHECK:=true}
export SCM_CHECK

# If your theme use command duration, uncomment this to
# enable display of last command duration.
export BASH_IT_COMMAND_DURATION=true
# You can choose the minimum time in seconds before
# command duration is displayed.
export COMMAND_DURATION_MIN_SECONDS=1

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Uncomment this to make Bash-it create alias reload.
export BASH_IT_RELOAD_LEGACY=1

# Load Bash It
[ ! -r $BASH_IT/bash_it.sh ] || source $BASH_IT/bash_it.sh

# Customize to your needs...

{% include "shrc.sh" %}

# For brew bash completion
#
# https://github.com/Homebrew/brew/blob/master/docs/Shell-Completion.md#configuring-completions-in-bash
if type brew &>/dev/null; then
	for completion_file in $(brew --prefix)/etc/bash_completion.d/*; do
		source "$completion_file"
	done
fi

# For nix bash completion
if [ -r $HOME/.nix-profile/etc/profile.d/bash_completion.sh ]; then
	. $HOME/.nix-profile/etc/profile.d/bash_completion.sh
fi

# For Guix bash completion
if [ -r "${GUIX_PROFILE}/share/bash-completion/bash_completion" ]; then
	BASH_COMPLETION_COMPAT_DIR="${_GUIX_PROFILE}/etc/bash_completion.d"
	# Bind `BASH_COMPLETION_COMPAT_DIR` for loading Guix own completions.
	. "${GUIX_PROFILE}/share/bash-completion/bash_completion"
fi
