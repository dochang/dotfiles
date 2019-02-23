# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

[ -r /etc/bash_completion ] && . /etc/bash_completion




#### BASH IT !!!!

# Path to the bash it configuration
: ${BASH_IT:=$HOME/.bash_it}
export BASH_IT

# Lock and Load a custom theme file
# location /.bash_it/themes/
: ${BASH_IT_THEME:='bobby'}
export BASH_IT_THEME

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

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
