### Prompt
autoload -Uz promptinit
promptinit

prompt_debian_setup () {
	PS1='%n@%m:%0~%(#.#.%%) '
	PS2="> "
	prompt_opts=( cr percent )
}

prompt_debian_setup "$@"


### Completion
autoload -Uz compinit
compinit

## https://unix.stackexchange.com/a/188951
compdef gpg2=gpg


### For nenv completion
[ ! -r ${NENV_ROOT}/completions/zsh/_nenv ] || . ${NENV_ROOT}/completions/zsh/_nenv




#### OH MY ZSH !!!!

# Path to your oh-my-zsh configuration.
: ${ZSH:=$HOME/.oh-my-zsh}
export ZSH

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
: ${ZSH_THEME:="robbyrussell"}
export ZSH_THEME

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git docker bower vagrant)

[ ! -r $ZSH/oh-my-zsh.sh ] || source $ZSH/oh-my-zsh.sh

# Customize to your needs...

{% include "shrc.sh" %}

## The git plugin of oh-my-zsh defines `gvt` as an alias, which conflicts with
## the go vendoring tool [gvt][1].  Unalias it.
##
## [1]: https://github.com/FiloSottile/gvt
is_alias gvt && unalias gvt

setopt INTERACTIVE_COMMENTS
setopt NO_BEEP


### Key bindings

## Standard key bindings
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

## Select emacs-like key bindings
bindkey -e

## Bind `^U` to `backward-kill-line` rather than `kill-whole-line`.
# http://stackoverflow.com/a/3483679
bindkey "^U" backward-kill-line

bindkey "\el" down-case-word


### History
HISTFILE=~/.zhistory
