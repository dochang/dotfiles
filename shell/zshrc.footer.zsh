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
