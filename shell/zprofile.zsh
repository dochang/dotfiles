{% include "profile.sh" %}

# Do not verify the insecure directories for completions.
#
# https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/compfix.zsh
: ${ZSH_DISABLE_COMPFIX:=true}
export ZSH_DISABLE_COMPFIX
