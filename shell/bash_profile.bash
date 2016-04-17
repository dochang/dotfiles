{% include "profile.sh" %}

if expr "$-" : '.*i' > /dev/null ; then
        [ -r ~/.bashrc ] && . ~/.bashrc
fi
## `. ~/.bashrc` only if `bash` is invoked as an "interactive" shell.
