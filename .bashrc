[ -f ~/.profile ] && . ~/.profile

if [ -n "$BASH_VERSION" -a -n "$PS1" -a -z "$BASH_COMPLETION_COMPAT_DIR" ]; then
    # setting bash_completion
    [ -r /etc/bash_completion   ] && . /etc/bash_completion
    [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

    [ -r /usr/share/bash_completion/bash_completion ] && . /usr/share/bash_completion/bash_completion
fi

[ -f ~/.fzf-key-bindings.bash ] && . ~/.fzf-key-bindings.bash
