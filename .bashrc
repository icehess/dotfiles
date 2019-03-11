[ -f ~/.profile ] && . ~/.profile

if [ -n "$BASH_VERSION" -a -n "$PS1" -a -z "$BASH_COMPLETION_COMPAT_DIR" ]; then
    # setting bash_completion
    [ -r /etc/bash_completion   ] && . /etc/bash_completion
    [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

    [ -r /usr/share/bash_completion/bash_completion ] && . /usr/share/bash_completion/bash_completion
fi

# Setup fzf
# ---------
if [ -d ~/.fzf/ ]; then
    export PATH="$PATH:~/.fzf/bin"
    [ $- == *i* ] && source "~/.fzf/shell/completion.bash" 2> /dev/null
    [ -f ~/.fzf-key-bindings.bash ] && . ~/.fzf-key-bindings.bash
fi

_info () {
    msg="$1"
    printf "\e[1;36m::\e[1;37m $msg \e[00m \n"
}

_error () {
    msg="$1"
    printf "\e[1;37m::\e[1;31m $msg \e[00m \n"
}

_die () {
    _error "$1"
    exit 1
}
