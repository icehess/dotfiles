[ -f ~/.profile ] && . ~/.profile

if [ -n "$BASH_VERSION" -a -n "$PS1" -a -z "$BASH_COMPLETION_COMPAT_DIR" ]; then
    # setting bash_completion
    [ -r /etc/bash_completion   ] && . /etc/bash_completion
    [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

    [ -r /usr/share/bash_completion/bash_completion ] && . /usr/share/bash_completion/bash_completion
    # gaywd damn it centos
    [ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

    [ -f ~/.fzf/shell/completion.bash ] && . "~/.fzf/shell/completion.bash" 2> /dev/null
fi

if [ -f /usr/share/git/git-prompt.sh ]; then
    source /usr/share/git/.git-prompt.sh
else
    __git_ps1() {
        return $?
    }
fi
# GIT_PS1_SHOWDIRTYSTATE=1
# GIT_PS1_SHOWSTASHSTATE=1
# GIT_PS1_SHOWUNTRACKEDFILES=1
# GIT_PS1_SHOWUPSTREAM="auto"
# GIT_PS1_SHOWCOLORHINTS=1

## Prompt
if [[ ${EUID} == 0 ]] ; then
   PS1='\[\033[01;30m\][\h]\[\033[01;34m\] \W \[\033[01;31m\]\$\[\033[00m\] '
else
   #PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
   PS1='\[\033[01;37m\][\h]\[\033[01;34m\] \W\[\033[0;36m\]$(__git_ps1 " (%s)")\[\033[0;34m\] \$\[\033[00m\] '
fi

# Setup fzf
if [ -r ~/.fzf/bin/fzf ]; then
    export PATH="$PATH:~/.fzf/bin"
    [ -f ~/.fzf/shell/key-bindings.bash] && . ~/.fzf/shell/key-bindings.bash
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

export wKazoo="/opt/kazoo"
export KAZOO_SRC="$wKazoo"
alias kz="cd $wKazoo"
