# Init Path {{{
[ -d /usr/local/sbin ] && export PATH="/usr/local/sbin:${PATH}"
[ -d /usr/local/bin ] && export PATH="/usr/local/bin:${PATH}"

[ -d /usr/local/opt/coreutils/libexec/gnubin ] && export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
[ -d /usr/local/opt/findutils/libexec/gnubin ] && export PATH="/usr/local/opt/findutils/libexec/gnubin:${PATH}"
[ -d /usr/local/opt/gnu-sed/libexec/gnubin ] && export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
[ -d /usr/local/opt/make/libexec/gnubin ] && export PATH="/usr/local/opt/make/libexec/gnubin:${PATH}"

[ -d /opt/homebrew/opt/coreutils/libexec/gnubin ] && export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:${PATH}"
[ -d /opt/homebrew/opt/findutils/libexec/gnubin ] && export PATH="/opt/homebrew/opt/findutils/libexec/gnubin:${PATH}"
[ -d /opt/homebrew/opt/gnu-sed/libexec/gnubin ] && export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:${PATH}"
[ -d /opt/homebrew/opt/make/libexec/gnubin ] && export PATH="/opt/homebrew/opt/make/libexec/gnubin:${PATH}"

[ -d /opt/ruby/bin ] && export PATH="/usr/local/opt/ruby/bin:${PATH}"
[ -d /opt/homebrew/opt/ruby/bin ] && export PATH="/opt/homebrew/opt/ruby/bin:${PATH}"
[ -r "${HOME}/.cargo/env" ] && . "${HOME}/.cargo/env"

[ -d "$HOME/bin" ] && export PATH="$HOME/bin:${PATH}"
[ -d "$HOME/.local/bin" ] && export PATH="${HOME}/.local/bin:${PATH}"
[ -d "${HOME}/.local/share/nvim/mason/bin" ] && export PATH="${HOME}/.local/share/nvim/mason/bin:${PATH}"
[ -f "/Applications/WezTerm.app/Contents/MacOS/wezterm" ] && export PATH="/Applications/WezTerm.app/Contents/MacOS:${PATH}"

if [ -f /opt/homebrew/bin/brew ]; then
    # this is slow, that is why we export directly
    # eval "$(/opt/homebrew/bin/brew shellenv)"
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
elif [ -f /usr/local/bin/brew ]; then
    # this is slow, that is why we export directly
    # eval "$(/usr/local/bin/brew shellenv)"
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
# Instead of add ~/.dotfiles-site/linuxbrew.sh to /etc/bash.bashrc
# elif [ -f /home/linuxbrew/.linuxbrew/bin/brew ]; then
#     export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
#     export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
#     export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";
#     export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin${PATH+:$PATH}";
#     export MANPATH="/home/linuxbrew/.linuxbrew/share/man${MANPATH+:$MANPATH}:";
#     export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";
fi
# }}}

# If not running interactively, don't do anything
if [[ $- != *i* ]] ; then
   if type -P mise >/dev/null ; then
       eval "$(mise activate bash --shims)"
   elif [ -f /usr/local/opt/asdf/libexec/asdf.sh ]; then
       . /usr/local/opt/asdf/libexec/asdf.sh
   elif [ -f /opt/homebrew/opt/asdf/libexec/asdf.sh ]; then
       . /opt/homebrew/opt/asdf/libexec/asdf.sh
   elif [ -f /opt/asdf-vm/asdf.sh ]; then
       . /opt/asdf-vm/asdf.sh
   elif [ -f ${HOME}/.asdf/asdf.sh ]; then
       . ${HOME}/.asdf/asdf.sh
   fi

   return
fi

# Good Shell {{{
# If set, bash checks the window size after each external (non-builtin) command and, if necessary, updates the values of LINES
# and COLUMNS.  This option is enabled by default.
shopt -s checkwinsize

# shut up macOS cwd echo (in Terminal app, iTerm is okay):
# > It updates the prompt to echo the Current Working Directory (CWD) and is defined in /etc/bashrc
update_terminal_cwdprintf() {
 echo -n
}
export BASH_SILENCE_DEPRECATION_WARNING=1

# append new history items to .bash_history
shopt -s histappend
# don't put duplicate lines or lines starting with space in the history
HISTCONTROL="ignoredups:erasedups"
# increase history file size
HISTFILESIZE=1000000
# increase history size
HISTSIZE=${HISTFILESIZE}
# append new entries from memory to .bash_history, and vice-versa
PROMPT_COMMAND="history -a; history -n;" ## ${PROMPT_COMMAND}"

if type -P direnv >/dev/null ; then
    eval "$(direnv hook bash)"
fi

# Change the window title of X terminals
# if something adds to PROMPT_COMMAND later, there would be no space between that and printf
# case ${TERM} in
#   xterm*|rxvt*|Eterm|aterm|kterm|gnome*|terminator*)
#     PROMPT_COMMAND=${PROMPT_COMMAND}'printf "\033]0;%s@%s:%s\007" "$USER" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
#     ;;
#   screen)
#     PROMPT_COMMAND=${PROMPT_COMMAND}'printf "\033_%s@%s:%s\033\\" "$USER" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
#     ;;
# esac

# Use my colors
if type -P dircolors >/dev/null ; then
   if [[ -f ~/.dir_colors ]] ; then
      eval $(dircolors -b ~/.dir_colors)
   elif [[ -f /etc/DIR_COLORS ]] ; then
      eval $(dircolors -b /etc/DIR_COLORS)
   fi
fi
# }}}

# Shell Prompt {{{
if [ -f /usr/share/git/git-prompt.sh ]; then
    source /usr/share/git/git-prompt.sh
elif [ -f "~/.git-prompt.sh" ]; then
    source "~/.git-prompt.sh"
elif [ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]; then
    # CentOS
    source /usr/share/git-core/contrib/completion/git-prompt.sh
elif [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
    source /usr/local/etc/bash_completion.d/git-prompt.sh
elif [ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ]; then
    source /opt/homebrew/etc/bash_completion.d/git-prompt.sh
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
   PS1='\[\033[01;30m\]\h\[\033[01;34m\] \W \[\033[01;31m\]\$\[\033[00m\] '
   #PS1='\[\033[01;30m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
   #PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"
else
   #PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
   PS1='\[\033[01;37m\]\h\[\033[01;34m\] \W\[\033[0;36m\]$(__git_ps1 " (%s)")\[\033[0;34m\] \$\[\033[00m\] '
   #PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"
fi

PS2='> '
PS3='> '
PS4='+ '
# }}}

# Setting bash_completion {{{

## Common (Ubuntu? Debian?)
if [ -r /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# # Instead of add ~/.dotfiles-site/linuxbrew.sh to /etc/bash.bashrc
# if [ -d /home/linuxbrew/.linuxbrew/etc/bash_completion.d ]; then
#     if [ -n "$BASH_COMPLETION_USER_DIR" ]; then
#         export BASH_COMPLETION_USER_DIR="/home/linuxbrew/.linuxbrew/etc/bash_completion.d:$BASH_COMPLETION_USER_DIR"
#     else
#         export BASH_COMPLETION_USER_DIR="/home/linuxbrew/.linuxbrew/etc/bash_completion.d"
#     fi
#     . /home/linuxbrew/.linuxbrew/etc/bash_completion.d/brew
# fi


## Arch (it is already being source by /etc/bash.bashrc)
# [ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# macOS
[ -r /usr/local/etc/profile.d/bash_completion.sh ] && . /usr/local/etc/profile.d/bash_completion.sh
[ -r /opt/homebrew/etc/profile.d/bash_completion.sh ] && . /opt/homebrew/etc/profile.d/bash_completion.sh


[ -f /opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash ] && . /opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash
[ -f ${HOME}/.asdf/completions/asdf.bash ] && . ${HOME}/.asdf/completions/asdf.bash

if type -P mise >/dev/null ; then
    eval "$(mise activate bash)"
fi
# }}}

# Aliases {{{
alias ..='cd ..'
alias ...='cd ../..'

alias ll='ls -lAF'

alias simplehttpd='python3 -m http.server';
alias simplehttpd2='python -m SimpleHTTPServer';
alias jsonformat='python -m json.tool'

if type -P nvim >/dev/null ; then
    alias vim='nvim'
    export EDITOR='nvim'
elif type -P hx >/dev/null ; then
    export EDITOR="hx"
elif type -P vim >/dev/null ; then
    export EDITOR='vim'
elif type -P vi >/dev/null ; then
    alias vim='vi'
    export EDITOR='vi'
fi
alias cemacs='emacs --no-window-system'

if type broot > /dev/null 2>&1; then
    if [ -f ~/.config/broot/launcher/bash/br ]; then
        source ~/.config/broot/launcher/bash/br
    fi
fi

# [[ "$OSTYPE" == "darwin"* || "$OSTYPE" == "freebsd"* ]] && alias grep='grep --colour=auto' || grep='grep --color=auto'
if [ -n "${OSTYPE}" ]; then
    if [ "${OSTYPE}" == 'linux-gnu' ]; then
        alias grep='grep --color=auto'
    else
        alias grep='grep --colour=auto'
    fi
fi

# [[ "$OSTYPE" =~ linux* ]] && alias ls='ls -G' || alias ls='ls --color=auto'
alias ls='ls --color=auto'

type colordiff > /dev/null 2>&1 && alias diff='colordiff'

type less >/dev/null 2>&1 && alias less='less -R'
type less >/dev/null 2>&1 && export PAGER='less'

alias config="git --git-dir=${HOME}/.dotfiles/ --work-tree=${HOME}"
if [ -f /usr/share/bash-completion/completions/git ]; then
    source /usr/share/bash-completion/completions/git
    __git_complete config __git_main
elif [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    source /usr/local/etc/bash_completion.d/git-completion.bash
    __git_complete config __git_main
elif [ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ]; then
    source /opt/homebrew/etc/bash_completion.d/git-completion.bash
    __git_complete config __git_main
fi

alias gitAuthorMe='git config user.email "icehess@gmail.com"; git config user.name "Hesaam Farhang"'
alias configAuthorMe='config config user.email "icehess@gmail.com"; config config user.name "Hesaam Farhang"'
# }}}

[ -s "${HOME}/.dotfiles-site/functions" ] && source "${HOME}/.dotfiles-site/functions"

# PLATFORM=`uname -s`
# if [ x"`uname -a | grep -o Microsoft | uniq`" = x"Microsoft" ]; then
#     PLATFORM="Microsoft"
# elif [ x"`uname -a | grep -o Darwin | uniq`" = x"Darwin" ]; then
#     PLATFORM="Darwin"
# fi

# fzf {{{

if [ -d /home/linuxbrew/.linuxbrew/opt/fzf/shell ]; then
    . /home/linuxbrew/.linuxbrew/opt/fzf/shell/key-bindings.bash
    . /home/linuxbrew/.linuxbrew/opt/fzf/shell/completion.bash
elif [ -f /usr/share/fzf/key-bindings.bash ]; then
    . /usr/share/fzf/key-bindings.bash
    . /usr/share/fzf/completion.bash
elif [ -f /usr/local/opt/fzf/shell/key-bindings.bash ]; then
    . /usr/local/opt/fzf/shell/key-bindings.bash
    . /usr/local/opt/fzf/shell/completion.bash
elif [ -f /opt/homebrew/opt/fzf/shell/key-bindings.bash ]; then
    . /opt/homebrew/opt/fzf/shell/key-bindings.bash
    . /opt/homebrew/opt/fzf/shell/completion.bash
elif [ -f ~/.fzf.bash ]; then
    . ~/.fzf.bash
fi
# }}}

# Work Dirs {{{
export iceWork="${iceWork:-${HOME}/work}"
export iceDock="${iceWork}/dockerfiles"

alias icework="cd ${iceWork}"
alias icedock="cd ${iceDock}"
# }}}

# WSL Dirs {{{
export wslHome="${wslHome:-/mnt/c/Users/hesaam}"
if [ -d "${wslHome}" ]; then
    export wslDocument="${wHome}/Documents"
    export wslWork="${wDocument}/work"

    alias wslHome="cd ${wslHome}"
    alias wslWork="cd ${wslWork}"
fi
# }}}

# Kazoo Stuff {{{
export wKazoo="${wKazoo:-${iceWork}/2600hz}"
export KAZOO_SRC="${KAZOO_SRC:-${wKazoo}/kazoo-master}"
export KZ_DOCKER_DIR="${KZ_DOCKER_DIR:-${iceDock}/kazoo}"
export KZ_COMPOSE_DIR="${KZ_DOCKER_DIR}/compose"
export KZ_DOCKER_DESKTOP="${KZ_DOCKER_DIR}/docker-desktop"
export KZ_DOCKER_DESKTOP_FILE="${KZ_DOCKER_DESKTOP}/docker-compose.yml"

alias kazoo="cd ${wKazoo}"
alias kz="cd ${wKazoo}/kazoo-master; export KAZOO_SRC=${wKazoo}/kazoo-master"
alias kz3="cd ${wKazoo}/kazoo-4.3; export KAZOO_SRC=${wKazoo}/kazoo-4.3"
alias kz50="cd ${wKazoo}/kazoo-5.0; export KAZOO_SRC=${wKazoo}/kazoo-5.0"
alias kz5="cd ${wKazoo}/kazoo-5.4; export KAZOO_SRC=${wKazoo}/kazoo-5.4"
alias docs="cd ${wKazoo}/2600hz-docs-astro"

alias appex="cd ${wKazoo}/appex-server"
alias appexx="cd ${wKazoo}/appex-marketplace"
alias market="cd ${wKazoo}/market-ecommerce-server"

alias kgit="${wKazoo}/kazoo-master/kgit"
[ -f ${wKazoo}/kazoo-master/kgit-completion.bash ] && . ${wKazoo}/kazoo-master/kgit-completion.bash

if [ -f "$KZ_DOCKER_DESKTOP_FILE" ]; then
    alias kzdoc="COMPOSE_FILE=${KZ_DOCKER_DESKTOP_FILE} docker"
    complete -F _docker kzdoc
fi
# }}}

# Progamming Lang Settings {{{
export ERL_AFLAGS="-kernel shell_history enabled ${ERL_AFLAGS}"
# }}}

if [ -e ~/.bashlocal ]; then
    . ~/.bashlocal
fi

## Here goes garbage forking stupid apps shitting their shits here:
#
# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "${KITTY_INSTALLATION_DIR}/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION

# Instead of add ~/.dotfiles-site/linuxbrew.sh to /etc/bash.bashrc
# if [ -d /home/linuxbrew/.linuxbrew/share/applications ]; then
#     if [ -n "${XDG_DATA_DIRS}" ]; then
#         export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share:${XDG_DATA_DIRS}"
#     else
#         export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share"
#     fi
# fi

export HOMEBREW_NO_ANALYTICS=1

# >>> Scala coursier install directory >>>
[ -d "${HOME}/.local/share/coursier/bin" ] && export PATH="$PATH:${HOME}/.local/share/coursier/bin"
# <<< coursier install directory <<<
