# If not running interactively, don't do anything
if [[ $- != *i* ]] ; then
    [ -d /usr/local/sbin ] && export PATH="/usr/local/sbin:$PATH"
    [ -d /usr/local/bin ] && export PATH="/usr/local/bin:$PATH"
   return
fi

shopt -s checkwinsize

# Write to history whenever the prompt is displayed
PROMPT_COMMAND='history -a'
shopt -s histappend
history -a
HISTSIZE=1100000
HISTFILESIZE=1100000
HISTCONTROL="ignoredups:erasedups"

# Change the window title of X terminals
case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*|terminator*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "$USER" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
    ;;
  screen)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "$USER" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
    ;;
esac

# Use my colors
if type -P dircolors >/dev/null ; then
   if [[ -f ~/.dir_colors ]] ; then
      eval $(dircolors -b ~/.dir_colors)
   elif [[ -f /etc/DIR_COLORS ]] ; then
      eval $(dircolors -b /etc/DIR_COLORS)
   fi
fi

## Prompt
if [[ ${EUID} == 0 ]] ; then
   PS1='\[\033[01;30m\]\h\[\033[01;34m\] \W \[\033[01;31m\]\$\[\033[00m\] '
   #PS1='\[\033[01;30m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
   #PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"
else
   #PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
   PS1='\[\033[01;37m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
   #PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"
fi

#Use color for ls and grep
[[ "$OSTYPE" == "darwin"* || "$OSTYPE" == "freebsd"* ]] && alias ls='ls -G' || alias ls='ls --color=auto'
[[ "$OSTYPE" == "darwin"* || "$OSTYPE" == "freebsd"* ]] && alias grep='grep --colour=auto' || grep='grep --color=auto'
type colordiff > /dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package
alias less='less -R'

PS2='> '
PS3='> '
PS4='+ '

# Common junk
[[ -s "$HOME/.dotfiles-site/bash_alias" ]] && source "$HOME/.dotfiles-site/bash_alias"
#[[ -s "$HOME/.dotfiles-site/functions" ]] && source "$HOME/.dotfiles-site/functions"

## setting bash_completion
[ -r /etc/bash_completion   ] && . /etc/bash_completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

[ -r /usr/share/bash_completion/bash_completion ] && . /usr/share/bash_completion/bash_completion

[[ -d "$HOME/bin" ]] && PATH="$PATH:$HOME/bin"

export EDITOR="vim"

#[ -r /usr/bin/mate-terminal ] && export TERMINAL="mate-terminal"
[ -r /usr/bin/termite ] && export TERMINAL="termite"
[ -r /usr/bin/ksshaskpass ] && export SSH_ASKPASS="/usr/bin/ksshaskpass"
[ -r $XDG_RUNTIME_DIR/ssh-agent.socket ] && export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

PLATFORM=`uname -s`

alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

alias work="cd $HOME/work"
alias 2222="cd $HOME/work/2600hz"
alias kazoo="cd $HOME/work/2600hz/kazoo"
alias kz="cd $HOME/work/2600hz/kazoo/master; export KAZOO_SRC=$HOME/work/2600hz/kazoo/master"
alias kz2="cd $HOME/work/2600hz/kazoo/4.2; export KAZOO_SRC=$HOME/work/2600hz/kazoo/4.2"
alias kz1="cd $HOME/work/2600hz/kazoo/4.1; export KAZOO_SRC=$HOME/work/2600hz/kazoo/4.1"

if [ -e ~/.bashlocal ]; then
    . ~/.bashlocal
fi

