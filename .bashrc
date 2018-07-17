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
   PS1='\[\033[01;30m\]\h\[\033[01;31m\] \W \[\033[01;31m\]\$\[\033[00m\] '
else
   PS1='\[\033[01;35m\]\h\[\033[01;32m\] \W \$\[\033[00m\] '
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

export PAGER="less"

PLATFORM=`uname -s`

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
