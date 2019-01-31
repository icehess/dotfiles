# Use my colors
if type -P dircolors >/dev/null ; then
   if [[ -f ~/.dir_colors ]] ; then
      eval $(dircolors -b ~/.dir_colors)
   elif [[ -f /etc/DIR_COLORS ]] ; then
      eval $(dircolors -b /etc/DIR_COLORS)
   fi
fi

type colordiff > /dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package

# Common junk
alias ..='cd ..'
alias ...='cd ../..'
alias lf='ls -F'
alias ll='ls -lAF'
alias la='ls -A'
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date
alias lm='la | more'

export EDITOR="vim"
export PAGER="less"

PLATFORM=`uname -s`
