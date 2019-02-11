PLATFORM="`uname -s | tr [:upper:] [:lower:]`"

if [ -f /etc/os-release ]; then
    LSB_FILE=/etc/os-release
elif [ -f /etc/lsb-release ]; then
    LSB_FILE=/etc/lsb-release
elif [ -f /etc/alpine-release ]; then
    export DISTRO_TYPE=alpine
elif [ -f /etc/arch-release ]; then
    export DISTRO_TYPE=archlinux
elif [ -f /etc/debian_version ]; then
    # Older Debian/Ubuntu/etc.
    export DISTRO_TYPE=debian
elif type lsb_release >/dev/null 2>&1; then
    export DISTRO_TYPE=$(lsb_release -si)
fi

if [ -n "$LSB_FILE" ]; then
    export DISTRO_TYPE=`cat $LSB_FILE | tr [:upper:] [:lower:] | grep -Eoi '(debian|ubuntu|red hat|centos|alpine|archlinux|arch linux|gentoo)' | sed 's/ *//g' | uniq`
    unset LSB_FILE
fi

# Use my colors
if type -P dircolors >/dev/null ; then
   if [[ -f ~/.dir_colors ]] ; then
      eval $(dircolors -b ~/.dir_colors)
   elif [[ -f /etc/DIR_COLORS ]] ; then
      eval $(dircolors -b /etc/DIR_COLORS)
   fi
fi

if [ ! "`ls --version | grep -ic "GNU coreutils"`" -eq 0 ]; then
    alias ls="ls --color"
fi

if [ ! "`grep --version | grep -ic "GNU grep"`" -eq 0 ]; then
    alias grep="grep --color"
fi

type colordiff > /dev/null 2>&1 && alias diff='colordiff'              # requires colordiff package

# Common junk
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias lf='ls -F'
alias ll='ls -lAF'
alias la='ls -A'
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date

type vim  > /dev/null 2>&1 && export EDITOR=vim
if type nvim  > /dev/null 2>&1 ; then
    alias vim=nvim
    export EDITOR=nvim
fi
type less > /dev/null 2>&1 && export PAGER=less

# sorry, but I need my colors in vim
export TERM="xterm-256color"
