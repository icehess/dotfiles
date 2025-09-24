if [ -f /home/linuxbrew/.linuxbrew/bin/brew ]; then
    # Do not double source
    # echo "fuck profile.d"
    # [ -n "${LINUXBREW_WAS_HERE}" ] && return
    # export LINUXBREW_WAS_HERE=true
    # echo "fucked profile.d"

    export PATH="${PATH+$PATH:}/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin";
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
    export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
    export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";

    # If not running interactively, don't do anything
    [ -z "$PS1" ] && return

    export MANPATH="${MANPATH+$MANPATH:}/home/linuxbrew/.linuxbrew/share/man";
    if [ -n "${INFOPATH}" ]; then
        export INFOPATH="${INFOPATH}:/home/linuxbrew/.linuxbrew/share/info";
    else
        export INFOPATH="/home/linuxbrew/.linuxbrew/share/info";
    fi

    if [ -d /home/linuxbrew/bash-completion ]; then
        if [ -n "$BASH_COMPLETION_USER_DIR" ]; then
            export BASH_COMPLETION_USER_DIR="$BASH_COMPLETION_USER_DIR:/home/linuxbrew/bash-completion"
        else
            export BASH_COMPLETION_USER_DIR="/home/linuxbrew/bash-completion"
        fi
    elif [ -d /home/linuxbrew/.linuxbrew/etc/bash_completion.d ]; then
        . /home/linuxbrew/.linuxbrew/etc/bash_completion.d/brew
    fi

    if [ -d /home/linuxbrew/.linuxbrew/share/applications ]; then
      if [ -n "${XDG_DATA_DIRS}" ]; then
        export XDG_DATA_DIRS="${XDG_DATA_DIRS}:/home/linuxbrew/.linuxbrew/share"
      else
        export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share"
      fi
    fi
fi
