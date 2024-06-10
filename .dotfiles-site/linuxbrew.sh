if [ -f /home/linuxbrew/.linuxbrew/bin/brew ]; then
    export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin${PATH+:$PATH}";
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
    export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
    export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";

    # If not running interactively, don't do anything
    [ -z "$PS1" ] && return

    export MANPATH="/home/linuxbrew/.linuxbrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";

    if [ -d /home/linuxbrew/bash-completion ]; then
        if [ -n "$BASH_COMPLETION_USER_DIR" ]; then
            export BASH_COMPLETION_USER_DIR="/home/linuxbrew/bash-completion:$BASH_COMPLETION_USER_DIR"
        else
            export BASH_COMPLETION_USER_DIR="/home/linuxbrew/bash-completion"
        fi
    elif [ -d /home/linuxbrew/.linuxbrew/etc/bash_completion.d ]; then
        . /home/linuxbrew/.linuxbrew/etc/bash_completion.d/brew
    fi

    if [ -d /home/linuxbrew/.linuxbrew/share/applications ]; then
      if [ -n "${XDG_DATA_DIRS}" ]; then
        export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share:${XDG_DATA_DIRS}"
      else
        export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share"
      fi
    fi
fi
