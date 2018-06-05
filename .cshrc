# Aliases
alias h         history 25
alias jff               jobs -l
alias la        ls -aF
alias lf        ls -FA
alias ll        ls -lAF

# A righteous umask
umask 22

set path = (/sbin /bin /usr/sbin /usr/bin /usr/games /usr/local/sbin /usr/local/bin $HOME/bin)

setenv  EDITOR  vi
setenv  PAGER   less
setenv  BLOCKSIZE       K

if ($?prompt) then
        # An interactive shell -- set some stuff up
        if ($uid == 0) then
                set user = root
        endif
        set prompt = "[%n:%~]%# "
        set promptchars = "%#"

        set filec
        set rmstar
        set history = 100000
        set savehist = (100000 merge)
        set autolist = ambiguous
        # Use history to aid expansion
        set autoexpand
        set autorehash
        setenv CLICOLOR
        set color
        set no beep
        set mail = (/var/mail/$USER)
        if ( $?tcsh ) then
                bindkey "^W" backward-delete-word
                bindkey -k up history-search-backward
                bindkey -k down history-search-forward
        endif

endif
