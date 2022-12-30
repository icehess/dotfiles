# -*- mode: sh-script; -*-
# vim: set ft=sh:

# SSH Option 1: Vanilla ssh-agent {{{
if [ -z "$(pgrep ssh-agent)" ]; then
    rm -rf /tmp/ssh-*
    eval $(ssh-agent -s) > /dev/null
else
    export SSH_AGENT_PID=$(pgrep ssh-agent)
    export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent.*)
fi

# if ssh into this machine, and you don't have ssh-agent this prints garbage warn
if [ -f ~/.ssh/id_ed25519 -a `ssh-add -l | grep -c id_ed25519` -eq 0 ]; then
    ssh-add ~/.ssh/id_ed25519
fi
# }}}

# SSH Option 2: Keychain {{{
eval $(keychain --eval --agents ssh id_ed25519 2>/dev/null)
# }}}

# Lang and tools setup {{{
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# export PATH=$PATH:$(go env GOPATH)/bin

# . ${HOME}/.asdf/asdf.sh
# . ${HOME}/.asdf/completions/asdf.bash

# # Hook direnv into your shell.
# eval "$(asdf exec direnv hook bash)"

# # A shortcut for asdf managed direnv.
# direnv() { asdf exec direnv "$@"; }
# }}}
