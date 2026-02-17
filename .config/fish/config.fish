## Environment setup
# Apply .profile: use this to put fish compatible .profile stuff in
if test -f ~/.fish_profile
    source ~/.fish_profile
end

## Path setup
if test -f /home/linuxbrew/.linuxbrew/bin/brew
    set -x HOMEBREW_NO_ANALYTICS 1
    /home/linuxbrew/.linuxbrew/bin/brew shellenv | source
else if test -f /opt/homebrew/bin/brew
    set -x HOMEBREW_NO_ANALYTICS 1
    /opt/homebrew/bin/brew shellenv | source
end

if type -q mise
    mise activate fish | source
end

if test -d ~/.local/share/nvim/mason/bin
    set -p PATH ~/.local/share/nvim/mason/bin
end
if test -d ~/bin
    if not contains -- ~/bin $PATH
        set -p PATH ~/bin
    end
end

if test -d ~/.local/bin
    if not contains -- ~/.local/bin $PATH
        set -p PATH ~/.local/bin
    end
end

bind up history-prefix-search-backward
set -x FZF_CTRL_R_COMMAND
set -x FZF_ALT_C_COMMAND
if functions -q fzf_key_bindings
    fzf_key_bindings
end

# source ~/.config/fish/functions/handy.fish
source ~/.config/fish/conf.d/alias.fish

# Format man pages
if type -q bat
    set -x MANROFFOPT -c
    set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"
end

## Da rest
if type -q helix
    alias hx='helix'
    set -x EDITOR hx
else if type -q hx
    set -x EDITOR hx
else if type -q nvim
    alias vim='nvim'
    set -x EDITOR nvim
else if type -q vim
    set -x EDITOR vim
else if type -q vi
    alias vim='vi'
    alias nvim='vi'
    set -x EDITOR vi
end

# And at the end, my local per host configs
if test -f ~/.local.fish
    source ~/.local.fish
end
