#!/usr/bin/env bash

pushd $(dirname $0) > /dev/null

cd "$HOME"

_info() {
    printf "\e[1;36m::\e[1;37m $1 \e[00m \n"
}

function _die() {
    printf "\e[1;37m::\e[1;31m $1 \e[00m \n"
    exit 1
}

DOTFILE_BARE_DIR="$HOME/.dotfiles"

_info "Bare cloning dotfiles to $DOTFILE_BARE_DIR"
[ -d $DOTFILE_BARE_DIR/.git ] && _die "clone dir $DOTFILE_BARE_DIR is exists and not empty"
if [ ! -r $DOTFILE_BARE_DIR/HEAD ]; then
    git clone --bare git@github.com:icehess/dotfiles.git $DOTFILE_BARE_DIR || _die "failed to bare clone"
fi
echo

function config {
    git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$HOME $@
}

 _info "Checking out master branch to $HOME"
config checkout master . || _die "failed to checkout"
config config status.showUntrackedFiles no
echo

_info "Setting up neovim"
nvim +PlugInstall +qall
cp ~/.dotfiles-site/files/vim/plugins/kazoo_erlc.vim ~/.vim/bundle/ale/ale_linters/erlang/
echo

popd > /dev/null
