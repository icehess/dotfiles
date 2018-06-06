#!/usr/bin/env sh

_install_brew() {
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
}

type brew > /dev/null 2>&1 || echo _install_brew

INSTALL_PKS=""

[ -f /usr/local/bin/git ] || INSTALL_PKS="git $INSTALL_PKS"
[ -f /usr/local/bin/vim ] || INSTALL_PKS="vim $INSTALL_PKS"
[ -f /usr/local/bin/fzf ] || INSTALL_PKS="fzf $INSTALL_PKS"
[ -f /usr/local/bin/ctags ] || INSTALL_PKS="ctags $INSTALL_PKS"
[ -f /usr/local/bin/ag ] || INSTALL_PKS="the_silver_searcher $INSTALL_PKS"

[ -n $INSTALL_PKS ] && brew install $INSTALL_PKS

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim +PluginInstall +qall
