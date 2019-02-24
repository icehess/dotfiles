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

function _install_package() {
    sudo apt-get install $@ || _die "failed to install"
}

function _maybe_install_package() {
    _info "maybe install $1"
    if ! type "$1" > /dev/null 2>&1 ; then
        _install_package $2
    else
        echo "'$2' is already installed."
    fi
}

_maybe_install_package git git
_maybe_install_package curl curl

TMP_DOTFILE="$(mktemp --directory --tmpdir=$HOME dotfiles.tmp.XXXX)"
BAK_DIR="$(mktemp --directory --tmpdir=$HOME home.bak.XXXXXXXX)"

_info "Downloading tarball to $TMP_DOTFILE for getting list of files to backup"
(curl -q -L https://api.github.com/repos/icehess/dotfiles/tarball/master | tar -zx -C "$TMP_DOTFILE" --strip 1) || _die "failed to download"


_info "Backing up curent dotfiles to $BAK_DIR directory"
for file in $(ls -A $TMP_DOTFILE) ; do
    filename="`basename $file`"
    if [ -e "$HOME/$filename" ] ; then
        echo " - $filename"
        mv "$HOME/$filename" "$BAK_DIR/" || _die "failed to move file $filename to $BAK_DIR"
    fi
done
rm -rf $TMP_DOTFILE
echo "done"
echo


DOTFILE_BARE_DIR="$HOME/.dotfiles"
_info "Bare Cloning to $DOTFILE_BARE_DIR"

if [ -d "$DOTFILE_BARE_DIR" ]; then
    echo -n "Directory $DOTFILE_BARE_DIR is exists, removing it (Y|n):"
    read BYEBYE
    if [ x"$BYEBYE" = x"y" ]; then
        rm -rf "$DOTFILE_BARE_DIR"
        echo
    else
        echo  "okay do what you want!"
    fi
fi

git clone --bare git@github.com:icehess/dotfiles.git $DOTFILE_BARE_DIR || _die "failed to bare clone"
echo

function config {
    git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$HOME $@
}

_info "Checking out master branch to $HOME"
config checkout master || _die "failed to checkout"
config config status.showUntrackedFiles no
echo

_info "Installing required packages"
INSTALL_PKS=""
type nvim > /dev/null 2>&1 || INSTALL_PKS="neovim $INSTALL_PKS"
type ctags > /dev/null 2>&1 || INSTALL_PKS="ctags $INSTALL_PKS"
type ag > /dev/null 2>&1 || INSTALL_PKS="silversearcher-ag $INSTALL_PKS"
type colordiff > /dev/null 2>&1 || INSTALL_PKS="colordiff $INSTALL_PKS"
type curl > /dev/null 2>&1 || INSTALL_PKS="curl $INSTALL_PKS"

[ -n "$INSTALL_PKS" ] && _install_package $INSTALL_PKS
echo

if [ -d ~/.fzf ]; then
    _info "Installing fzf"
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
    echo
fi

_info "Setting up neovim"
nvim +PlugInstall +qall
cp ~/.dotfiles-site/files/vim/plugins/kazoo_erlc.vim ~/.vim/bundle/ale/ale_linters/erlang/
echo

SUBLIME_DIR="/mnt/c/Users/hesaam/AppData/Roaming/Sublime Text 3"
if [ -d "$SUBLIME_DIR" ] ; then
    _info "Setting up Sublime Text 3"
    echo " - backing existing data folder"
    mv "$SUBLIME_DIR" "$BAK_DIR/"

    cp -R ".config/sublime-text-3" "$SUBLIME_DIR"

    mkdir -p "$SUBLIME_DIR/Installed Packages/"
    curl "https://packagecontrol.io/Package%20Control.sublime-package" > "$SUBLIME_DIR/Installed Packages/Package Control.sublime-package"
echo
fi

VSCODE_DIR="/mnt/c/Users/hesaam/AppData/Roaming/Code - Insiders"
if [ -d "$VSCODE_DIR" ] ; then
  _info "Setting up VS Code Insider"

  echo "WTF M$, you have to run 'code-insiders --list-extensions' from Command Prompt,"
  echo "then install plugins by running these commands:"

  CODE_PLUGINS=""
  CODE_PLUGINS="Equinusocio.vsc-material-theme $CODE_PLUGINS"
  CODE_PLUGINS="formulahendry.docker-explorer $CODE_PLUGINS"
  CODE_PLUGINS="ms-kubernetes-tools.vscode-kubernetes-tools $CODE_PLUGINS"
  CODE_PLUGINS="ms-vscode.cpptools $CODE_PLUGINS"
  CODE_PLUGINS="ms-vscode.node-debug2 $CODE_PLUGINS"
  CODE_PLUGINS="ms-vscode.PowerShell $CODE_PLUGINS"
  CODE_PLUGINS="p1c2u.docker-compose $CODE_PLUGINS"
  CODE_PLUGINS="PeterJausovec.vscode-docker $CODE_PLUGINS"
  CODE_PLUGINS="pgourlain.erlang $CODE_PLUGINS"
  CODE_PLUGINS="redhat.vscode-yaml $CODE_PLUGINS"
  CODE_PLUGINS="robertohuertasm.vscode-icons $CODE_PLUGINS"
  CODE_PLUGINS="vsmobile.vscode-react-native $CODE_PLUGINS"
  CODE_PLUGINS="yuce.erlang-otp $CODE_PLUGINS"
  CODE_PLUGINS="zhuangtongfa.Material-theme $CODE_PLUGINS"

  for plugin in "$CODE_PLUGINS" ; do
    echo "code --install-extension $plugin"
  done


  mv "$VSCODE_DIR/User/settings.json" "$BAK_DIR/code-insiders.settings.json"
  cp ".config/Code - Insiders/User/settings.json" "$VSCODE_DIR/User/settings.json"
fi


INSTALL_PKS=""

#type nitrogen > /dev/null 2>&1 || INSTALL_PKS="nitrogen $INSTALL_PKS"
#type compton > /dev/null 2>&1 || INSTALL_PKS="compton $INSTALL_PKS"
#type kwalletd5 > /dev/null 2>&1 || INSTALL_PKS="kwallet $INSTALL_PKS"
#[ -f /usr/lib/pam_kwallet_init ] || INSTALL_PKS="kwallet-pam $INSTALL_PKS"
#type rofi > /dev/null 2>&1 || INSTALL_PKS="rofi-greenclip $INSTALL_PKS" # AUR
#type pa-applet > /dev/null 2>&1 || INSTALL_PKS="pa-applet-git $INSTALL_PKS"
#type pasystray > /dev/null 2>&1 || INSTALL_PKS="pasystray-gtk3-standalone $INSTALL_PKS"
#INSTALL_PKS="faenza-cupertino-icon-theme $INSTALL_PKS"
#INSTALL_PKS="vertex-themes $INSTALL_PKS"
#INSTALL_PKS="otf-san-francisco $INSTALL_PKS"
type dig > /dev/null 2>&1 || INSTALL_PKS="dnsutils $INSTALL_PKS"
type tree > /dev/null 2>&1 || INSTALL_PKS="tree $INSTALL_PKS"
#type which > /dev/null 2>&1 || INSTALL_PKS="which $INSTALL_PKS"
type whois > /dev/null 2>&1 || INSTALL_PKS="whois $INSTALL_PKS"
#type xbacklight > /dev/null 2>&1 || INSTALL_PKS="xorg-xbacklight $INSTALL_PKS"
#type termite > /dev/null 2>&1 || INSTALL_PKS="termite termite-terminfo $INSTALL_PKS"

[ -n "$INSTALL_PKS" ] && _info "Installing Eye Candies" && _info "Installing: $INSTALL_PKS" && _install_package $INSTALL_PKS


popd > /dev/null
