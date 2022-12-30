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
    pacaur -S $@ || _die "failed to install"
}

function _maybe_install_package() {
    if [ ! -f $1 ]; then
        _install_package $2
    else
        echo "'$2' is already installed."
    fi
}

_maybe_install_package /usr/bin/git git

DOTFILE_BARE_DIR="$HOME/.dotfiles"
TMP_DOTFILE="$(mktemp --directory --tmpdir=$HOME dotfiles.tmp.XXXX)"

_info "Bare cloning dotfiles to $DOTFILE_BARE_DIR"
[ -d $DOTFILE_BARE_DIR/.git ] && _die "clone dir $DOTFILE_BARE_DIR is exists and not empty"
if [ ! -r $DOTFILE_BARE_DIR/HEAD ]; then
    git clone --bare git@github.com:icehess/dotfiles.git $DOTFILE_BARE_DIR || _die "failed to bare clone"
fi
echo

function config {
    git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$HOME $@
}

function tmp_config {
    git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$TMP_DOTFILE $@
}

_info "Checking out master branch to $TMP_DOTFILE"
tmp_config checkout . || _die "something failed during checking out"
echo

BAK_DIR="$(mktemp --directory --tmpdir=$HOME home.bak.XXXXXXXX)"

_info "Backing up curent dotfiles to $BAK_DIR direcotry"
for file in $(ls -A $TMP_DOTFILE) ; do
    filename="`basename $file`"
    if [ -e "$HOME/$filename" ] ; then
        echo " - $filename"
        mv "$HOME/$filename" "$BAK_DIR/" || _die "failed to move file $filename to $BAK_DIR"
    fi
done
echo "done"
echo

 _info "Checking out master branch to $HOME"
rm $TMP_DOTFILE
config checkout master . || _die "failed to checkout"
config config status.showUntrackedFiles no
echo

_info "Installing required packages"
INSTALL_PKS=""
type vim > /dev/null 2>&1 || INSTALL_PKS="vim $INSTALL_PKS"
type ctags > /dev/null 2>&1 || INSTALL_PKS="ctags $INSTALL_PKS"
type ag > /dev/null 2>&1 || INSTALL_PKS="the_silver_searcher $INSTALL_PKS"
type colordiff > /dev/null 2>&1 || INSTALL_PKS="colordiff $INSTALL_PKS"

[ -n $INSTALL_PKS ] && _install_package $INSTALL_PKS
echo

_info "Installing fzf"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
echo

_info "Setting up vim"
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall
cp ~/.dotfiles-site/files/vim/plugins/kazoo_erlc.vim ~/.vim/bundle/ale/ale_linters/erlang/
echo

_info "Setting up Sublime Text 3 Package Conrtol"
echo
mkdir -p "$HOME/.config/sublime-text-3/Installed Packages/"
curl "https://packagecontrol.io/Package%20Control.sublime-package" > "$HOME/.config/sublime-text-3/Installed Packages/Package Control.sublime-package"
echo

# _info "Setting up VS Code Insider"
# CODE_PLUGINS=""
# CODE_PLUGINS="Equinusocio.vsc-material-theme $CODE_PLUGINS"
# CODE_PLUGINS="formulahendry.docker-explorer $CODE_PLUGINS"
# CODE_PLUGINS="ms-kubernetes-tools.vscode-kubernetes-tools $CODE_PLUGINS"
# CODE_PLUGINS="ms-vscode.cpptools $CODE_PLUGINS"
# CODE_PLUGINS="ms-vscode.node-debug2 $CODE_PLUGINS"
# CODE_PLUGINS="ms-vscode.PowerShell $CODE_PLUGINS"
# CODE_PLUGINS="p1c2u.docker-compose $CODE_PLUGINS"
# CODE_PLUGINS="PeterJausovec.vscode-docker $CODE_PLUGINS"
# CODE_PLUGINS="pgourlain.erlang $CODE_PLUGINS"
# CODE_PLUGINS="redhat.vscode-yaml $CODE_PLUGINS"
# CODE_PLUGINS="robertohuertasm.vscode-icons $CODE_PLUGINS"
# CODE_PLUGINS="vsmobile.vscode-react-native $CODE_PLUGINS"
# CODE_PLUGINS="yuce.erlang-otp $CODE_PLUGINS"
# CODE_PLUGINS="zhuangtongfa.Material-theme $CODE_PLUGINS"

# for plugin in "$CODE_PLUGINS" ; do
#     echo "code-insider --install-extension $plugin"
# done

_info "Installing Eye Candies"
INSTALL_PKS=""

type nitrogen > /dev/null 2>&1 || INSTALL_PKS="nitrogen $INSTALL_PKS"
type compton > /dev/null 2>&1 || INSTALL_PKS="compton $INSTALL_PKS"
type kwalletd5 > /dev/null 2>&1 || INSTALL_PKS="kwallet $INSTALL_PKS"
[ -f /usr/lib/pam_kwallet_init ] || INSTALL_PKS="kwallet-pam $INSTALL_PKS"
type rofi > /dev/null 2>&1 || INSTALL_PKS="rofi-greenclip $INSTALL_PKS" # AUR
type pa-applet > /dev/null 2>&1 || INSTALL_PKS="pa-applet-git $INSTALL_PKS"
type pasystray > /dev/null 2>&1 || INSTALL_PKS="pasystray-gtk3-standalone $INSTALL_PKS"
INSTALL_PKS="faenza-cupertino-icon-theme $INSTALL_PKS"
INSTALL_PKS="vertex-themes $INSTALL_PKS"
INSTALL_PKS="otf-san-francisco $INSTALL_PKS"
type dig > /dev/null 2>&1 || INSTALL_PKS="bind-tools $INSTALL_PKS"
type tree > /dev/null 2>&1 || INSTALL_PKS="tree $INSTALL_PKS"
type which > /dev/null 2>&1 || INSTALL_PKS="which $INSTALL_PKS"
type whois > /dev/null 2>&1 || INSTALL_PKS="whois $INSTALL_PKS"
type xbacklight > /dev/null 2>&1 || INSTALL_PKS="xorg-xbacklight $INSTALL_PKS"
type termite > /dev/null 2>&1 || INSTALL_PKS="termite termite-terminfo $INSTALL_PKS"

_info "Installing: $INSTALL_PKS"
_install_package $INSTALL_PKS

popd > /dev/null
