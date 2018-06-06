#!/usr/bin/env bash

[ -f /usr/bin/git ] || sudo apt-get install git

_info() {
	printf "\e[1;37m::\e[1;36m $1 \e[0:0m \n"
}

DOTFILE_BARE_DIR="$HOME/.dotfiles"
TMP_DOTFILE="$(mktemp --directory --tmpdir=$HOME dotfiles.tmp.XXXX)"

_info "Bare cloning dotfiles"
[ -d $DOTFILE_BARE_DIR/.git ] || git clone --bare git@github.com:icehess/dotfiles.git $DOTFILE_BARE_DIR
echo

function config {
   git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$HOME $@
}

function tmp_config {
   git --git-dir=$DOTFILE_BARE_DIR/ --work-tree=$TMP_DOTFILE $@
}

_info "Checking out wsl branch to $TMP_DOTFILE"
tmp_config checkout wsl || (echo "something failed during checking out" && exit 1)
echo

BAK_DIR="$(mktemp --directory --tmpdir=$HOME home.bak.XXXXXXXX)"

_info "Backing up curent dotfiles to $BAK_DIR direcotry"
for file in $(ls -A $TMP_DOTFILE) ; do
	if [ -e "$file" ] ; then
		echo " - $file"
		mv "$file" "$TMP_DOTFILE/"
	fi
done
echo "done"
echo

_info "Checking out wsl branch to $HOME"
rm $TMP_DOTFILE
config checkout wsl
config config status.showUntrackedFiles no
echo

_info "Installing required packages"
INSTALL_PKS=""
type vim > /dev/null 2>&1 || INSTALL_PKS="vim $INSTALL_PKS"
type ctags > /dev/null 2>&1 || INSTALL_PKS="ctags $INSTALL_PKS"
type ag > /dev/null 2>&1 || INSTALL_PKS="silversearcher-ag $INSTALL_PKS"
type colordiff > /dev/null 2>&1 || INSTALL_PKS="colordiff $INSTALL_PKS"

[ -n $INSTALL_PKS ] && sudo apt-get install $INSTALL_PKS
echo

_info "Installing fzf"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
echo

_info "Setting up vim"
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall
echo

_info "Setting up Sublime Text 3"
SUBLIME_DIR="/mnt/c/Users/hesaam/AppData/Roaming/Sublime Text 3"
if [ -d "$SUBLIME_DIR" ] ; then
	echo " - backing existing data folder"
	mv "$SUBLIME_DIR" "$BAK_DIR/"
fi
cp -R ".config/sublime-text-3" "$SUBLIME_DIR"
echo

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
mv "/mnt/c/Users/hesaam/AppData/Roaming/Code - Insiders/User/settings.json" "$BAK_DIR/code-insiders.settings.json"
cp ".config/Code - Insiders/User/settings.json" "/mnt/c/Users/hesaam/AppData/Roaming/Code - Insiders/User/settings.json"
