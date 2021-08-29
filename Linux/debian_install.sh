#!/usr/bin/env bash

#----------------------
# function
#----------------------
function LOG() {
	echo "[#Install Log] $1"
}

function ERROR() {
	echo "[#Install Error] $1"
}

#----------------------
# apt install
#----------------------
# sudo apt update
LOG "apt install ..."
sudo apt install git vim zsh curl wget emacs tmux tldr python python3 python3-pip -y
LOG "apt install done"

#----------------------
# install oh my zsh
#----------------------
LOG "install zsh && oh-my-zsh ..."
if [ -d "$HOME/.oh-my-zsh" ]; then
	LOG "oh my zsh already exists, reinstall it"
	rm -rf "$HOME/.oh-my-zsh" 
fi

if [ -f "$HOME/.zshrc" ]; then
	LOG ".zshrc already exists, back up to .zshrc.bac"
	mv "$HOME/.zshrc" "$HOME/.zshrc.bac"
fi

printf "y" | sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# change zsh theme
sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="bira"/' ~/.zshrc
chsh -s $(which zsh)
LOG "install zsh && oh-my-zsh done"

# zsh plugin
LOG "install zsh plugin ..."

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
sed -i 's/plugins=(/plugins=( zsh-autosuggestions /' ~/.zshrc

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
sed -i 's/plugins=(/plugins=( zsh-syntax-highlighting /' ~/.zshrc

LOG "install zsh plugin done"

#----------------------
# git config
#----------------------
git config --global alias.st status
git config --global alias.co checkout
git config --global alias.ci commit
git config --global alias.br branch

#----------------------
# custom script folder
#----------------------
LOG "create script folder ..."
if [ ! -d "$HOME/.rscripts" ]; then
	mkdir "$HOME/.rscripts"
fi
echo export PATH='$PATH:$HOME/.rscripts' >> ~/.zshrc
LOG "create script folder done"

#----------------------
# tldr update
#----------------------
tldr --update

#----------------------
# emacs config
#----------------------
LOG "config emacs ..."

wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/__emacs2 -O ~/.emacs
if [ ! -d "$HOME/.emacs.d" ]; then
	mkdir "$HOME/.emacs.d"
fi
wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/init9.el -O ~/.emacs.d/init.el
echo alias emsd=\'emacs --daemon\' >> ~/.zshrc
echo alias emsdq=\'emacsclient --eval \"\(kill-emacs\)\"\' >> ~/.zshrc
echo alias emc=\'emacsclient -t -a \"\"\' >> ~/.zshrc

wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/em.py -O ~/.rscripts/em.py
echo alias em=\'~/.rscripts/em.py\' >> ~/.zshrc

LOG "config emacs done"

#----------------------
# tmux config
#----------------------
LOG "config tmux ..."

if [ -f "$HOME/.tmux.conf" ]; then
	LOG "tmux conf already exists, make a backup to .tmux.conf.bac"
	mv "$HOME/.tmux.conf" "$HOME/.tmux.conf.bac"
fi

touch "$HOME/.tmux.conf"
echo 'set -g default-terminal "screen-256color"' >> "$HOME/.tmux.conf"
echo 'unbind C-b' >> "$HOME/.tmux.conf"
echo set -g prefix \'C-\\\' >> "$HOME/.tmux.conf"

echo alias tmux=\"tmux -u\" >> "$HOME/.zshrc"

LOG "config tmux done"

LOG "-------------------------------"
LOG "------- Install Finish --------"
LOG "-------------------------------"

#----------------------
# change to zsh
#----------------------
zsh
