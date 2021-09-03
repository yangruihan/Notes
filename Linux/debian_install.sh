#!/usr/bin/env bash

#-------------------------------------------------------------------------------
# function
#-------------------------------------------------------------------------------

function LOG() {
    echo "[#Install Log] $1"
}

function ERROR() {
    echo "[#Install Error] $1"
}

#-------------------------------------------------------------------------------
# get github release latest version number
# return value save to release_ver
# for example:
#    getGithubLatestReleaseVersion "dandavison/delta" # release_ver=="v0.8.3"
# code from https://gist.github.com/lukechilds/a83e1d7127b78fef38c2914c4ececc3c
#-------------------------------------------------------------------------------
function getGithubLatestReleaseVersion() {
    release_ver=`curl --silent "https://api.github.com/repos/$1/releases/latest" | grep -Po '"tag_name": "\K.*?(?=")'`
}

#-------------------------------------------------------------------------------
# get system arch
# return value save to sysarch
#-------------------------------------------------------------------------------
function getSysArch() {
    sysarch=`uname -m`
}

# get system arch
getSysArch

#-------------------------------------------------------------------------------
# apt install
#-------------------------------------------------------------------------------
sudo apt update
LOG "apt install ..."
sudo apt install \
     git vim zsh curl wget \
     emacs tmux tldr python \
     python3 python3-pip \
     man gdb build-essential net-tools -y
LOG "apt install done"

#-------------------------------------------------------------------------------
# install oh my zsh
#-------------------------------------------------------------------------------
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
sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="bira"/' "$HOME/.zshrc"
chsh -s $(which zsh)
LOG "install zsh && oh-my-zsh done"

# zsh plugin
LOG "install zsh plugin ..."

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
sed -i 's/plugins=(/plugins=( zsh-autosuggestions /' "$HOME/.zshrc"

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
sed -i 's/plugins=(/plugins=( zsh-syntax-highlighting /' "$HOME/.zshrc"

LOG "install zsh plugin done"

#-------------------------------------------------------------------------------
# git config
#-------------------------------------------------------------------------------
if [ -f "$HOME/.gitconfig" ]; then
    LOG "git config file exists, back up to .gitconfig.bac"
    mv "$HOME/.gitconfig" "$HOME/.gitconfig.bac"
fi

git config --global alias.st status
git config --global alias.co checkout
git config --global alias.ci commit
git config --global alias.br branch

#-------------------------------------------------------------------------------
# custom script folder
#-------------------------------------------------------------------------------
LOG "create script folder ..."
if [ ! -d "$HOME/.rscripts" ]; then
    mkdir "$HOME/.rscripts"
fi
echo export PATH='$PATH:$HOME/.rscripts' >> "$HOME/.zshrc"
LOG "create script folder done"

#-------------------------------------------------------------------------------
# tldr update
#-------------------------------------------------------------------------------
tldr --update

#-------------------------------------------------------------------------------
# emacs config
#-------------------------------------------------------------------------------
LOG "config emacs ..."

wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/__emacs2 -O ~/.emacs
if [ ! -d "$HOME/.emacs.d" ]; then
    mkdir "$HOME/.emacs.d"
fi
wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/init9.el -O ~/.emacs.d/init.el
echo alias emsd=\'emacs --daemon\' >> "$HOME/.zshrc"
echo alias emsdq=\'emacsclient --eval \"\(kill-emacs\)\"\' >> "$HOME/.zshrc"
echo alias emc=\'emacsclient -t -a \"\"\' >> "$HOME/.zshrc"

wget https://raw.githubusercontent.com/yangruihan/Notes/master/Emacs/em.py -O ~/.rscripts/em.py
chmod +x ~/.rscripts/em.py 
echo alias em=\'~/.rscripts/em.py\' >> "$HOME/.zshrc"

LOG "config emacs done"

#-------------------------------------------------------------------------------
# tmux config
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# install delta
#-------------------------------------------------------------------------------
LOG "install delta ..."

getGithubLatestReleaseVersion "dandavison/delta"
delta_version=$release_ver
delta_arch="amd64"
if [ "$sysarch" = "x86_64" ]; then
    delta_arch="amd64"
else
    delta_arch="armhf"
fi
LOG "delta version $delta_version"
LOG "delta arch $delta_arch"
wget "https://github.com/dandavison/delta/releases/download/$delta_version/git-delta_${delta_version}_${delta_arch}.deb" -O delta.deb
sudo dpkg -i delta.deb
rm delta.deb

read -r -d '' delta_config << EOM
[pager]
    diff = delta
    log = delta
    reflog = delta
    show = delta
[delta]
    features = side-by-side line-numbers decorations
    syntax-theme = Dracula
    plus-style = syntax "#003800"
    minus-style = syntax "#3f0001"
[delta "decorations"]
    commit-decoration-style = bold yellow box ul
    file-style = bold yellow ul
    file-decoration-style = none
    hunk-header-decoration-style = cyan box ul
[delta "line-numbers"]
    line-numbers-left-style = cyan
    line-numbers-right-style = cyan
    line-numbers-minus-style = 124
    line-numbers-plus-style = 28
[interactive]
    diffFilter = delta --color-only
EOM

echo "$delta_config" >> "$HOME/.gitconfig"

LOG "install delta done"

#-------------------------------------------------------------------------------
# install fd
#-------------------------------------------------------------------------------
LOG "install fd ..."
sudo apt install fd-find -y
echo alias fd=\"fd-find\" >> "$HOME/.zshrc"
LOG "install fd done"

#-------------------------------------------------------------------------------
# install ripgrep
#-------------------------------------------------------------------------------
LOG "install ripgrep ..."
sudo apt install ripgrep -y
LOG "install ripgrep done"

#-------------------------------------------------------------------------------
# install fzf
#-------------------------------------------------------------------------------
LOG "install fzf ..."
sudo apt install fzf -y
LOG "install fzf done"

#-------------------------------------------------------------------------------
# install mcfly
#-------------------------------------------------------------------------------
LOG "install mcfly ..."
getGithubLatestReleaseVersion "cantino/mcfly"
mcfly_version=$release_ver
mcfly_arch="x86_64-unknown-linux-musl"
if [ "$sysarch" = "x86_64" ]; then
    mcfly_arch="x86_64-unknown-linux-musl"
else
    mcfly_arch="armv7-unknown-linux-gnueabihf"
fi
LOG "mcfly version $mcfly_version"
LOG "mcfly arch $mcfly_arch"
wget "https://github.com/cantino/mcfly/releases/download/$mcfly_version/mcfly-${mcfly_version}-${mcfly_arch}.tar.gz" -O mcfly.tar.gz
tar xvf mcfly.tar.gz
mv mcfly "$HOME/.rscripts/"
rm mcfly.tar.gz
echo 'eval "$(mcfly init zsh)"' >> "$HOME/.zshrc"
LOG "install mcfly done"

#-------------------------------------------------------------------------------
# install autojump
#-------------------------------------------------------------------------------
LOG "install autojump ..."
sudo apt install autojump -y
echo ". /usr/share/autojump/autojump.sh" >> "$HOME/.zshrc"
LOG "install autojump"
LOG "-------------------------------"
LOG "------- Install Finish --------"
LOG "-------------------------------"
#-------------------------------------------------------------------------------
# change to zsh
#-------------------------------------------------------------------------------
if [ ! -n "$ZSH_VERSION" ]; then
    LOG "change to zsh"
    zsh
else
    source "$HOME/.zshrc"
fi
