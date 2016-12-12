#!/bin/bash

# 更新源
sudo rm /etc/apt/sources.list
sudo cp sources.list /etc/apt/sources.list
sudo apt update

# 安装 vim git
sudo apt install vim git -y

# 配置 vim
git clone https://github.com/wklken/k-vim.git ~/.k-vim
sudo apt install ctags -y
sudo apt install build-essential cmake python-dev -y
sudo apt install silversearcher-ag -y
sudo pip install pyflakes
sudo pip install pylint
sudo pip install pep8
cd ~/.k-vim
sh -x install.sh

# 安装 zsh
sudo apt install zsh -y
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# 安装 pyenv
git clone https://github.com/yyuu/pyenv.git ~/.pyenv
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.zshenv
echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.zshenv
echo 'eval "$(pyenv init -)"' >> ~/.zshenv

git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.zshenv

exec $SHELL

sudo apt install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils -y
sudo apt install python-pip python-dev build-essential -y
sudo apt install python-dev python3-dev -y

# 打开 ssh 服务
sudo apt install openssh-client openssh-server -y
sudo /etc/init.d/ssh start

# 安装 java
sudo apt install python-software-properties -y
sudo add-apt-repository ppa:webupd8team/java
sudo apt update
sudo apt install oracle-java8-installer -y

echo 'export JAVA_HOME="/usr/lib/jvm/java-8-oracle/jre/bin"' >> ~/.zshenv
exec $SHELL

# 安装 tmux
sudo apt install tmux