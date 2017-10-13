# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/yangruihan/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="bira"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export PATH=${PATH}:$HOME/WorkSpace/Android/sdk/platform-tools:$HOME/WorkSpace/Android/sdk/tools

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
# export PYENV_VIRTUALENV_DISABLE_PROMPT=1

alias gitp="sh ~/WorkSpace/Scripts/git_add_commit_push.sh"

alias p35="pyenv activate pyenv_3_5_2"
alias p27="pyenv activate pyenv_2_7_12"
alias p277="pyenv activate pyenv_2_7_7"

alias gitpla="p35 && python ~/Workspace/Scripts/all_git_pull.py"

alias update="sh ~/WorkSpace/Scripts/update.sh"

alias sshu="ssh ubuntu@119.29.167.139"

alias emacs="open -a /Applications/Emacs.app"

alias szsh="source ~/.zshrc"

vcode () {
   if [[ $# = 0 ]]
   then
       open -a "Visual Studio Code"
   else
       [[ $1 = /* ]] && F="$1" || F="$PWD/${1#./}"
       open -a "Visual Studio Code" --args "$F"
   fi
}

alias fuck-it='export THEFUCK_REQUIRE_CONFIRMATION=False; fuck; export THEFUCK_REQUIRE_CONFIRMATION=True'


eval $(thefuck --alias)

alias py="python"

alias rn="react-native"

alias show_file="defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder"
alias hide_file="defaults write com.apple.finder AppleShowAllFiles -boolean false ; killall Finder"

alias unity="/Applications/Unity5.6.3/Unity.app/Contents/MacOS/Unity -projectPath"
alias unity2017="/Applications/Unity2017/Unity.app/Contents/MacOS/Unity -projectPath"

alias finder="open -a Finder ./"
alias cls="clear"
