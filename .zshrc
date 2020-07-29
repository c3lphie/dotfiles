#############
#  Exports  #
#############

# export ZSH="/home/manjaro/.oh-my-zsh" # <= Oh My Zsh install
export SUCKDIR="$HOME/.suckless" # <= Suckless repos
export MY_TOOLS="$HOME/.local/bin" # <= My Shell Scripts
export REPOS="$HOME/repositories" # <= Git repositories (Non-Suckless)

###########
#  THEME  #
###########
ZSH_THEME="dracula"

################
#  ZSH CONFIG  #
################
ENABLE_CORRECTION="true"
HIST_STAMPS="mm/dd/yyyy"

#####################
#  PLUGINS FOR ZSH  #
#####################
plugins=(git)

# source $ZSH/oh-my-zsh.sh
# source $ZSH/zsh-git-prompt.sh

#############
#  ALIASES  #
#############
alias ls='ls -AL --color'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'

alias suckless='cd $SUCKDIR'
alias scripts='cd $MY_TOOLS'
alias repos='cd $REPOS'
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
