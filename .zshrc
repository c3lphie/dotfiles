#############
#  Exports  #
#############
export GO_PATH="$HOME/go/bin"
export SUCKDIR="$HOME/.suckless" # <= Suckless repos
export MY_TOOLS="$HOME/.local/bin" # <= My Shell Scripts
export REPOS="$HOME/repositories" # <= Git repositories (Non-Suckless)
export BIBDIR="$HOME/Dropbox/org/bibliography/bibtex-pdfs"
export PATH="$MY_TOOLS:$MY_TOOLS/tools:$GO_PATH:$PATH"

################
#  ZSH CONFIG  #
################
ENABLE_CORRECTION="true"
HIST_STAMPS="mm/dd/yyyy"

# Syntax highlight
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#############
#  ALIASES  #
#############
alias ls='ls -AL --color'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias spt='spt -c ~/.config/spotify-tui/client.yml'

alias suckless='cd $SUCKDIR'
alias scripts='cd $MY_TOOLS'
alias repos='cd $REPOS'
alias bibs='cd $BIBDIR'
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias get_idf='. $HOME/esp/esp-idf/export.sh'

#############
# Functions #
#############

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

eval "$(starship init zsh)"
