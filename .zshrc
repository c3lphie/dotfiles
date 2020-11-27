#############
#  Exports  #
#############
export ZSH="/home/c3lphie/.oh-my-zsh" # <= Oh My Zsh install
export SUCKDIR="$HOME/.suckless" # <= Suckless repos
export MY_TOOLS="$HOME/.local/bin" # <= My Shell Scripts
export REPOS="$HOME/repositories" # <= Git repositories (Non-Suckless)
export PATH="$MY_TOOLS:$GO_PATH:$PATH"

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

source $ZSH/oh-my-zsh.sh
#source $ZSH/zsh-git-prompt.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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

#############
# Functions #
#############
gdb ()
{
    tmux has-session -t gdb_sess &> /dev/null
    if [ $? -eq 0 ]; then
        tmux kill-session -t gdb_sess
    fi

    tmux new-session -d -s gdb_sess "/usr/bin/gdb $1"
    tmux attach -t gdb_sess
}

###########
#   nnn   #
###########
# Quitcd
n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}
