# Initialize completion
autoload -Uz compinit && compinit

# Set automatic cd (typing directory name with no 'cd')
setopt autocd

# Nicer history
export HISTSIZE=16384
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=$HISTSIZE

# Use vim as the editor
export EDITOR=vim

# Use vim style line editing in zsh
bindkey -v

# Use incremental search
bindkey "^R" history-incremental-search-backward
