# some commands need to be executed automatically

# create or attach tmux  
if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
    tmux attach -t default || tmux new -s default
fi
