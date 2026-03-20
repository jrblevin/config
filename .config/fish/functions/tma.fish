function tma --wraps='tmux -CC attach' --description 'alias tmux -CC [attach]'
    # Check if tmux is running and has active sessions
    if tmux list-sessions 2>/dev/null
        # If sessions exist, attach to the most recent session
        tmux -CC attach
    else
        # If no sessions exist, start a new tmux session
        tmux -CC
    end
end

