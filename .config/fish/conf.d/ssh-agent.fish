# SSH Agent
setenv SSH_ENV "$HOME/.ssh-agent.fish.$hostname"

function start_agent
    echo -n "Initializing SSH agent..."
    ssh-agent -c | sed 's/^echo/#echo/' > "$SSH_ENV"
    echo " OK"
    chmod 600 "$SSH_ENV"
    source "$SSH_ENV" > /dev/null
    /usr/bin/ssh-add --apple-use-keychain
end

# Source SSH settings
if test -f "$SSH_ENV"
    source "$SSH_ENV" > /dev/null
    or begin
        ps -x | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
        or start_agent
    end
else
    start_agent
end
