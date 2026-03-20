fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/sbin
fish_add_path ~/bin
fish_add_path ~/.local/bin

starship init fish | source

# Set default editor
set -x EDITOR $HOME/bin/EDITOR

# Added by LM Studio CLI (lms)
set -gx PATH $PATH $HOME/.cache/lm-studio/bin
