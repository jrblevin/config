function activate
    # Check if a project name is provided
    if test (count $argv) -eq 0
        echo "Please provide a project name"
        return 1
    end

    # Store the project name
    set -l project $argv[1]

    # Array of potential project paths to search
    set -l search_paths ~/git ~/git/*

    # Find the project directory
    set -l project_dir
    for path in $search_paths
        # Look for the project directory with .git and virtual env
        set -l potential_dir (find $path -maxdepth 3 -type d -name "$project" | while read -l dir
            if test -d "$dir/.git" -a \( -d "$dir/.venv" -o -d "$dir/venv" \)
                echo "$dir"
                break
            end
        end)
        
        if test -n "$potential_dir"
            set project_dir "$potential_dir"
            break
        end
    end

    # Check if project directory was found
    if test -z "$project_dir"
        echo "Project $project not found in search paths"
        return 1
    end

    # Change to the project directory
    cd "$project_dir"

    # Check for virtual environment
    if test -d .venv
        source .venv/bin/activate.fish
    else if test -d venv
        source venv/bin/activate.fish
    end

    # Rename tmux window
    tmux rename-window "$project"
end

# Much faster completion function - back to targeted search
function __fish_complete_activate
    set -l projects
    
    # First, check direct ~/git subdirectories (catches most projects)
    for dir in ~/git/*/
        if test -d "$dir/.git" -a \( -d "$dir/.venv" -o -d "$dir/venv" \)
            set -a projects (basename "$dir")
        end
    end
    
    # Then check one level deeper for nested projects (like jblevins.org/htdocs)
    for dir in ~/git/*/*/
        if test -d "$dir/.git" -a \( -d "$dir/.venv" -o -d "$dir/venv" \)
            set -a projects (basename "$dir")
        end
    end
    
    # Remove duplicates and sort
    printf "%s\n" $projects | sort -u
end

# Register the completion
complete -c activate -f -a "(__fish_complete_activate)"
