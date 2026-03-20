function gcan --wraps='git commit -v --amend --no-edit' --description 'alias gcan git commit -v --amend --no-edit'
  git commit -v --amend --no-edit $argv
        
end
