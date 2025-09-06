if status is-interactive
    # Commands to run in interactive sessions can go here
end

# path management
fish_add_path ~/.dotnet/tools
fish_add_path ~/.cargo/bin
fish_add_path /usr/local/go/bin
fish_add_path ~/go/bin

# aliases
# Replace docker with podman
alias docker='podman'
alias docker-compose='podman-compose'

function jshell-mvn
    set cp (mvn -q dependency:build-classpath -Dmdep.outputAbsoluteArtifactFilename=true)
    jshell --class-path $cp $argv
end

function tm
    if test (count $argv) -eq 0
        echo "Usage: tm <session-name>"
        return 1
    end

    set name $argv[1]

    if tmux has-session -t $name 2>/dev/null
        tmux attach -t $name
    else
        tmux new -s $name
    end
end

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

source ~/.cargo/env.fish
/home/karl/.local/bin/mise activate fish | source
starship init fish | source

set -x -U GOPATH $HOME/go

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/karl/.ghcup/bin # ghcup-env

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/karl/.opam/opam-init/init.fish' && source '/home/karl/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration
