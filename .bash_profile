#!/bin/bash

source ~/.prompt
source ~/.exports
source ~/.aliases
source ~/.scripts/*

# history
export HISTCONTROL=ignoreboth:erasedupes
export HISTSIZE=5000 # number of entries in memory
export HISTFILESIZE=5000 # number of lines in hist file
shopt -s histappend # Append to history file, rather than overwriting it

# keychain
# key() {
#   local keys=""
#   for k in "$@"; do
#     keys="$keys $HOME/.ssh/${k}_id"
#   done
#   keychain --timeout 5 --quiet --host agent $keys
#   source ~/.keychain/agent-sh
# }

# if [ "$(hostname)" = "ealbrigt-ws" ]; then
#   key sqbu work
# elif [ "$(hostname)" = "kjttks" ]; then
#   key github main
# else
#   [ -f ~/.keychain/agent-sh ] && source ~/.keychain/agent-sh
# fi
