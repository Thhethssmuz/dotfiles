#!/bin/bash

for file in ~/.{prompt,exports,aliases,functions,k8s-helpers}; do
  source "$file"
  [ -f "$file.$HOSTNAME" ] && source "$file.$HOSTNAME"
done

# history
export HISTCONTROL=ignoreboth:erasedupes
export HISTSIZE=5000 # number of entries in memory
export HISTFILESIZE=5000 # number of lines in hist file
shopt -s histappend # Append to history file, rather than overwriting it

# set tabsize
tabs -2
