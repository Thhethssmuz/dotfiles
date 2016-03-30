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
