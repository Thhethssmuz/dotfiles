#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# shellcheck disable=SC1090
[ -f ~/.bash_profile ] && source ~/.bash_profile

# Enable bash completion
[ -r /usr/share/bash-completion/bash_completion ] && \
  source /usr/share/bash-completion/bash_completion

# Extra custom bash completions
for file in ~/.bash-completion/*; do
  # shellcheck disable=SC1090
  source "$file"
done
