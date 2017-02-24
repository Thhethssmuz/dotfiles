#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ -f ~/.bash_profile ] && source ~/.bash_profile

# Enable bash completion
[ -r /usr/share/bash-completion/bash_completion ] && \
  source /usr/share/bash-completion/bash_completion

# Extra custom bash completions
[ -r ~/.bash-completion ] &&
  source ~/.bash-completion/*
