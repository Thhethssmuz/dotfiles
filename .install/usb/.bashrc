#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias .........='cd ../../../../../../../..'
alias ..........='cd ../../../../../../../../..'
alias ...........='cd ../../../../../../../../../..'
alias ............='cd ../../../../../../../../../../..'
alias .............='cd ../../../../../../../../../../../..'
alias l='ls -1 --group-directories-first'
alias la='ls -1A --group-directories-first'
alias ll='ls -Ahl --group-directories-first'
alias ls='ls --color=auto --group-directories-first'
alias tree='tree -C --dirsfirst'
alias grep='grep --color=auto'
alias diff='diff -Naur --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias pacman='pacman --color=auto'

# Enable bash completion
[ -r /usr/share/bash-completion/bash_completion ] && \
  source /usr/share/bash-completion/bash_completion
