#
# ~/.bashrc
#


# If not running interactively, don't do anything
[[ $- != *i* ]] && return


# Import functions
. ~/.scripts/*


# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'


# Prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[1;30m\]\u@\h\[\033[00m\] \[\033[1;34m\]\w\[\033[00m\] \[\033[1;34m\]\$\[\033[00m\] '
trap 'echo -ne "\e[0m"' DEBUG


# Exports
export PATH=$PATH:$HOME/.cabal/bin:$HOME/.thhe

export CXX=clang++
export CC=clang

export EDITOR=vim
export VISUAL=vim
