#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'

PS1='[\u@\h \W]\$ '
if [ -f "$HOME/.bash_ps1" ]; then

. "$HOME/.bash_ps1"

fi

if [ -f $HOME/.thhe/nice.sh ] > /dev/null; then

  $HOME/.thhe/nice.sh

fi

torr () {
  rsync -chzP -e ssh "$1" broxy:/home/bro/dumptruck/DL/.torr/
}

export PATH=$PATH:$HOME/.cabal/bin:$HOME/.thhe

export CXX=clang++
export CC=clang

export EDITOR=vim
export VISUAL=vim
