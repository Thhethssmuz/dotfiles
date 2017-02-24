#!/usr/bin/env bash

_key() {
  local cur opts
  cur="${COMP_WORDS[COMP_CWORD]}"
  opts="$(find ~/.ssh -name "*_id" | sed 's/^.*\.ssh\/\(.*\)_id/\1/')"
  COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
  return 0
}

complete -F _key key
