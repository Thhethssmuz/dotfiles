#!/usr/bin/env bash

_git_crypt() {
  local cur cword
  cur="${COMP_WORDS[COMP_CWORD]}"
  cword=1

  if [ "${COMP_WORDS[0]}" == "git" ]; then
    cword=$((COMP_CWORD - 1))
  fi

  case $cword in

    1)
      COMPREPLY=($(compgen -W "init status lock add-gpg-user unlock export-key" -- "$cur"))
      ;;

    2)
      case "${COMP_WORDS[cword]}" in

        init)
          case "$cur" in
            --*)
              COMPREPLY=($(compgen -W "--key-name" -- "$cur"))
              ;;
          esac
          ;;

        status)
          case "$cur" in
            --*)
              COMPREPLY=($(compgen -W "--fix" -- "$cur"))
              ;;
          esac
          ;;

        lock)
          case "$cur" in
            --*)
              COMPREPLY=($(compgen -W "--all --key-name --force" -- "$cur"))
              ;;
          esac
          ;;

        add-gpg-user)
          case "$cur" in
            --*)
              COMPREPLY=($(compgen -W "--key-name --no-commit --trusted" -- "$cur"))
              ;;
          esac
          ;;

        export-key)
          case "$cur" in
            --*)
              COMPREPLY=($(compgen -W "--key-name" -- "$cur"))
              ;;
          esac
          ;;

      esac
      ;;

    *)
      COMPREPLY=()
      ;;

  esac
}

complete -F _git_crypt git-crypt
