#!/bin/bash
set -euo pipefail

declare -A DCONF_SETTINGS=(\
  ["/org/gtk/settings/file-chooser/sort-directories-first"]="true" \
  ["/org/gtk/settings/file-chooser/show-hidden"]="true" \
  )

order() {
  echo "post-pac"
}

install() {
  if ! hash dconf 2>/dev/null; then
    return;
  fi
  for key in "${!DCONF_SETTINGS[@]}"; do
    dconf write "$key" "${DCONF_SETTINGS[$key]}"
  done
}

remove() {
  :
}

status() {
  local actual
  for key in "${!DCONF_SETTINGS[@]}"; do
    if ! hash dconf 2>/dev/null; then
      echo "dconf.src/install/error/${key}"
      continue
    fi

    actual="$(dconf read "$key")"
    if [ "$actual" != "${DCONF_SETTINGS[$key]}" ]; then
      echo "dconf.src/install/error/${key}"
    fi
  done
}

"$@"
