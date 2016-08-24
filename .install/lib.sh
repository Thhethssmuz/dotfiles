#!/bin/bash
set -euo pipefail


#
# Helpers
#

filter() {
  while read -r line; do
    if ! "${line##*.}-check" "${line%.*}"; then
      echo "$line"
    fi
  done
}

cfg-path() {
  head -n1 "templates/$1.tmpl" | sed 's/^#\s*//'
}
render-tmpl() {
  eval "$(echo -e "cat <<EOF\n$(tail -n+2 "templates/$1.tmpl")\nEOF")"
}


#
# Check rules
#

pac-check() {
  pacman -Qn "$1" >/dev/null 2>&1
}
aur-check() {
  pacman -Qm "$1" >/dev/null 2>&1
}
npm-check() {
  [ -d "/usr/lib/node_modules/$1" ]
}
ini-check() {
  [ -e "$(cfg-path "$1")" ]
}
cfg-check() {
  ini-check "$1" && render-tmpl "$1" | diff "$(cfg-path "$1")" - >/dev/null 2>&1
}
srv-check() {
  systemctl is-enabled "$1.service" >/dev/null 2>&1
}
usr-check() {
  sudo -u "$USERNAME" -i SERVICE="$1" bash <<- 'EOF'
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"
    systemctl --user is-enabled $SERVICE.service >/dev/null 2>&1
EOF
}


#
# Install rules
#

pac-install() {
  pacman -S --noconfirm --needed "$@"
}
aur-install() {
  apacman -S --noconfirm --needed "$@"
}
npm-install() {
  npm install -g "$@"
}
ini-install() {
  for TARGET in "$@"; do
    render-tmpl "$TARGET" > "$(cfg-path "$TARGET")"
  done
}
cfg-install() {
  ini-install "$@"
}
srv-install() {
  systemctl enable "${*/%/.service}"
}
usr-install() {
  sudo -u "$USERNAME" -i SERVICES="${*/%/.service}" bash <<- 'EOF'
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"
    systemctl --user enable $SERVICES
EOF
}


#
# Check-update rules
#

pac-check-updates() {
  checkupdates
}
aur-check-updates() {
  pacman -Qm | while read -r line; do
    PKG="$(echo "$line" | awk '{print $1}')"
    VER="$(echo "$line" | awk '{print $2}')"
    UP="$(apacman -Si "$PKG" 2>/dev/null | grep Version | awk '{print $3}')"

    if [ "$VER" != "$UP" ]; then
      echo "$PKG $VER -> $UP"
    fi
  done
}
npm-check-updates() {
  npm outdated -g --depth=0 2>/dev/null | tail -n+2 | while read -r line; do
    PKG="$(echo "$line" | awk '{print $1}')"
    VER="$(echo "$line" | awk '{print $2}')"
    UP="$(echo "$line" | awk '{print $4}')"
    echo "$PKG $VER -> $UP"
  done
}


#
# Update rules
#

pac-update() {
  pacman -Syu
}
aur-update() {
  apacman -Syu --auronly
}
npm-update() {
  PKGS=($(npm outdated -g --depth=0 --parseable 2>/dev/null | cut -d: -f4))
  npm install -g "${PKGS[*]}"
}


#
# Main rules
#

status() {
  while read -r line; do
    if "${line##*.}-check" "${line%.*}"; then
      echo -e "\e[1m$line\e[0m \e[1;32minstalled\e[0m"
    else
      echo -e "\e[1m$line\e[0m \e[1;31mnot installed\e[0m"
    fi
  done
}

install() {
  TARGETS=$(filter)

  for TYPE in pac aur npm ini cfg srv usr; do
    if echo "$TARGETS" | egrep "\.$TYPE\$" >/dev/null 2>&1; then
      TARGETS_OF_TYPE=($(echo "$TARGETS" | egrep "\.$TYPE\$" | sed 's/\..*$//'))
      if [ "${#TARGETS_OF_TYPE[*]}" ]; then

        for TARGET in ${TARGETS_OF_TYPE[*]}; do
          if [ -e "hooks/$TARGET.$TYPE.pre" ]; then
            echo "running pre-install hook for $TARGET.$TYPE"
          fi
        done

        # shellcheck disable=SC2086
        "$TYPE-install" ${TARGETS_OF_TYPE[*]}

        for TARGET in ${TARGETS_OF_TYPE[*]}; do
          if [ -e "hooks/$TARGET.$TYPE.post" ]; then
            echo "running post-install hook for $TARGET.$TYPE"
          fi
        done

      fi
    fi
  done
}

check-updates() {
  for TYPE in pac aur npm; do
    "$TYPE-check-updates" | sed "s/^/$TYPE /"
  done
}

update() {
  for TYPE in pac aur npm; do
    "$TYPE-updates"
  done
}
