#!/bin/bash

# prompt for install configuration
. ~/.install/iso/prompt-profile.sh || exit 1

# Create temporary build directory
TMPDIR="$(mktemp -d /tmp/archlive.XXXXXXXX)"
ISODIR="$(mktemp -d /tmp/archiso.XXXXXXXX)"

# copy the base archiso
cp -r /usr/share/archiso/configs/releng/* "$TMPDIR"

# create folder for personal install scripts
mkdir -p "$TMPDIR"/airootfs/etc/skel

# copy personal install scripts to the live image
case "$PROFILE" in
  usb)     cp ~/.install/usb/* "$TMPDIR"/airootfs/etc/skel/ ;;
  vm[kbw]) cp ~/.install/vm/* "$TMPDIR"/airootfs/etc/skel/  ;;
  *)       echo "unrecognised profile \`$PROFILE'"; exit 1 ;;
esac

# inject install script variables, for profiles with auto-install
case "$PROFILE" in
  usb) ;;
  vm[kbw])
    for FILE in "$TMPDIR"/airootfs/etc/skel/{live,chroot,firstboot}.sh; do
      sed -i "s|\$DISC|$DISC|g" "$FILE"
      sed -i "s|\$HOSTNAME|$HOSTNAME|g" "$FILE"
      sed -i "s|\$USERNAME|$USERNAME|g" "$FILE"
      sed -i "s|\$PASSWORD|$PASSWORD|g" "$FILE"
    done
    ;;
  *)
    echo "unrecognised profile \`$PROFILE'"
    exit 1 ;;
esac

# set the live shell to bash not zsh
# sed -i 's/zsh/bash/' "$TMPDIR"/airootfs/root/customize_airootfs.sh
sed -i 's/zsh/bash/' "$TMPDIR"/airootfs/etc/passwd

# automatically source the live script when starting up the live image
# echo 'echo "bash live.sh" >> /root/.bashrc' \
#   >> "$TMPDIR"/airootfs/root/customize_airootfs.sh

# build the image
sudo mkarchiso -v -w "$ISODIR" -o . "$TMPDIR"

# copy out and clean up
sudo rm -rf "$TMPDIR"

if findmnt | grep "$ISODIR"; then
  echo "mounts still present in build directory!"
  exit 1
else
  sudo rm -rf "$ISODIR"
fi
