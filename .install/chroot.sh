#!/bin/bash
set -exuo pipefail

DISK="$1"
HOSTNAME="$2"

# generate locales
cat <<EOF > /etc/locale.gen
en_GB.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF

locale-gen

# ctl
systemd-firstboot \
  --timezone=Europe/Oslo \
  --locale=en_GB.UTF-8 \
  --locale-messages=en_GB.UTF-8 \
  --hostname="$HOSTNAME"

hwclock --systohc --utc

echo "KEYMAP=dvorak" > /etc/vconsole.conf

# add mkinitcpio hooks
if ! grep '^HOOKS=.*encrypt' /etc/mkinitcpio.conf; then
  sed -i '/^HOOKS=/ s/filesystem/keymap\ encrypt\ lvm2\ resume\ filesystem/' /etc/mkinitcpio.conf
fi
pacman -S --noconfirm linux

# configure systemd boot
bootctl --path=/boot install

# LVM on LUKS config
cat <<EOF > /boot/loader/entries/lvmluks.conf
title Arch Linux
linux /vmlinuz-linux
initrd /initramfs-linux.img
options cryptdevice=${DISK}2:vgroup resume=/dev/mapper/vgroup-swap root=/dev/mapper/vgroup-root quiet rw
EOF

# bootloader conf
cat <<EOF > /boot/loader/loader.conf
timeout 5
default lvmluks
EOF

exit
