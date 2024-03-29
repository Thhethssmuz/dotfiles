#!/bin/bash
set -exuo pipefail

PARTITION2="$1"
CPU_PROFILE="$2"
HOSTNAME="$3"

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

echo "Set root password"
passwd

# install network tools useful for the new environment, can be removed later if
# not required
pacman -S --noconfirm iwd inetutils dhcpcd

# install lvm2 before we run mkinitcpio
pacman -S --noconfirm lvm2

# add mkinitcpio hooks
if ! grep '^HOOKS=.*encrypt' /etc/mkinitcpio.conf; then
  sed -i '/^HOOKS=/ s/filesystem/keymap\ encrypt\ lvm2\ resume\ filesystem/' /etc/mkinitcpio.conf
fi
mkinitcpio -P

if [ "$CPU_PROFILE" = "intel" ]; then
  pacman -S intel-ucode
elif [ "$CPU_PROFILE" = "amd" ]; then
  pacman -S amd-ucode
fi

# configure systemd boot
bootctl --path=/boot install

# LVM on LUKS config
cat <<EOF > /boot/loader/entries/lvmluks.conf
title Arch Linux
linux /vmlinuz-linux
$([ "$CPU_PROFILE" != "intel" ] && echo '#' || echo '')initrd /intel-ucode.img
$([ "$CPU_PROFILE" != "amd" ] && echo '#' || echo '')initrd /amd-ucode.img
initrd /initramfs-linux.img
options cryptdevice=${PARTITION2}:porn resume=/dev/mapper/porn-swap root=/dev/mapper/porn-root quiet rw
EOF

# bootloader conf
cat <<EOF > /boot/loader/loader.conf
timeout 5
default lvmluks
EOF

# leave chroot
exit
