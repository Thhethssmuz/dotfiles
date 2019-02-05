#!/bin/bash
set -exuo pipefail

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

pacman -S --noconfirm linux

# configure systemd boot
bootctl --path=/boot install

# Standard root config
cat <<EOF > /boot/loader/entries/arch.conf
title Arch Linux
linux /vmlinuz-linux
initrd /initramfs-linux.img
options root=$(blkid --match-tag PARTUUID "$DISC2" | awk '{print $2}' | sed 's/"//g') rw
EOF

# bootloader conf
cat <<EOF > /boot/loader/loader.conf
timeout 5
default arch
EOF

# enable autologin for first boot
mkdir -p /etc/systemd/system/getty\@tty1.service.d
cat <<EOF > /etc/systemd/system/getty@tty1.service.d/autologin.conf
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin root --noclear %I 38400 linux
EOF
systemctl enable getty\@tty1.service.d
echo 'bash /root/firstboot.sh' > /root/.profile

# leave chroot
exit
