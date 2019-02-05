#!/bin/bash
set -exuo pipefail

# don't automatically re-execute if something goes wrong
sed -i 's/bash live.sh//' ~/.bashrc

# partition disk
sgdisk \
  -n 1:0:+128M -t 1:ef00 -c 1:bootefi \
  -n 2:0:0 -t 2:8300 -c 2:linux \
  -p "$DISC"

# create filesystems
mkfs.fat -F32 "$DISC1"
mkfs.ext4 "$DISC2"

# mount filesystems
mkdir -p /mnt
mount "$DISC2" /mnt
mkdir -p /mnt/boot
mount "$DISC1" /mnt/boot

# bootstrap
pacstrap /mnt base base-devel
genfstab -U -p /mnt >> /mnt/etc/fstab

# copy scripts
cp ./chroot.sh /mnt/root
cp ./firstboot.sh /mnt/root

# chroot
arch-chroot /mnt /root/chroot.sh

# remove chroot script
rm /mnt/root/chroot.sh

# clean up and reboot
umount -R /mnt
reboot
