#!/bin/bash
set -euo pipefail

if [[ $EUID -ne 0 ]]; then
  echo "You must be root inside live environment" 1>&2
  exit 1
fi

loadkeys dvorak
dhcpcd

source live-prompt.sh
ping google.com -c 1 || source ip-prompt.sh

echo -e "\e[31mWARNING: this will overwrite the disk ${DISK}\e[0m"
echo -e "\e[31mThis is a $(lsblk "$DISK" -ldn | awk '{print $4}') disk\e[0m"
echo -e "\e[31mlast chance are you sure you want to do this?\e[0m"
read -rp "Press enter to continue"

# in case something goes wrong
set -x

# partition disk
sgdisk -Z "$DISK"
sgdisk \
  -n 1:0:+512M -t 1:ef00 -c 1:bootefi \
  -n 2:0:0 -t 2:8300 -c 2:cryptlvm \
  -p "$DISK"

# encrypt main partition
cryptsetup -v --cipher aes-xts-plain64 --key-size 512 --hash sha512 \
  --use-random luksFormat "${DISK}2"
cryptsetup luksOpen "${DISK}2" lvm

# create physical volume on top of luks
pvcreate /dev/mapper/lvm

# create volume group from physical volume
vgcreate vgroup /dev/mapper/lvm

# create logical partitions on the volume group
if [ -n "$SWAP" ]; then
  lvcreate -L "$SWAP" -n swap vgroup
fi
lvcreate -l 100%free -n root vgroup

# create filesystems
mkfs.fat -F32 "${DISK}1"
mkfs.ext4 /dev/mapper/vgroup-root
if [ -n "$SWAP" ]; then
  mkswap /dev/mapper/vgroup-swap
fi

# mount filesystems
mkdir -p /mnt/arch
mount /dev/mapper/vgroup-root /mnt/arch
mkdir -p /mnt/arch/boot
mount "${DISK}1" /mnt/arch/boot
if [ -n "$SWAP" ]; then
  swapon /dev/mapper/vgroup-swap
fi

# mount encrypted partition on usb
cryptsetup luksOpen "$(blkid -U "$(cat crypton.uuid)")" crypton
mkdir -p /mnt/crypton
mount /dev/mapper/crypton /mnt/crypton

# bootstrap
pacstrap /mnt/arch base base-devel
genfstab -U -p /mnt/arch >> /mnt/arch/etc/fstab

# copy
mkdir -p /mnt/arch/root/crypton
cp -r /mnt/crypton/{.gnupg,.ssh,.password-store} /mnt/arch/root/crypton
cp ./*.sh /mnt/arch/root

# chroot
arch-chroot /mnt/arch /root/chroot.sh "$DISK" "$HOSTNAME"

# clean up
umount -R /mnt/arch
umount /mnt/crypton
if [ -n "$SWAP" ]; then
  swapoff /dev/mapper/vgroup-swap
fi
vgchange -a n vgroup
cryptsetup close lvm
cryptsetup close crypton

# the freshly installed arch should now have priority booting with efi so it
# should be safe to just reboot even if the usb is still plugged in
reboot
