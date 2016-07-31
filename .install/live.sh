#!/bin/bash
set -exuo pipefail

abort() {
  echo "$1" 1>&2
  exit 1
}
[[ $EUID -eq 0 ]] || abort "You must be root inside live environment"
[[ $DISK ]] || abort "You must specify the disk to partition"
[[ $HOSTNAME ]] || abort "You must specify a hostname"

echo "WARNING: this will overwrite the disk $DISK"
echo "This is a $(lsblk "$DISK" -ldn | awk '{print $4}') disk"
read -rp "Press enter to continue or ctrl+c to cancel: "

loadkeys dvorak
dhcpcd

# partition disk
sgdisk -Z \
  -n 1:0:+512M -t 1:ef00 -c 1: bootefi \
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
lvcreate -L 8G -n swap vgroup
lvcreate -l 100%free -n root vgroup

# create filesystems
mkfs.fat -F32 "${DISK}1"
mkfs.ext4 /dev/mapper/vgroup-root
mkswap /dev/mapper/vgroup-swap

# mount filesystems
mount /dev/mapper/vgroup-root /mnt
mkdir -p /mnt/boot
mount "${DISK}1" /mnt/boot
swapon /dev/mapper/vgroup-swap

# base install
pacstrap /mnt base base-devel
genfstab -U -p /mnt >> /mnt/etc/fstab
cp chroot.sh /mnt
cp firstboot.sh /mnt/root
arch-chroot /mnt ./chroot.sh "$DISK" "$HOSTNAME"

umount -R /mnt
echo "chroot created, reboot and run firstboot.sh"
