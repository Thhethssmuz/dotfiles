#!/bin/bash
set -exuo pipefail

# should turn this into a proper script at some point... but for now just go
# through each section in order.
echo "this is a pseudo script..." 1>&2
exit 1

# For further reading, most of this script is taken from:
# https://wiki.archlinux.org/index.php/USB_flash_installation_media#Using_manual_formatting

DISC=/dev/sde
ISO=

#
# Create partition table for the usb device
#
partition() {

  # check dependencies
  if ! hash sgdisk 2>/dev/null; then
    echo "sgdisk does not appear to be installed, run \`pacman -S gdisk' to install"
    exit 1
  fi

  # create partition table
  #   1. 1G   - bootable arch live image
  #   2. 24G  - encrypted data storage
  #   3. rest - unencrypted data storage
  sgdisk -Z \
    -n 1:0:+1G -t 1:ef00 -c 1:"arch linux" \
    -n 2:0:+24G -t 2:8300 -c 2:"crypton" \
    -n 3:0:0 -t 3:0700 -c 3:"dump" \
    -p "$DISC"
}

#
# Install the arch live image to the first partition on the usb device
#
install_live_image() {

  # check dependencies
  if ! hash syslinux 2>/dev/null; then
    echo "syslinux does not appear to be installed, run \`pacman -S syslinux' to install"
    exit 1
  fi

  # format file-systems
  mkfs.fat -F32 "${DISC}1"

  # mount boot drive and arch iso image and copy over iso contents to the live
  # partition
  mkdir -p /mnt/{iso,usb}
  mount -o loop "$ISO" /mnt/iso
  mount "${DISC}1" /mnt/usb
  cp -a /mnt/iso/* /mnt/usb
  sync
  umount /mnt/iso

  # get the live partitions UUID
  UUID=$(blkid -o value -s UUID "${DISC}1")

  # set the boot UUID in the live arch boot configs by replacing the setting
  # `archisolabel=<...>` with `archisodevice=/dev/disk/by-uuid/<UUID>`
  for file in /mnt/usb/{arch/boot/syslinux/archiso_sys{32,64}.cfg,loader/entries/archiso-x86_64.conf}; do
    sed -e "s/archisolabel=.*$/archisodevice=\/dev\/disk\/by-uuid\/$UUID/" "$file" > "$file.tmp"
    mv "$file.tmp" "$file"
  done

  # install syslinux to the live partition
  cp -r /usr/lib/syslinux/bios/*.c32 /mnt/usb/arch/boot/syslinux
  extlinux --install /mnt/usb/arch/boot/syslinux

  # TODO: inject install scripts to device
  # https://wiki.archlinux.org/index.php/Remastering_the_Install_ISO#Customization

  # mark the partition as bootable
  umount /mnt/usb
  sgdisk "$DISC" --attributes=1:set:2
  dd bs=440 conv=notrunc count=1 if=/usr/lib/syslinux/bios/gptmbr.bin of="$DISC"

  sync
  umount /mnt/usb
  rm -rf /mnt/{iso,usb}
}

#
# Make encrypted partition
#
make_encrypted_partition() {

  # encrypt partition
  cryptsetup -v --cipher aes-xts-plain64 --key-size 512 --hash sha512 \
    --use-random luksFormat "${DISC}2"

  # make file system
  cryptsetup luksOpen "${DISC}2" crypton
  mkfs.ext4 /dev/mapper/crypton

  # mount device
  mkdir -p ~/crypton
  mount /dev/mapper/crypton ~/crypton
  chown thhethssmuz:users ~/crypton

  # add private repos and any other private data to the device
  cd ~/crypton
  git clone git@brogs:Thhethssmuz/gpg.git .gnupg
  git clone git@brogs:Thhethssmuz/ssh.git .ssh
  git clone git@brogs:Thhethssmuz/pass.git .password-store

  sync
  umount ~/crypton
  cryptsetup close crypton
}

#
# Make unencrypted general purpose partition
#
make_unencrypted_partition() {
  mkfs.ntfs --quick "${DISC}3"
  sync
}