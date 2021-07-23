#!/bin/bash
set -eo pipefail

# for future reference, most of this script is taken from:
# https://wiki.archlinux.org/index.php/USB_flash_installation_media#Using_manual_formatting


TMP="$HOME/.install/.tmp"
USB="$HOME/.install/usb"
DISK=
ISO=


#
# Create partition table for the usb device
#
partition() {

  # check dependencies
  if ! hash sgdisk 2>/dev/null; then
    echo "sgdisk does not appear to be installed, run 'pacman -S gdisk' to install"
    exit 1
  fi

  echo "WARNING: this will overwrite the disk $DISK"
  echo "This is a $(lsblk "$DISK" -ldn | awk '{print $4}') disk"
  read -rp "Press enter to continue"

  set -x

  # create partition table
  #   1. 4G   - bootable arch live image
  #   2. 24G  - encrypted data storage
  #   3. rest - unencrypted data storage
  sudo sgdisk -Z "$DISK"
  sudo sgdisk \
    -n 1:0:+4G -t 1:ef00 -c 1:"arch linux" \
    -n 2:0:+24G -t 2:8300 -c 2:"crypton" \
    -n 3:0:0 -t 3:0700 -c 3:"dump" \
    -p "$DISK"
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
  if ! hash unsquashfs 2>/dev/null; then
    echo "squashfs-tools does not appear to be installed, run \`pacman -S squashfs-tools' to install"
    exit 1
  fi

  set -x

  # format file-system
  sudo mkfs.fat -F32 "${DISK}1"

  # mount boot drive and copy over iso contents to the live partition
  mkdir -p "$TMP"/usb
  sudo mount "${DISK}1" "$TMP"/usb
  sudo bsdtar -x -f "$ISO" -C "$TMP"/usb
  sync

  # get the partitions UUIDs
  ARCH_UUID=$(sudo blkid -o value -s UUID "$DISK"1)
  CRYPTON_UUID=$(sudo blkid -o value -s UUID "$DISK"2)
  DUMP_UUID=$(sudo blkid -o value -s UUID "$DISK"3)

  # set the boot UUID in the live arch boot configs by replacing the setting
  # `archisolabel=<...>` with `archisodevice=/dev/disk/by-uuid/<UUID>`
  for file in "$TMP"/usb/loader/entries/*.conf; do
    sudo sed -i "s/archisolabel=.*$/archisodevice=\/dev\/disk\/by-uuid\/$ARCH_UUID/" "$file"
  done

  # inject uuid files ++ to device
  for arch in "$TMP"/usb/arch/x86_64; do
    sudo unsquashfs -dest "$TMP"/squashfs-root "$arch/airootfs.sfs"

    # .bashrc seams to be deleted by archiso script :/
    # for now just inject it again here...
    sudo cp "$USB"/.bashrc "$TMP"/squashfs-root/root/

    sudo bash -c "echo '$ARCH_UUID' > $TMP/squashfs-root/root/arch.uuid"
    sudo bash -c "echo '$CRYPTON_UUID' > $TMP/squashfs-root/root/crypton.uuid"
    sudo bash -c "echo '$DUMP_UUID' > $TMP/squashfs-root/root/dump.uuid"

    sudo rm "$arch/airootfs.sfs"
    sudo mksquashfs "$TMP"/squashfs-root "$arch/airootfs.sfs"
    sudo rm -rf "$TMP"/squashfs-root
    ( cd "$arch" && sudo bash -c "sha512sum airootfs.sfs > airootfs.sha512" )
  done

  # mark the partition as bootable
  sync
  sudo umount "$TMP"/usb
  sudo syslinux --directory syslinux --install "$DISK"1
  sudo dd bs=440 count=1 conv=notrunc if=/usr/lib/syslinux/bios/gptmbr.bin of="$DISK"

  sync
  sudo rm -rf "$TMP"/usb
}


#
# Make encrypted partition
#
make_encrypted_partition() {

  set -x

  # encrypt partition
  sudo cryptsetup -v --cipher aes-xts-plain64 --key-size 512 --hash sha512 \
    --use-random luksFormat "${DISK}2"

  # make file system
  sudo cryptsetup luksOpen "${DISK}2" crypton
  sudo mkfs.ext4 /dev/mapper/crypton

  # mount device
  mkdir -p "$TMP"/crypton
  sudo mount /dev/mapper/crypton "$TMP"/crypton
  sudo chown -R thhethssmuz:users "$TMP"/crypton

  # add private repos and any other private data to the device
  git clone git@brogs:Thhethssmuz/gpg.git "$TMP"/crypton/.gnupg
  git clone git@brogs:Thhethssmuz/ssh.git "$TMP"/crypton/.ssh
  git clone git@brogs:Thhethssmuz/pass.git "$TMP"/crypton/.password-store
  sudo chmod 700 "$TMP"/crypton/{.gnupg,.ssh,.password-store}

  sync
  sudo umount "$TMP"/crypton
  sudo cryptsetup close crypton
}


#
# Make unencrypted general purpose partition
#
make_unencrypted_partition() {
  set -x
  sudo mkfs.ntfs --quick "${DISK}3"
  sync
}


#
# Print usage
#
usage() {
  cat <<EOF
Usage: $(basename "$0") [-h | --help] (-d | --disk <disk>) [-i | --iso <path>] <command>

OPTIONS:
  -d, --disk <disk>     the disk to operate on
  -i, --iso <path>      specify the path to the arch iso
  -h, --help            show this help message and exit

COMMANDS:
  partition             partition the device
  dump                  install the unencrypted partition
  crypton               install the encrypted partition
  arch                  install live image to the first partition, this asumes
                        that the device has been partitioned and that the two
                        other expected partitions have already been made
  all                   all of the above

EOF
}


#
# Main
#
DO_PARTITION=false
DO_DUMP=false
DO_CRYPTON=false
DO_ARCH=false

while [[ $# -gt 0 ]]; do

  case $1 in

    -h|--help)
      usage
      exit 0
      ;;

    -d|--disk|--drive)
      if [ -z "$2" ]; then
        echo "missing argument \`$1'" 1>&2
        exit 1
      elif ! [[ $2 =~ ^/dev/sd[a-z]$ ]]; then
        echo "invalid argument \`$2'" 1>&2
        exit 1
      fi
      DISK="$2"
      shift
      ;;

    -i|--iso)
      if [ -z "$2" ]; then
        echo "missing argument \`$1'" 1>&2
        exit 1
      fi
      ISO="$2"
      shift
      ;;

    --*)
      echo "$(basename "$0"): invalid option \`$1'" 1>&2
      echo "Try $(basename "$0") --help for more info" 1>&2
      exit 1
      ;;


    -??*)
      set -- "-${1:1:1}" "-${1:2}" "${@:2}"
      continue
      ;;

    partition)
      DO_PARTITION=true
      ;;

    arch)
      DO_ARCH=true
      ;;

    crypton)
      DO_CRYPTON=true
      ;;

    dump)
      DO_DUMP=true
      ;;

    all)
      DO_PARTITION=true
      DO_ARCH=true
      DO_CRYPTON=true
      DO_DUMP=true
      ;;

    *)
      echo "$(basename "$0"): invalid command \`$1'" 1>&2
      echo "Try $(basename "$0") --help for more info" 1>&2
      exit 1
      ;;

  esac

  shift

done

if [ -z "$DISK" ]; then
  echo "missing option \`--disk'" 1>&2
  exit 1
fi

if $DO_ARCH && [ -z "$ISO" ]; then
  echo "missing option \`--iso'" 1>&2
  exit 1
fi

if [ "$EUID" -eq 0 ]; then
  echo "This script should not be run as root" 1>&2
  exit 1
fi

$DO_PARTITION && partition
$DO_DUMP && make_unencrypted_partition
$DO_CRYPTON && make_encrypted_partition
$DO_ARCH && install_live_image

rm -rf "$TMP"
