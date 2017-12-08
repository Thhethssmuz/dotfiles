#!/bin/bash

# $ sudo pacman -S archiso qemu ovmf
# $ modprobe kvm
# $ modprobe kvm_intel

build_vm_iso() {
  mkdir -p archlive
  cp -r /usr/share/archiso/configs/releng/* archlive

  # copy install scripts to live image
  mkdir -p archlive/airootfs/etc/skel
  cp vm/* archlive/airootfs/etc/skel/

  # TODO: install variables
  sed -i "s/\\\$USERNAME/$(whoami)/g" archlive/airootfs/etc/skel/firstboot.sh
  sed -i "s/\\\$PASSWORD/1234/g" archlive/airootfs/etc/skel/firstboot.sh

  # automatically source the live script when starting up the live image
  sed -i 's/zsh/bash/' archlive/airootfs/root/customize_airootfs.sh
  echo 'echo "bash live.sh" >> /root/.bashrc' \
    >> archlive/airootfs/root/customize_airootfs.sh

  # build the image
  mkdir -p archlive/out/
  ( cd archlive && sudo ./build.sh -v )

  # copy out and clean up
  mv archlive/out/archlinux-*-x86_64.iso .
  sudo rm -rf archlive
}

build_vm() {
  qemu-img create -f qcow2 varch.cow 10G
  cp /usr/share/ovmf/ovmf_vars_x64.bin varch-uefi.bin

  qemu-system-x86_64 \
    -name varch \
    -display sdl \
    -boot once=d \
    -cdrom archlinux-2017.12.08-x86_64.iso \
    -m 2G \
    -machine type=pc,accel=kvm \
    -drive file=varch.cow,if=virtio,cache=writeback,discard=ignore,format=qcow2 \
    -drive if=pflash,format=raw,readonly,file=/usr/share/ovmf/ovmf_code_x64.bin \
    -drive if=pflash,format=raw,file=varch-uefi.bin
}

run_vm() {
  qemu-system-x86_64 \
    -name varch \
    -display sdl \
    -boot once=d \
    -m 2G \
    -machine type=pc,accel=kvm \
    -drive file=varch.cow,if=virtio,cache=writeback,discard=ignore,format=qcow2 \
    -drive if=pflash,format=raw,readonly,file=/usr/share/ovmf/ovmf_code_x64.bin \
    -drive if=pflash,format=raw,file=varch-uefi.bin
}

"$@"
