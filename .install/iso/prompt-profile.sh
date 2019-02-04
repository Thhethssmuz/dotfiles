#!/bin/bash

echo ""
echo "ISO profiles"
echo "  usb: Generic, suitable for USB stick"
echo "  vmk: Auto-install for KVM guest"
echo "  vmb: Auto-install for VirtualBox guest"
echo "  vmw: Auto-install for VMWare guest"
echo ""

while true; do
  read -rp "Select ISO profile: " PROFILE
  case "$PROFILE" in

    usb)
      break ;;

    vmk)
      DISC=/dev/vda
      . ~/.install/iso/prompt-vm.sh
      break ;;

    vmb)
      DISC=/dev/sda
      . ~/.install/iso/prompt-vm.sh
      break ;;

    # vmw)
    #   DISC=/dev/???
    #   . prompt-vm.sh
    #   break ;;

    *)
      echo "invalid type" ;;
  esac
done

export PROFILE
export DISC
