#!/bin/bash
set -exuo pipefail

dhcpcd

passwd

# include multilib
sed -i '/^#\[multilib\]$/ s/$/\nInclude = \/etc\/pacman.d\/mirrorlist/' /etc/pacman.conf

# update
pacman -Syy
pacman -Syu

# install sudo and add wheel group
pacman -S --noconfirm  sudo
sed -i '/^# %wheel ALL=(ALL) ALL$/ s/^# //' /etc/sudoers
visudo -cf /etc/sudoers

# make user
useradd -m -g users -G audio,games,rfkill,uucp,video,wheel -s /bin/bash thhethssmuz
passwd thhethssmuz

# localectl
# timedatectl
# video drivers
# ...
