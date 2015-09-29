# -- Install ------------------------------------------------------------------

loadkeys dvorak

# current partition layout
# /dev/sda1   system reserved
# /dev/sda2   win 7
# /dev/sda3   arch (boot flag)

# for info run fdisk -l

cfdisk /dev/sda

# format and mount the install drive
mkfs.ext4 -E discard /dev/sda3 # -E discard is for SSD partitions
mount /dev/sda3 /mnt

# connect to the web
dhcpcd
  # test by pinging google or something

# Install base to /mnt
# base-devel contains build essensials
pacstrap /mnt base base-devel
# outdatet keys?
  # pacman -Sy
  # pacman -S archlinux-keyring

# Install grub to /mnt
pacstrap /mnt grub-bios

# Generate fstab
genfstab -U -p /mnt >> /mnt/etc/fstab
nano /mnt/etc/fstab
  # for SSD discs add the discard option
  UUID=#### / ext4 rw,relatime,data=ordered,discard 0 1

# chroot and configure base system:
arch-chroot /mnt

  mkinitcpio -p linux

  grub-mkconfig -o /boot/grub/grub.cfg
  grub-install /dev/sda

  # if wifi
  packman -S dialog wpa_supplicant

  passwd
  
  exit

umount /mnt
systemctl reboot

# -----------------------------------------------------------------------------

hostnamectl set-hostname archbox

timedatectl set-timezone Europe/Oslo

nano /etc/locale.gen
locale-gen

localectl set-locale LANG="en_GB.UTF-8"

timedatectl set-local-rtc true

# -----------------------------------------------------------------------------

# Add windows to grub
nano /boot/grub/grub.cfg
  # add section:
    # Windows
    menuentry "Windows 7" {
    set root=(hd0,1)
    chainloader +1
    }

# Add multilib
nano /etc/pacman.conf
  # Uncomment [multilib] section

pacman -Syy
pacman -Syu

pacman -S sudo

groupadd networkmanager

useradd -m -g users -G audio,disk,games,lp,network,networkmanager,optical,power,scanner,storage,sys,uucp,video,wheel -s /bin/bash thhethssmuz
passwd thhethssmuz

#make new user a sudoer
nano /etc/sudoers
  # uncomment %weel... line under "with password" section

# -----------------------------------------------------------------------------

# alsa
pacman -S alsa-utils

# Install X
pacman -S xorg-server

# Nvidia
  pacman -S nvidia
  # if pacman fails to remove dependencies:
    pacman -Rdd libgl
    pacman -Rdd mesa-libgl

# Test x
startx
  # if it fails try generating new xorg.conf with:
  nvidia-xconfig

systemctl reboot

# -----------------------------------------------------------------------------

# Install some fonts
pacman -S ttf-dejavu

# Install Gnome
pacman -S gnome gnome-extra

systemctl enable gdm

# -----------------------------------------------------------------------------

# Automount X and U
sudo nano /etc/fstab
  # add:
    # /dev/sdc1
    UUID=1C98A44498A41E70 media/X ntfs-3g uid=thhethssmuz,gid=users 0 0
  # UUID can be found by using: lsblk -f

pacman -S xorg-xrandr

# -- Setup --------------------------------------------------------------------

# See Arch.sh

# -- Rescue mode --------------------------------------------------------------

  loadkeys dvorak

  mkdir mnt
  mount /dev/sda3 /mnt

  arch-chroot /mnt

    # Do stuff... 

    exit # to leave chroot

  umount /mnt
  reboot

