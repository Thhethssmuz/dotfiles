# -- Install ------------------------------------------------------------------

loadkeys dvorak

# Partition harddrive
# fdisk -l to check if ssd is loaded at /dev/sda

gdisk /dev/sda
  # part  size   type          @
  # 1     +2MB   EF02 (BIOS)   /dev/sda1
  # 2     200MB  8300 (Linux)  /dev/sda2
  # 3     rest   8E00 (LVM)    /dev/sda3
reboot

# fdisk -l to check if ssd is loaded at /dev/sda

# format and encrypt disk
cryptsetup -c aes-xts-plain -y -s 512 luksFormat /dev/sda3

# Open newly encrypted drive
cryptsetup luksOpen /dev/sda3 lvm

# Create mapping and logical volume group
lvm pvcreate /dev/mapper/lvm
lvm vgcreate vgroup /dev/mapper/lvm

# Create logical partition on encrypted drive
lvm lvcreate -L 6G -n swap vgroup
lvm lvcreate -l 100%free -n root vgroup

# set filesystems
mkfs.ext2 /dev/sda2
mkfs.ext4 /dev/mapper/vgroup-root
mkswap /dev/mapper/vgroup-swap

# Mount disks
mount /dev/mapper/vgroup-root /mnt
mkdir /mnt/boot
mount /dev/sda2
swapon /dev/mapper/vgroup-swap

# Install base to /mnt
pacstrap /mnt base base-devel

# Install grub to /mnt
pacstrap /mnt grub-bios

# Generate fstab
genfstab -U -p /mnt >> /mnt/etc/fstab
nano /mnt/etc/fstab

# chroot and configure base system:
arch-chroot /mnt
  passwd
  echo archtop > /etc/hostname

  ln -sf /usr/share/zoneinfo/Europe/Oslo /etc/localtime

  nano /etc/locale.gen

  nano /etc/locale.conf
    LANG="en_GB.UTF-8"
    LC_COLLATE="C"

  locale-gen

  hwclock --systohc --utc

  nano /etc/vconsole.conf
    KEYMAP=dvorak
    FONT=lat9w-16
    FONT_MAP=8859-1_to_uni

  # Update Hooks in mkinitcpio.conf
  nano /etc/mkinitcpio.conf 
    HOOKS="base udev autodetect modconf block usbinput keymap encrypt lvm2 filesystems fsck"
  
  mkinitcpio -p linux

  grub-install /dev/sda

  grub-mkconfig -o /boot/grub/grub.cfg

  nano /etc/rc.conf
    USELVM="yes"

  nano /boot/grub/grub.cfg
    linux  /vmlinuz-linux root=/dev/mapper/vgroup-root cryptdevice=/dev/sda3:vgroup ro quiet

  exit

umount /mnt/boot
umount /mnt
swapoff

reboot

# -----------------------------------------------------------------------------

nano /etc/pacman.conf
  # Uncomment [multilib] section

pacman -Syy
pacman -Syu

pacman -S sudo

useradd -m -g users -G audio,disk,games,lp,network,optical,power,scanner,storage,sys,users,uucp,video,wheel -s /bin/bash thhethssmuz
passwd thhethssmuz

#make new user a sudoer
nano /etc/sudoers
  # uncomment %weel... line under "with password" section

# -----------------------------------------------------------------------------

# Install X
pacman -S xorg 
pacman -S xorg-server xorg-xinit xorg-server-utils
pacman -S mesa
pacman -S libgl

# Intel graphics driver
pacman -S xf86-video-i740

# Test X
startx
  # if every thing works pkill X

# Install some fonts
pacman -S ttf-dejavu

# Install Gnome
pacman -S gnome gnome-extra

systemctl enable gdm

# -- Setup --------------------------------------------------------------------

# See Arch.sh

# -- Rescue -------------------------------------------------------------------

  # mount lvm drives from boot medium:
  loadkeys dvorak

  modprobe dm_crypt
  modprobe dm_mod

  cryptsetup luksOpen /dev/sdb3 lvm  
  # note: sda3 <=> sdb3 (usb sometimes mounts to sda)

  vgscan
  vgchange -ay

  mkdir mnt
  mkdir /mnt/boot

  mount /dev/mapper/vgroup-root /mnt
  mount /dev/sda2 /mnt/boot

  arch-chroot /mnt

    # Do stuff... 

      # For example: Downgrade kernel
      # old kernels may be found in cach:
      cd /var/cache/pacman/pkg
      pacman -U linux-3.5.6-1-x86_64.pkg.tar.xz linux-headers-3.5.6-1-x86_64.pkg.tar.xz virtualbox-host-modules-4.2.0-5-x86_64.pkg.tar.xz
      
      grub-mkconfig -o /boot/grub/grub.cfg
      nano /boot/grub/grub.cfg # then add "cryptdevice=/dev/..."
      # linux  /vmlinuz-linux root=/dev/mapper/vgroup-root cryptdevice=/dev/sda3:vgroup ro quiet

    exit # to leave chroot

  umount /mnt/boot
  umount /mnt
  reboot
