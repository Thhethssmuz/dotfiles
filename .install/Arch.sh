# -- Setup --------------------------------------------------------------------

# run as root

pacman -S
  archlinux-themes-slim   # theme for slim

  bash-completion
  bc                      # commandline calculator
  blender                 # 3D editor

  calc
  chromium
  clang
  dzen2

  elementary-icon-theme
  espeak

  firefox
  #flashplugin            # may not need any more?

  # the Haskell platform
    ghc
    cabal-install
    haddock
    happy
    alex

  gimp
  glfw
  gmrun                   # gnome run applet
  #gnome-tweak-tool       # not really useful with xmonad
  gstreamer0.10-plugins   # (all)
  guake

  human-icon-theme
  hunspell
  hunspell-en

  imagemagic              # image conversion tool (generally useful)

  libreoffice
  lolcat                  # fun
  lsb-release
  lxapperance             # GTK+ theme switcher

  mpc                     # for media button bindings
  mpd                     # music player daemon (start as user $ systemctl --user enable/start mpd)

  ncmpcpp                 # terminal interface to mpd
  networkmanager
  network-manager-applet
  ngrep
  nodejs
  npm
  ntfs-3g
  numix-themes
  numlockx                # for activating numlock on start

  openssh

  pass
  pwgen

  redshift
  rsync
  rxvt-unicode            # terminal

  scrot                   # A simple command-line screenshot utility for X
  shellcheck
  slim                    # login manager, requires some configuration in /etc/slim.conf,
                          # themes are located in /usr/share/slim/themes/
  slimlock                # screen locker
  # smb
    samba
    gvfs-smb
    gnome-vfs
  sshfs

  tcpdump
  texlive-most
  tig
  traceroute
  ttf-dejavu
  ttf-droid
  ttf-freefont
  ttf-liberation
  ttf-linux-libertine
  ttf-ubuntu-font-family

  unclutter               # hide mouse cursor when inactive
  unrar
  unzip

  vlc

  wget
  wine
  wireshark-gtk
  wmctrl                  # for graceful exit script

  xautolock               # automatic lock screen after a period
  xclip
  xdotool                 # execute key commands
  xmonad
  xmonad-contrib
  xsel

  youtube-dl

# -- Install from the AUR -----------------------------------------------------

  compton                 # (aur) window transparency and effects

  dropbox                 # (aur)
  dropbox-cli             # (aur)

  gcalcli                 # (aur) Google Calendar Command Line Interface

  hsetroot                # (aur) wallpaper


# -- Install Haskell packages -------------------------------------------------

cabal install
  dbus
  glfw
  glfw-b
  pandoc
  random

# -- Install Node.js packages -------------------------------------------------

npm install -g
  browserify
  jshint
  pm2

# -- Symlinks -----------------------------------------------------------------

# symlink user-themes dirs in ~/.themes and ~/.local/share/themes
ln -s ~/.themes ~/.local/share/themes

# symlink music folders (~/Music/X will be created)
ln -s /media/X/Music ~/Music/X

# -- NetworkManager -----------------------------------------------------------

systemctl enable NetworkManager
systemctl start NetworkManager

# -- Samba --------------------------------------------------------------------

cp /etc/samba/smb.conf.default /etc/samba/smb.conf

echo '
[Series]
  path = /media/X/Series
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no

[Movies]
  path = /media/X/Movies
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no

[Music]
  path = /media/X/Music
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no

[Series2]
  path = /media/U/Series2
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no

[Movies2]
  path = /media/U/Movies2
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no

[Music2]
  path = /media/U/Music
  valid users = thhethssmuz
  public = no
  writable = no
  printable = no
' >> /etc/samba/smb.conf

# add user to samba (user must already be a system user)
pdbedit -a -u thhethssmuz

systemctl enable smbd
systemctl enable nmbd
systemctl start smbd
systemctl start nmbd

# Failed to start Samba SMB/CIFS server
# try: $ chmod 0755 /var/cache/samba/msg


# -- Fonts --------------------------------------------------------------------

wget https://github.com/driftyco/ionicons/blob/master/fonts/ionicons.ttf \
  ~/.local/share/fonts/ionicons.ttf

wget https://github.com/FortAwesome/Font-Awesome/blob/master/fonts/fontawesome-webfont.ttf?raw=true \
  ~/.local/share/fonts/fontawesome-webfont.tt


# -- Crypton ------------------------------------------------------------------

# Set ownership
# $ mkdir MOUNTPOINT
# ... mount
# $ sudo chown USER -R MOUNTPOINT

# mount

  # sudo cryptsetup --type luks open /dev/sd# DEVICENAME
  # sudo mount -t FILEFORMAT /dev/mapper/DEVICENAME MOUNTPOINT

# unmount

  # sudo umount MOUNTPOINT
  # sudo cryptsetup close /dev/mapper/DEVICENAME

# -- Other --------------------------------------------------------------------

# fix for awkward flash plugin glitch with pulseaudio
ln -s /usr/lib/mozilla/plugins /opt/google/chrome/plugins
