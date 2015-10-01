# -- Setup --------------------------------------------------------------------

# run as root

pacman -S
  bash-completion
  bc

  chromium
  clementine

  elementary-icon-theme
  espeak

  flashflugin

  ghc cabal-install haddock happy alex
  gimp
  glfw
  gnome-tweak-tool
  gstreamer0.10-plugins # (all)
    # telepathy # (all: at least -gabble(empathy) or -haze(pidgin))
    # empathy   # (may need to be reinstalled after telepathy)
  guake

  human-icon-theme

  libreoffice
  lsb-release

  networkmanager
  network-manager-applet
  ngrep
  nodejs
  ntfs-3g
  numix-themes

  openssh

  pass
  pwgen

  redshift
  rsync

  samba gvfs-smb gnome-vfs

  tcpdump
  texlive-most
  traceroute
  ttf-dejavu
  ttf-droid
  ttf-freefont
  ttf-liberation
  ttf-linux-libertine
  ttf-ubuntu-font-family

  unrar

  vlc

  wget
  wine
  wireshark-gtk

  xsel

  youtube-dl


# xmonad test

pacman -S
  xmonad
  xmonad-contrib

  slim         # login manager, installs slimlock.
               # requires some configuration, go through /etc/slim.conf
               # themes are located in /usr/share/slim/themes/
  archlinux-themes-slim
  xautolock

  dzen2
  conky
  compton      # (aur) window transparency and effects
  hsetroot     # (aur) wallpaper
  gmrun        # gnome run applet

  unclutter    # hide mouse cursor when inactive
  numlockx     # activate numlock on start

  imagemagic   # image conversion tool (generally useful)

  feh          # image rendering tool (may not need?)
  xdotool      # execute key commands
  wmctrl       # for graceful exit script

  mpd          # music player daemon (start as user $ systemctl --user enable/start mpd)
  ncmpcpp      # terminal interface to mpd

  lxapperance  # Feature-rich GTK+ theme switcher of the LXDE Desktop

  gcalcli      # (AUR) Google Calendar Command Line Interface
  scrot        # A simple command-line screenshot utility for X


cabal install glfw glfw-b random pandoc

npm install -g jshint browserify babel stylus


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

# bindings for espeak
# bash -c "xsel | espeak -s 210"
# bash -c "xsel | espeak -v no -s 210"
# pkill -9 espeak

# Install from AUR
# - dropbox
# - nautilus-dropbox


# fix for awkward flash plugin glitch with pulseaudio
sudo ln -s /usr/lib/mozilla/plugins /opt/google/chrome/plugins
