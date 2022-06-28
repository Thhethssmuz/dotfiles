MAKEFLAGS += --no-builtin-rules --silent
SHELL      = /bin/bash

export
MAKEDIR    = $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
HOSTNAME  ?= $(shell hostname)
USERNAME  ?= $(shell echo '$(MAKEDIR)' | cut -d'/' -f3)
TYPE      ?=
TARGET    ?= all

ifneq ($(shell pwd),$(MAKEDIR))
$(error bad execution directory)
endif

.PHONY: all status check-updates update install explicit remove-orphans iso
.PHONY: archbox archtop sia-11 virtual %-vmk %-vmb %-vmw
.ONESHELL:


#
# Main targets
#

all: $(HOSTNAME)

status:
	$(MAKE) --no-print-directory $(TARGET) | lib/provision.sh status

check-updates:
	lib/provision.sh check-updates

update:
	lib/provision.sh update

install:
	$(MAKE) --no-print-directory $(TARGET) | lib/provision.sh install

explicit:
	$(MAKE) --no-print-directory $(TARGET) | lib/provision.sh explicit

remove-orphans:
	$(MAKE) --no-print-directory $(TARGET) | lib/provision.sh remove-orphans

iso:
	iso/mkiso.sh


#
# Hosts
#

archbox: \
	media-m.amt \
	base nvidia input fs mouse-accel \
	network network-manager sshd dropbox \
	audio pipewire pulse-tools desktop compton misc browsers \
	blender discord evince gimp libreoffice musescore reaper redshift signal steam sublime-text thunar virtualbox vlc weechat \
	dev docker mkvm

archtop: \
	base intel-ucode.pac mesa input touchpad fs \
	network networkd wifi dropbox \
	audio pipewire pulse-tools desktop misc browsers \
	evince gimp libreoffice musescore redshift sublime-text thunar virtualbox vlc weechat \
	dev docker mkvm mysql sqlite

sia-11: \
	mnt-winfil1-public.amt mnt-winfil1-operations.amt mnt-winfil1-sensors.amt \
	base nvidia input fs mouse-accel \
	network networkd sshd nxserver remmina \
	audio pipewire pulse-tools desktop compton misc browsers \
	evince gimp libreoffice sublime-text thunar thunderbird virtualbox vlc weechat \
	dev docker mkvm mysql sqlite

broxy: \
	media-dumptruck.amt \
	base.pac linux dbus pacman systemd secret -.mnt boot.mnt timezone.src \
	nvidia input fs ssh smb network-tools networkd sshd.srv \
	xorg cinnamon lightdm \
	audio pulse pulse-tools misc browsers vlc


#
# Virtual Hosts
#

virtual: \
	base mesa input fs \
	networkd ssh sshd smb network-tools vpn \
	desktop misc browsers \
	sublime-text weechat \
	dev docker

%-vm: virtual
	-
%-vmk: virtual
	-
%-vmb: virtual \
	virtualbox-guest-utils.pac virtualbox-guest-modules-arch.pac \
	vboxservice.srv
	-
%-vmw: virtual \
	open-vm-tools.pac vmware-vmblock-fuse.srv vmtoolsd.srv
	-


#
# Base
#

base: base.pac linux dbus pacman systemd secret -.mnt boot.mnt keymap.src timezone.src

dbus: dbus.pac
gpg: gnupg.pac gpg-agent.usc gpg-agent-browser.usc gpg-agent-extra.usc gpg-agent-ssh.usc
linux: linux.pac linux-firmware.pac
pacman: pacman.pac pacman-contrib.pac reflector.pac mirrorupgrade.cfg
systemd: logind.cfg systemd.pac
secret: gpg haveged.pac keychain.pac lvm2.pac pass.pac pass-otp.pac ssh


#
# Drivers
#

nvidia: nvidia.pac nvidia-utils.pac lib32-nvidia-utils.pac nvidia-settings.pac initial-nvidia-xorg.src
mesa: mesa.pac lib32-mesa.pac xf86-video-intel.pac arandr.pac
input: libevdev.pac xf86-input-evdev.pac
touchpad: xf86-input-synaptics.pac
fs: dosfstools.pac ntfs-3g.pac mtpfs.pac gvfs-mtp.pac
printer: cups.pac org.cups.cupsd.srv hplip.pac
mouse-accel: mouse-accel.cfg

#
# Network
#

network: ssh smb nxclient network-tools

ssh: openssh.pac sshfs.pac
smb: samba.pac
nxclient: nomachine.aur
network-tools: curl.pac gnu-netcat.pac httping.pac inetutils.pac ngrep.pac rsync.pac tcpdump.pac traceroute.pac whois.pac wireshark-qt.pac wget.pac

networkd: systemd-networkd.soc systemd-networkd.srv systemd-networkd-wait-online.srv systemd-resolvconf.pac systemd-resolved.srv systemd-resolv-stub.src
network-manager: networkmanager.pac NetworkManager.srv NetworkManager-wait-online.srv
sshd: ssh sshd.srv sshd.cfg
smbd: smb smbd.srv nmbd.srv smbd.cfg
nxserver: nxclient nxserver.srv
dropbox: dropbox.aur dropbox-cli.aur dropbox.usr dropbox.src
remmina: remmina.pac freerdp.pac
vpn: openconnect.pac
wifi: iwd.pac iwd.srv


#
# Audio
#

audio: alsa midi mpd mpc espeak audio-tools
pulse: pulseaudio-alsa.pac pulseaudio.usc
pipewire: pipewire.pac pipewire-alsa.pac pipewire-pulse.pac pipewire-jack.pac pipewire.usc pipewire-pulse.usc wireplumber.pac wireplumber.usr

alsa: alsa-utils.pac
pulse-tools: pavucontrol.pac
midi: timidity++.pac freepats-general-midi.pac
mpd: mpd.pac mpd.usr
mpc: mpc.pac ncmpcpp.pac
espeak: espeak-ng.pac
audio-tools: id3v2.pac


#
# Desktop
#

desktop: xorg slock xmonad environment

xorg: xorg-server.pac xorg-xinit.pac xorg-apps.pag
lxdm: lxdm.pac archlinux-lxdm-theme-full.aur lxdm.cfg lxdm.srv
slock: slock.pac xautolock.pac
xmonad: xmonad.pac xmonad-contrib.pac xmonadupgrade.cfg
environment: dzen2.pac haskell-async.pac haskell-dbus.pac ghc-libs.pac hsetroot.pac tesseract status-bar-icons.src

cinnamon: cinnamon.pac
compton: picom.pac
lightdm: lightdm.pac lightdm-gtk-greeter.pac
slim: slim.pac archlinux-themes-slim.pac slim.cfg
tesseract: tesseract.pac tesseract-data-eng.pac tesseract-data-nor.pac


#
# Misc
#

misc: home aur-helper x-tools fonts gtk-theme ranger terminals terminal-tools hunspell archive-tools image-tools sys-info

home: xdg-user-dirs.pac xdg-user-dirs-update.usr dotfiles.git .gnupg.git .password-store.git .ssh.git
aur-helper: curl.pac jshon.pac wget.pac apacman.src apacman.aur expac.pac
x-tools: numlockx.pac xclip.pac xdotool.pac xorg-xev.pac xorg-xprop.pac xorg-xwininfo.pac xsel.pac
fonts: gnu-free-fonts.pac gucharmap.pac ttf-dejavu-sans-mono-powerline-git.aur ttf-dejavu.pac ttf-droid.pac ttf-font-awesome.pac ttf-ionicons.pac ttf-liberation.pac ttf-linux-libertine.pac ttf-opensans.pac ttf-ubuntu-font-family.pac noto-fonts.pac noto-fonts-emoji.pac
gtk-theme: dconf.pac dconf.src elementary-icon-theme.pac arc-gtk-theme.pac lxappearance.pac
ranger: ranger.pac highlight.pac
terminals: alacritty.pac rxvt-unicode.pac xterm.pac
terminal-tools: bc.pac calc.pac diffutils.pac less.pac lsb-release.pac man-db.pac man-pages.pac nano.pac ncdu.pac pwgen.pac youtube-dl.pac
hunspell: hunspell.pac hunspell-en_gb.pac hunspell-en_us.pac # hunspell-no.src
archive-tools: atool.pac tar.pac unrar.pac unzip.pac zip.pac p7zip.pac
image-tools: imagemagick.pac scrot.pac gimp.pac sxiv.pac
sys-info: lshw.pac lsof.pac


#
# Browsers
#

browsers: chromium firefox opera

chromium: chromium.pac
firefox: firefox.pac
opera: opera.pac


#
# Applications
#

blender: blender.pac
discord: discord.pac
evince: evince.pac
gimp: gimp.pac
libreoffice: libreoffice-fresh.pac
musescore: midi musescore.pac
kvm: qemu.pac ovmf.pac
reaper: reaper-bin.aur carla.pac
redshift: redshift.pac redshift.cfg redshift.usr
signal: signal-desktop.pac
steam: steam.pac steam-fonts.aur
sublime-text: sublime-text-dev.aur package-control.src
thunar: thunar.pac
thunderbird: thunderbird.pac
transmission: transmission-gtk.pac
virtualbox: virtualbox.pac virtualbox-host-modules-arch.pac
vlc: vlc.pac
weechat: weechat.pac


#
# Development
#

dev: bash cpp git haskell js pdf json dev-tools sublime-text

bash: bash-completion.pac shellcheck.pac
cpp: clang.pac make.pac cmake.pac ninja.pac gdb.pac
git: git.pac tig.pac
haskell: ghc.pac stack.pac ncurses5-compat-libs.aur
js: nodejs.pac npm.pac eslint.npm prettier.npm
latex: texlive-most.pag
pdf: latex pandoc.pac evince # ppp
json: jq.pac jshon.pac
dev-tools: git make.pac testssl.sh.pac the_silver_searcher.pac

mkvm: archiso.pac
docker: docker.pac docker.srv loop.mod
k3s: k3s-bin.aur kubectl.pac helm.pac
mysql: mariadb.pac mariadb.srv
sqlite: sqlite.pac sqlite3.npm


#
# List targets
#

%.pag:
	[[ "$@" =~ $(TYPE)$$ ]] && pacman -Sgq "$*" | sed 's/$$/.pac/' || true

%.pac:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.aur:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.npm:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true

%.cfg:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.fil:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true

%.mod:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.srv:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.soc:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.usr:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.usc:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.mnt:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.amt:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true

%.git:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true

%.src:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
