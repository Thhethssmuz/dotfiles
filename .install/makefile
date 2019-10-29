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
.PHONY: archbox archtop sia-10 virtual %-vmk %-vmb %-vmw
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
	media-lib.amt media-u.amt media-x.amt \
	base nvidia input fs \
	network network-manager sshd dropbox \
	audio desktop compton misc browsers \
	blender evince gimp libreoffice musescore reaper redshift steam sublime-text thunar virtualbox vlc weechat \
	dev docker mkvm

archtop: \
	base mesa input touchpad fs \
	network network-manager wifi dropbox \
	audio desktop misc browsers \
	evince gimp libreoffice musescore redshift sublime-text thunar virtualbox vlc weechat \
	dev docker mkvm mysql

sia-10: \
	mnt-winfil1-public.amt mnt-winfil1-operations.amt mnt-winfil1-sensors.amt \
	base nvidia input touchpad fs printer \
	network network-manager sshd nxserver remmina \
	audio desktop compton misc browsers \
	evince gimp libreoffice sublime-text thunar thunderbird virtualbox vlc weechat \
	dev docker mkvm mysql

sia-11: \
	mnt-winfil1-public.amt mnt-winfil1-operations.amt mnt-winfil1-sensors.amt \
	base nvidia input fs mouse-accel \
	network networkd sshd nxserver \
	audio desktop compton misc browsers \
	evince gimp libreoffice sublime-text thunar thunderbird virtualbox vlc weechat \
	dev docker mkvm mysql


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

base: base.pac dbus pacman systemd secret -.mnt boot.mnt keymap.src timezone.src

dbus: dbus.pac
gpg: gnupg.pac gpg-agent.usc gpg-agent-browser.usc gpg-agent-extra.usc gpg-agent-ssh.usc
pacman: pacman.pac pacman-contrib.pac reflector.pac mirrorupgrade.cfg
systemd: logind.cfg systemd.pac
secret: gpg haveged.pac keychain.pac pass.pac ssh


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
network-tools: curl.pac bind-tools.pac gnu-netcat.pac httping.pac ngrep.pac rsync.pac tcpdump.pac traceroute.pac whois.pac wireshark-qt.pac wget.pac

networkd: systemd-networkd.soc systemd-networkd.srv systemd-networkd-wait-online.srv systemd-resolvconf.pac systemd-resolved.srv
network-manager: networkmanager.pac NetworkManager.srv NetworkManager-wait-online.srv
sshd: ssh sshd.srv sshd.cfg
smbd: smb smbd.srv nmbd.srv smbd.cfg
nxserver: nxclient nxserver.srv
dropbox: dropbox.aur dropbox-cli.aur dropbox.usr dropbox.src
remmina: remmina.pac freerdp.pac remmina-plugin-rdesktop.aur
vpn: openconnect.pac
wifi: iw.pac dialog.pac wpa_supplicant.pac


#
# Audio
#

audio: alsa pulse midi mpd mpc espeak audio-tools

alsa: alsa-utils.pac
pulse: pulseaudio-alsa.pac pavucontrol.pac pulseaudio.usc
midi: timidity++.pac freepats-general-midi.pac
mpd: mpd.pac mpd.usr
mpc: mpc.pac ncmpcpp.pac
espeak: espeak-ng.pac
audio-tools: id3v2.pac


#
# Desktop
#

desktop: xorg lxdm slock xmonad environment

xorg: xorg-server.pac xorg-xinit.pac xorg-apps.pag
lxdm: lxdm.pac archlinux-lxdm-theme-full.aur lxdm.cfg lxdm.srv
slock: slock.pac xautolock.pac
xmonad: xmonad.pac xmonad-contrib.pac xmonadupgrade.cfg
environment: dzen2.pac haskell-async.pac haskell-dbus.pac ghc-libs.pac hsetroot.pac status-bar-icons.src

slim: slim.pac archlinux-themes-slim.pac slim.cfg
compton: compton.pac


#
# Misc
#

misc: home aur-helper x-tools fonts gtk-theme ranger terminals terminal-tools hunspell archive-tools image-tools sys-info

home: xdg-user-dirs.pac xdg-user-dirs-update.usr dotfiles.git .gnupg.git .password-store.git .ssh.git
aur-helper: curl.pac jshon.pac wget.pac apacman.src apacman.aur expac.pac
x-tools: numlockx.pac xclip.pac xdotool.pac xorg-xev.pac xorg-xprop.pac xorg-xwininfo.pac xsel.pac
fonts: gnu-free-fonts.pac gucharmap.pac ttf-dejavu-sans-mono-powerline-git.aur ttf-dejavu.pac ttf-droid.pac ttf-font-awesome.pac ttf-ionicons.pac ttf-liberation.pac ttf-linux-libertine.pac ttf-opensans.pac ttf-ubuntu-font-family.pac
gtk-theme: dconf.pac dconf.src elementary-icon-theme.pac arc-gtk-theme.pac lxappearance.pac
ranger: ranger.pac highlight.pac
terminals: alacritty.pac rxvt-unicode.pac xterm.pac
terminal-tools: bc.pac calc.pac diffutils.pac less.pac lsb-release.pac man-pages.pac nano.pac ncdu.pac pwgen.pac youtube-dl.pac
hunspell: hunspell.pac hunspell-en_GB.pac hunspell-en_US.pac # hunspell-no.src
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
evince: evince.pac
gimp: gimp.pac
libreoffice: libreoffice-fresh.pac
musescore: midi musescore.pac
kvm: qemu.pac ovmf.pac
reaper: reaper-bin.aur carla.pac
redshift: redshift.pac redshift.cfg redshift.usr
steam: steam.pac steam-fonts.aur steam-native-runtime.pac
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
js: nodejs.pac npm.pac eslint.npm
latex: texlive-most.pag
pdf: latex pandoc.pac evince # ppp
json: jq.pac jshon.pac
dev-tools: git make.pac testssl.sh.pac the_silver_searcher.pac

mkvm: archiso.pac
docker: docker.pac docker.srv loop.mod
mysql: mariadb.pac mariadb.srv mysqld.srv


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
