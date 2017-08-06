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

.PHONY: all status check-updates update install explicit remove
.PHONY: archbox archtop sia-10
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

remove:
	$(MAKE) --no-print-directory $(TARGET) | lib/provision.sh remove

#
# Hosts
#

archbox: base \
	nvidia input fs \
	network audio \
	xorg lxdm slock xmonad compton environment misc dev \
	blender browsers evince gimp libreoffice redshift steam sublime-text transmission vlc weechat \
	sshd smbd dropbox

archtop: base \
	mesa input touchpad fs \
	network audio \
	xorg lxdm slock xmonad environment misc dev \
	browsers evince gimp libreoffice sublime-text vlc weechat \
	dropbox

sia-10: base \
	nvidia input touchpad fs \
	network audio \
	xorg lxdm slock xmonad compton environment misc dev \
	browsers evince gimp libreoffice sublime-text thunderbird transmission vlc weechat \
	sshd nxserver pm2


#
# Base
#

base: systemd secret

systemd: logind.cfg
secret: gnupg.pac haveged.pac keychain.pac pass.pac ssh


#
# Drivers
#

nvidia: nvidia.pac nvidia-utils.pac lib32-nvidia-utils.pac nvidia-settings.pac initial-nvidia-xorg.ini
mesa: mesa.pac lib32-mesa.pac xf86-video-intel.pac arandr.pac
input: libevdev.pac xf86-input-evdev.pac
touchpad: xf86-input-synaptics.pac
fs: dosfstools.pac ntfs-3g.pac mtpfs.pac


#
# Network
#

network: network-manager ssh smb nxclient network-tools

network-manager: networkmanager.pac NetworkManager.srv NetworkManager-wait-online.srv
ssh: openssh.pac sshfs.pac
smb: samba.pac
nxclient: nomachine.aur
network-tools: curl.pac bind-tools.pac ngrep.pac rsync.pac tcpdump.pac traceroute.pac whois.pac wireshark-gtk.pac wget.pac

sshd: ssh sshd.srv sshd.cfg
smbd: smb smbd.srv nmbd.srv smbd.cfg
nxserver: nxclient nxserver.srv
dropbox: dropbox.aur dropbox-cli.aur dropbox.usr


#
# Audio
#

audio: alsa pulse midi mpd mpc espeak audio-tools

alsa: alsa-utils.pac
pulse: pulseaudio-alsa.pac pavucontrol.pac pulseaudio.usr
midi: timidity++.pac timidity-freepats.pac
mpd: mpd.pac mpd.usr
mpc: mpc.pac ncmpcpp.pac
espeak: espeak.pac
audio-tools: mp3info.pac
music-tools: musescore.pac


#
# Desktop
#

xorg: xorg-server.pac xorg-xinit.pac xorg-server-utils.pac
lxdm: lxdm.pac archlinux-lxdm-theme-full.aur lxdm.cfg lxdm.srv
slim: slim.pac archlinux-themes-slim.pac slim.cfg
slock: slock.pac xautolock.pac
xmonad: xmonad.pac xmonad-contrib.pac
compton: compton.pac
environment: dzen2.pac haskell-async.pac haskell-dbus.pac haskell-stm.pac hsetroot.aur


#
# Misc
#

misc: home aur-helper x-tools fonts gtk-theme terminals terminal-tools hunspell archive-tools image-tools

home: xdg-user-dirs.pac
aur-helper: curl.pac wget.pac apacman.aur
x-tools: numlockx.pac xclip.pac xdotool.pac xorg-xev.pac xorg-xprop.pac xorg-xwininfo.pac xsel.pac
fonts: ttf-dejavu.pac ttf-liberation.pac ttf-ubuntu-font-family.pac ttf-ionicons.pac ttf-font-awesome.aur ttf-opensans.aur ttf-dejavu-sans-mono-powerline-git.aur
gtk-theme: elementary-icon-theme.pac numix-gtk-theme.pac lxappearance.pac
terminals: guake.pac rxvt-unicode.pac xterm.pac
terminal-tools: bc.pac calc.pac lolcat.pac lsb-release.pac nano.pac ncdu.pac ranger.pac youtube-dl.pac zalgolize.npm
hunspell: hunspell.pac hunspell-en.pac
archive-tools: atool.pac unrar.pac unzip.pac zip.pac p7zip.pac
image-tools: imagemagick.pac scrot.pac gimp.pac sxiv.pac


#
# Browsers
#

browsers: chromium firefox opera google-talkplugin.aur

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
redshift: redshift.pac redshift.usr
steam: steam.pac steam-fonts.aur steam-native-runtime.pac

sublime-text: sublime-text-dev.aur
thunderbird: thunderbird.pac
transmission: transmission-gtk.pac
vlc: vlc.pac
weechat: weechat.pac


#
# Development
#

dev: bash cpp git haskell js pdf json dev-tools sublime-text

bash: bash-completion.pac shellcheck.pac
cpp: clang.pac make.pac
git: git.pac tig.pac git-crypt.aur
haskell: ghc.pac stack.pac libtinfo.aur
js: nodejs.pac npm.pac jshint.npm
latex: $(shell pacman -Ssq texlive-most | sed 's/$$/.pac/')
pdf: latex pandoc.pac evince # ppp
json: jshon.pac json.npm
dev-tools: git make.pac the_silver_searcher.pac

pm2: pm2.npm pm2.srv
mysql: mariadb.pac


#
# List targets
#

%.pac:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.aur:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.npm:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.ini:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.cfg:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.fil:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.srv:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
%.usr:
	[[ "$@" =~ $(TYPE)$$ ]] && echo "$@" || true
