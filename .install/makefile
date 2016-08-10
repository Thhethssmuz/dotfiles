MAKEFLAGS += --no-builtin-rules
SHELL      = /bin/bash
USERNAME   = $(shell echo '$(abspath $(lastword $(MAKEFILE_LIST)))' | cut -d'/' -f3)
HOSTNAME  ?= $(shell hostname)
TARGET    ?= status

.PHONY: all install status paclist aurlist npmlist
.PHONY: archbox archtop
.ONESHELL:

# default target
all: status

#
# main targets
#

install:
	@TARGET=install $(MAKE) $(HOSTNAME) --no-print-directory

status:
	@TARGET=status $(MAKE) $(HOSTNAME) --no-print-directory

paclist:
	@TARGET=paclist $(MAKE) $(HOSTNAME) --no-print-directory

aurlist:
	@TARGET=aurlist $(MAKE) $(HOSTNAME) --no-print-directory

npmlist:
	@TARGET=npmlist $(MAKE) $(HOSTNAME) --no-print-directory

cfglist:
	@TARGET=cfglist $(MAKE) $(HOSTNAME) --no-print-directory


#
# host definitions
#

archbox: \
	base \
	nvidia \
	xorg \
	home \
	fonts \
	network-manager \
	xmonad \
	environment \
	lxdm \
	pulseaudio \
	mpd \
	dropbox \
	redshift \
	terminals \
	browsers \
	gtk-theme \
	dev-tools \
	pdf-tools \
	network-tools \
	fun-tools

archtop: \
	base \
	mesa \
	xorg \
	home \
	fonts \
	network-manager \
	xmonad \
	environment \
	smb \
	lxdm \
	pulseaudio \
	mpd \
	dropbox \
	redshift \
	terminals \
	browsers \
	gtk-theme \
	dev-tools \
	pdf-tools \
	network-tools \
	fun-tools

sia-10:
	base \
	nvidia \
	xorg \
	home \
	fonts \
	network-manager \
	xmonad \
	environment \
	smb \
	sshd \
	lxdm \
	pulseaudio \
	mpd \
	terminals \
	browsers \
	gtk-theme \
	dev-tools \
	network-tools \
	fun-tools \
	libreoffice \
	thunderbird


#
# Video driver detection
#

ifeq ($(shell lspci | grep VGA | grep NVIDIA 1>&2>/dev/null; echo $$?),0)
VIDEO-DRIVER = nvidia
else ifeq ($(shell lspci | grep VGA | grep Intel 1>&2>/dev/null; echo $$?),0)
VIDEO-DRIVER = mesa
else
VIDEO-DRIVER = .video-driver-detection-fail
endif

video-drivers: $(DETECTED-VIDEO-DRIVER)
.video-driver-detection-fail:
	@echo -e "   \e[1;31mFailed to detect appropriate video drivers!\e[0m"


#
# Named target bundles and other custom rules
#

base: \
	$(shell pacman -Sgq base | sed 's/$$/.pac/') \
	$(shell pacman -Sgq base-devel | sed 's/$$/.pac/') \
	sudo.pac \
	gnupg.pac \
	openssh.pac \
	git.pac \
	make.pac \
	ntfs-3g.pac

nvidia: \
	nvidia\ nvidia-libgl\ libevdev\ lib32-nvidia-libgl.pac.list \
	nvidia-settings.pac \
	initial-nvidia-xorg.ini

mesa: mesa\ mesa-libgl\ lib32-mesa-libgl.pac.list xf86-video-intel.pac

xorg: \
	xf86-input-evdev.pac \
	xorg-server.pac \
	xorg-xinit.pac \
	xorg-server-utils.pac \
	xorg-xev.pac \
	xorg-xprop.pac \
	xorg-xwininfo.pac

home: xdg-user-dirs.pac
	@case "$(TARGET)" in
		status)
			echo -e "   \e[33mHome directories status is unknown...\e[0m" ;;
		install)
			xdg-user-dirs-update ;;
	esac

fonts: \
	ttf-dejavu.pac \
	ttf-liberation.pac \
	ttf-ubuntu-font-family.pac \
	ttf-ionicons.pac \
	ttf-font-awesome.aur \
	ttf-opensans.aur

network-manager: \
	networkmanager.pac \
	NetworkManager.srv \
	NetworkManager-wait-online.srv

xmonad: \
	xmonad.pac \
	xmonad-contrib.pac

hunspell: hunspell.pac hunspell-en.pac
	@case "$(TARGET)" in
		status)
			echo | hunspell -D 2> >(grep hunspell) >/dev/null | grep nb_NO 1>/dev/null &&
				echo -e "   hunspell-no \e[1;32minstalled\e[0m (custom)" ||
				echo -e "   hunspell-no \e[1;31mnot installed\e[0m (custom)" ;;
		install)
			if echo | hunspell -D 2> >(grep hunspell) >/dev/null | grep nb_NO 1>/dev/null; then
				echo -e "   hunspell-no \e[1;32minstalled\e[0m (custom)"
			else
				echo -e "=> installing hunspell-no..."
				/home/$(USERNAME)/.install/custom/hunspell-no.sh
			fi ;;
	esac

status-icons:
	@case "$(TARGET)" in
		status)
			echo -e "   \e[33mStatus icons status is unknown...\e[0m" ;;
		install)
			sudo -u $(USERNAME) "/home/$(USERNAME)/.xmonad/script/mkicons.sh" ;;
	esac

environment: \
	bash-completion.pac \
	bc.pac \
	calc.pac \
	compton.pac \
	dzen2.pac \
	espeak.pac \
	gimp.pac \
	haskell-async.pac \
	haskell-dbus.pac \
	haskell-stm.pac \
	hsetroot.aur \
	hunspell \
	imagemagick.pac \
	keychain.pac \
	lsb-release.pac \
	numlockx.pac \
	pass.pac \
	scrot.pac \
	slock.pac \
	status-icons \
	unrar.pac \
	unzip.pac \
	vlc.pac \
	wmctrl.pac \
	xautolock.pac \
	xclip.pac \
	xdotool.pac \
	xsel.pac \
	youtube-dl.pac

smb: samba.pac ## some mounts...

sshd: openssh.pac sshd.srv # sshd.cfg (host dependent maybe?)

smbd: samba.pac smbd.srv nmbd.srv # smbd.cfg maybe gvfs.pac?

lxdm: lxdm.pac lxdm.cfg lxdm.srv

pulseaudio: pulseaudio-alsa.pac pavucontrol.pac pulseaudio.usr

mpd: mpd.pac pulseaudio.usr mpd.usr ncmpcpp.pac

dropbox: dropbox.aur dropbox-cli.aur dropbox.usr #selective sync?

redshift: redshift.pac redshift.usr


terminals: xterm.pac rxvt-unicode.pac guake.pac
browsers: chromium.pac firefox.pac
gtk-theme: elementary-icon-theme.pac numix-themes.pac lxapperance.pac


dev-tools: \
	clang.pac \
  ghc.pac \
  git.pac \
  jshint.npm \
  json.npm \
  lsb-release.pac \
  make.pac \
  nodejs.pac \
  npm.pac \
  shellcheck.pac \
  tig.pac

pdf-tools: \
	$(shell pacman -Sgq texlive-most | sed 's/$$/.pac/') \
	pandoc.pac \
	evince.pac # ppp

network-tools: \
  ngrep.pac \
  rsync.pac \
  sshfs.pac \
  tcpdump.pac \
  traceroute.pac \
  wireshark-gtk.pac \
  wget.pac

fun-tools: lolcat.pac zalgolize.npm

#
# Generic rules for Pacman (.pac) targets
#

%.pac:
	@case "$(TARGET)" in
		status)
			pacman -Qn $* 1>/dev/null 2>/dev/null &&
				echo -e "   $* \e[1;32minstalled\e[0m (pacman)" ||
				echo -e "   $* \e[1;31mnot installed\e[0m (pacman)" ;;
		install)
			if pacman -Qn $* 1>/dev/null 2>/dev/null; then
				echo -e "   $* \e[1;32minstalled\e[0m (pacman)"
			else
				echo -e "=> installing $*..."
				pacman -S --noconfirm $*
			fi ;;
		paclist)
			echo "$*" ;;
	esac

%.pac.list:
	@case "$(TARGET)" in
		status)
			pacman -Qn $* 1>/dev/null 2>/dev/null &&
				for x in $*; do echo -e "   $$x \e[1;32minstalled\e[0m (pacman)"; done ||
				for x in $*; do
					pacman -Qn "$$x" 1>/dev/null 2>/dev/null &&
						echo -e "   $$x \e[1;32minstalled\e[0m (pacman)" ||
						echo -e "   $$x \e[1;31mnot installed\e[0m (pacman)"
				done ;;
		install)
			if pacman -Qn $* 1>/dev/null 2>/dev/null; then
				for x in $*; do echo -e "   $$x \e[1;32minstalled\e[0m (pacman)"; done
			else
				echo -e "=> installing $*..."
				pacman -S --noconfirm $*
			fi ;;
		paclist)
			for x in $*; do echo "$$x"; done ;;
	esac


#
# AUR targets
#

apacman.aur: wget.pac curl.pac jshon.pac
	@case "$(TARGET)" in
		status)
			pacman -Qm apacman 1>/dev/null 2>/dev/null &&
				echo -e "   apacman \e[1;32minstalled\e[0m (aur)" ||
				echo -e "   apacman \e[1;31mnot installed\e[0m (aur)" ;;
		install)
			if pacman -Qm $* 1>/dev/null 2>/dev/null; then
				echo -e "   apacman \e[1;32minstalled\e[0m (aur)"
			else
				echo -e "=> installing apacman..."
				cd /home/$(USERNAME)
				su $(USERNAME) -l << EOF
				  cd ~
				  curl -L -O https://aur.archlinux.org/cgit/aur.git/snapshot/apacman.tar.gz
				  tar xvzf apacman.tar.gz
				  cd apacman
				  makepkg
				EOF
				pacman -U --noconfirm apacman/apacman-*.pkg.tar.xz
				rm -rf apacman apacman.tar.gz
			fi ;;
		aurlist)
			echo "apacman" ;;
	esac

%.aur: apacman.aur
	@case "$(TARGET)" in
		status)
			pacman -Qm $* 1>/dev/null 2>/dev/null &&
				echo -e "   $* \e[1;32minstalled\e[0m (aur)" ||
				echo -e "   $* \e[1;31mnot installed\e[0m (aur)" ;;
		install)
			if pacman -Qm $* 1>/dev/null 2>/dev/null; then
				echo -e "   $* \e[1;32minstalled\e[0m (aur)"
			else
				echo -e "=> installing $*..."
				apacman -S --noconfirm $*
			fi ;;
		aurlist)
			echo "$*" ;;
	esac


#
# NPM targets
#

# using 'npm ls' is slow as fuck! so we'll just hack it by checking if the
# directory exist in the global node_modules folder.
%.npm: nodejs.pac npm.pac
	@case "$(TARGET)" in
		status)
			ls /usr/lib/node_modules | egrep '^$*$$' 1>/dev/null 2>/dev/null &&
				echo -e "   $* \e[1;32minstalled\e[0m (npm)" ||
				echo -e "   $* \e[1;31mnot installed\e[0m (npm)" ;;
		install)
			if ls /usr/lib/node_modules | egrep '^$*$$' 1>/dev/null 2>/dev/null; then
				echo -e "   $* \e[1;32minstalled\e[0m (npm)"
			else
				echo -e "=> installing $*..."
				npm install -g $*
			fi ;;
		npmlist)
			echo "$*" ;;
	esac


#
# System services
#

%.srv: systemd.pac
	@case "$(TARGET)" in
		status)
			systemctl is-enabled $*.service 1>/dev/null 2>/dev/null &&
				echo -e "   $* \e[1;32menabled\e[0m (service)" ||
				echo -e "   $* \e[1;31mnot enabled\e[0m (service)" ;;
		install) \
			if systemct is-enabled $*.service 1>/dev/null 2>/dev/null; then
				echo -e "   $* \e[1;32menabled\e[0m (service)"
			else
				echo -e "=> enabling $*..."
				systemctl enable $*.service
			fi ;;
		srvlist)
			echo "$*" ;;
	esac


#
# User services
#

%.usr: systemd.pac
	@case "$(TARGET)" in
		status)
			sudo -u "$(USERNAME)" systemctl --user is-enabled $*.service 1>/dev/null 2>/dev/null &&
				echo -e "   $* \e[1;32menabled\e[0m (user service)" ||
				echo -e "   $* \e[1;31mnot enabled\e[0m (user service)" ;;
		install) \
			if sudo -u "$(USERNAME)" systemct --user is-enabled $*.service 1>/dev/null 2>/dev/null; then
				echo -e "   $* \e[1;32menabled\e[0m (user service)"
			else
				echo -e "=> enabling $*..."
				sudo -u "$(USERNAME)" systemctl --user enable $*.service
			fi ;;
		usrlist)
			echo "$*" ;;
	esac


#
# Configuration files
#

# configuration that is only expected to be initialized, but overwritten later
%.ini:
	@render() { eval "$$(echo -e "cat <<EOF\n$$(tail -n+2 "template/$*.tmpl")\nEOF")"; }
	CONFIG="$$(head -n1 "template/$*.tmpl" | sed 's/^#\s*//')"
	case "$(TARGET)" in
		status)
			[ -e "$$CONFIG" ] &&
				echo -e "   $$CONFIG \e[1;32mok\e[0m (config)" ||
				echo -e "   $$CONFIG \e[1;31mdoes not exist\e[0m (config)" ;;
		install)
			if [ -e "$$CONFIG" ]; then
				echo -e "   $$CONFIG \e[1;32mok\e[0m (config)"
			else
				echo -e "=> creating $$CONFIG..."
				render > "$$CONFIG"
			fi ;;
		cfglist)
			echo "$$CONFIG" ;;
	esac

# configuration that should be synced with the one under version control
%.cfg:
	@render() { eval "$$(echo -e "cat <<EOF\n$$(tail -n+2 "template/$*.tmpl")\nEOF")"; }
	CONFIG="$$(head -n1 "template/$*.tmpl" | sed 's/^#\s*//')"
	case "$(TARGET)" in
		status)
			if [ ! -e "$$CONFIG" ]; then
				echo -e "   $$CONFIG \e[1;31mdoes not exist\e[0m (config)"
			else
				render | diff "$$CONFIG" - 1>/dev/null 2>/dev/null &&
					echo -e "   $$CONFIG \e[1;32mok\e[0m (config)" ||
					echo -e "   $$CONFIG \e[1;31mdiffers\e[0m (config)"
			fi ;;
		install)
			if [ ! -e "$$CONFIG" ]; then
				echo -e "=> creating $$CONFIG..."
			else
				if render | diff "$$CONFIG" - 1>/dev/null 2>/dev/null; then
					echo -e "   $$CONFIG \e[1;32mok\e[0m (config)"
				else
					echo -e "=> updating $$CONFIG..."
					render > "$$CONFIG"
				fi
			fi ;;
		cfglist)
			echo "$$CONFIG" ;;
	esac
