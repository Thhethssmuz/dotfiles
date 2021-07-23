#!/bin/bash
set -euo pipefail

if ! ping google.com -c 1; then
  echo "you must configure the network before running the firstboot script!"
  exit 1
fi

source user-prompt.sh

# in case something goes wrong
set -x

# localectl
localectl set-locale LANG=en_GB.UTF-8
# localectl set-x11-keymap dvorak pc104 "no(dvorak)"
localectl set-keymap --no-convert dvorak

# timedatectl
timedatectl set-ntp true

# include multilib
sed -i 's/^#\[multilib\]/[multilib]\nInclude = \/etc\/pacman.d\/mirrorlist/' /etc/pacman.conf
pacman -Syy
pacman -Syu

# install sudo and add wheel group to sudoers
pacman -S --noconfirm  sudo
sed -i '/^# %wheel ALL=(ALL) ALL$/ s/^# //' /etc/sudoers
visudo -cf /etc/sudoers

# create new user and set password
useradd -m -g users -G audio,games,rfkill,uucp,video,wheel -s /bin/bash "$USERNAME"
echo "Set user password for ${USERNAME}"
passwd "$USERNAME"

# install ssh and git
pacman -S --noconfirm git openssh

# copy private to home and clean up in root
mv /root/crypton/{.gnupg,.ssh,.password-store} "/home/$USERNAME"
chown -R "$USERNAME":users /home/"$USERNAME"/{.gnupg,.ssh,.password-store}
rm -rf /root/crypton /root/{live,chroot,*-prompt}.sh

# pass ownership of the tty to the user (needed for gpg)
chown "$USERNAME" "$(tty)"

# finalize install as user
su "$USERNAME" -l << EOF
  set -xeuo pipefail
  cd ~

  # start the gpg-agent with curses support
  gpg-agent --daemon --pinentry-program /usr/bin/pinentry-curses

  # SSH Agent -> GPG
  export GPG_TTY="\$(tty)"
  unset SSH_AGENT_PID
  if [ "\${gnupg_SSH_AUTH_SOCK_by:-0}" -ne \$\$ ]; then
    export SSH_AUTH_SOCK="\$(gpgconf --list-dirs agent-ssh-socket)"
  fi

  # decrypt private repos (prompts for a whole lot of passwords...)
  ~/.gnupg/import.sh

  # clone dotfiles
  git clone git@github.com:Thhethssmuz/dotfiles.git tmp
  mv tmp/.git .
  rm -rf tmp
  git reset --hard
  git pull
EOF

# you should now be set to run your normal install scripts
# this should be fully non-interactive, go get some coffee :)
cd "/home/$USERNAME/.install"
make install

# if all goes well reboot into new system
reboot
