MAKEFLAGS += --no-builtin-rules --silent
SHELL      = /bin/bash
MAKEDIR    = $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))

USERNAME    ?= $(shell whoami)
PASSWORD    ?= 1234
MEMORY      ?= 2G
CPU_CORES   ?= 1
CPU_THREADS ?= 2
DISC        ?= 64G
MOUNTS      ?= ''

ifneq ($(shell pwd),$(MAKEDIR))
$(error bad execution directory)
endif

.PHONY: help
.PRECIOUS: data data/%-vm.iso data/%-vm.qcow2 data/%-vm.fd
.ONESHELL:

help:
	echo 'Usage: [<var>=<value>] make <vm-name>'
	echo ''
	echo 'VARIABLES     DEFAULTS'
	echo '  USERNAME    $(shell whoami)'
	echo '  PASSWORD    1234'
	echo '  MEMORY      2G'
	echo '  CPU_CORES   1'
	echo '  CPU_THREADS 2'
	echo '  DISC        64G'
	echo '  MOUNT       '"''"
	echo ''
	echo 'EXAMPLE:'
	echo '  MEMORY=4G CPU_CORES=4 MOUNT=/path/to/mount make my-new-vm'

%-vm: | data/%-vm.iso data/%-vm.qcow2 data/%-vm.fd
	echo '#!/usr/bin/bash' > '$*-vm'
	echo '' >> '$*-vm'
	echo 'ARGS=( \' >> '$*-vm'
	echo '  qemu-system-x86_64 \' >> '$*-vm'
	echo '    -name "$*-vm" \' >> '$*-vm'
	echo '    -display sdl,gl=off \' >> '$*-vm'
	echo '    -vga virtio \' >> '$*-vm'
	echo '    -boot "once=d" \' >> '$*-vm'
	echo '    -m "$(MEMORY)" \' >> '$*-vm'
	echo '    -cpu host \' >> '$*-vm'
	echo "    -smp \"$$(($(CPU_CORES) * $(CPU_THREADS))),sockets=1,cores=$(CPU_CORES),threads=$(CPU_THREADS)\" \\" >> '$*-vm'
	echo '    -enable-kvm \' >> '$*-vm'
	echo '    -machine "type=pc,accel=kvm" \' >> '$*-vm'
	echo '    -drive "file=$(MAKEDIR)/data/$*-vm.qcow2,if=virtio,cache=writeback,discard=ignore,format=qcow2" \' >> '$*-vm'
	echo '    -drive "if=pflash,format=raw,readonly,file=/usr/share/ovmf/x64/OVMF_CODE.fd" \' >> '$*-vm'
	echo '    -drive "if=pflash,format=raw,file=$(MAKEDIR)/data/$*-vm.fd" \' >> '$*-vm'
	echo '    -net nic \' >> '$*-vm'
	echo '    -net user \' >> '$*-vm'
	echo ')' >> '$*-vm'
	echo '' >> '$*-vm'
	echo 'if [ -f "$(MAKEDIR)/data/$*-vm.iso" ]; then' >> '$*-vm'
	echo '  ARGS+=(-cdrom "$(MAKEDIR)/data/$*-vm.iso")' >> '$*-vm'
	echo 'else' >> '$*-vm'
	echo '  :' >> '$*-vm'
	for mount in $(MOUNT); do
		if [ -n "$$mount" ]; then
			echo '  ARGS+=(-virtfs "local,id='"$$(basename "$$mount")"',path='"$$mount"',mount_tag='"$$(basename "$$mount")"',security_model=mapped")' >> '$*-vm'
		fi
	done
	echo 'fi' >> '$*-vm'
	echo '' >> '$*-vm'
	echo '"$${ARGS[@]}"' >> '$*-vm'

	chmod +x '$*-vm'

data:
	mkdir -p data

data/%-vm.iso: | data
	DIR="$$(mktemp -d /tmp/archlive.XXXXXXXX)"
	cp -r /usr/share/archiso/configs/releng/* "$$DIR"

	# copy install scripts to live image
	mkdir -p "$$DIR"/airootfs/etc/skel
	cp ~/.install/vm/* "$$DIR"/airootfs/etc/skel/

	# inject install script variables
	sed -i "s/\\\$$HOSTNAME/$*-vm/g" "$$DIR"/airootfs/etc/skel/chroot.sh
	sed -i "s/\\\$$USERNAME/$(USERNAME)/g" "$$DIR"/airootfs/etc/skel/firstboot.sh
	sed -i "s/\\\$$PASSWORD/$(PASSWORD)/g" "$$DIR"/airootfs/etc/skel/firstboot.sh

	# automatically source the live script when starting up the live image
	sed -i 's/zsh/bash/' "$$DIR"/airootfs/root/customize_airootfs.sh
	echo 'echo "bash live.sh" >> /root/.bashrc' \
		>> "$$DIR"/airootfs/root/customize_airootfs.sh

	# build the image
	mkdir -p "$$DIR"/out/
	( cd "$$DIR" && sudo ./build.sh -v )

	# copy out and clean up
	mv "$$DIR"/out/archlinux-*-x86_64.iso "data/$*-vm.iso"
	sudo rm -rf "$$DIR"

data/%-vm.qcow2: | data
	qemu-img create -f qcow2 "data/$*-vm.qcow2" "$(DISC)"

data/%-vm.fd: | data
	cp /usr/share/ovmf/x64/OVMF_VARS.fd "data/$*-vm.fd"
