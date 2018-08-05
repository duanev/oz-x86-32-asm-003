
# note: qemu doesn't have mwait/monitor
#NFLAGS=-DHAVE_MWAIT

SHELL=/bin/bash

# as a first shot at debugging: pick a minimal set of init apps
# (you must always append nullapp to the end of the init apps list)

#apps = apps/testvga/testvga32 apps/nullapp/nullapp
#apps = apps/initsmp/initsmp apps/nullapp/nullapp
#apps = apps/initsmp/initsmp apps/testvga/testvga32 apps/nullapp/nullapp
apps = apps/testfpu/testfpu32 apps/initsmp/initsmp apps/testvga/testvga32 apps/nullapp/nullapp

all: oz.iso oz_usb

oz_fd.img: oz_fd
	dd if=/dev/zero of=oz_fd.img bs=512 count=`expr 2 \* 80 \* 18`
	dd if=oz_fd of=oz_fd.img conv=notrunc

oz_fd: oz.asm ozirq.asm ozsys.asm _apps makefile
	nasm $(NFLAGS) -DFLOPPY -DAPP_SIZE=`wc -c _apps | cut -d' ' -f1` -l oz_fd.lst -o oz_fd1 oz.asm
	@echo alignment is $$(( $$(stat --printf="%s" oz_fd1) & 0xfff )) - should be 1024
	@if [ $$(( $$(stat --printf="%s" oz_fd1) & 0xfff )) -ne 1024 ]; then \
	    echo error: kernel size $$(( 1024 - $$(stat --printf="%s" oz_fd1) & 0xfff )) is not acceptable,; \
	    echo adjust 'interseg' in increments of 32 to fix.; exit 1; \
	else true; fi
	cat oz_fd1 $(apps) > oz_fd
	rm -f oz_fd1
	@echo "---- floppy image available as: oz_fd.img"

oz_usb: oz.asm ozirq.asm ozsys.asm _apps makefile
	nasm $(NFLAGS) -DUSB -DAPP_SIZE=`wc -c _apps | cut -d' ' -f1` -l oz_usb.lst -o oz_usb1 oz.asm
	cat oz_usb1 $(apps) > oz_usb
	rm -f oz_usb1
	@echo "---- usb image available as oz_usb - carefully as root: # dd if=oz_usb of=/dev/sdX"

oz.iso: oz_fd.img
	cp oz_fd.img iso
	genisoimage -V oz32-003 -o oz.iso -b oz_fd.img -hide oz_fd.img iso
	@echo "---- cdrom image available as: oz.iso"

_apps: $(apps) makefile
	cat $(apps) > _apps

clean:
	rm -f oz_fd oz_fd.img oz_usb *.lst _apps

