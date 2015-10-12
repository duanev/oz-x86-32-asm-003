
# first shot at debugging: pick a minimal set of init apps.
# the first 'apps' runs on almost all the hardware I have,
# the Centrino can run them all, but not an Athelon or i7.
# qemu is apparently more generous with malformed selectors
# and tss structs, and has been running everything for a while,
# at least up to -smp 8

#apps = apps/testvga/testvga32 apps/nullapp/nullapp
#apps = apps/initsmp/initsmp apps/nullapp/nullapp
#apps = apps/initsmp/initsmp apps/testvga/testvga32 apps/nullapp/nullapp
apps = apps/testfpu/testfpu32 apps/initsmp/initsmp apps/testvga/testvga32 apps/nullapp/nullapp

all: oz_fd.img oz_usb

oz_fd.img: oz_fd
	dd if=/dev/zero of=oz_fd.img bs=512 count=`expr 2 \* 80 \* 18`
	dd if=oz_fd of=oz_fd.img conv=notrunc

oz_fd: oz.asm ozirq.asm ozsys.asm _apps makefile
	nasm -DFLOPPY -DAPP_SIZE=`wc -c _apps | cut -d' ' -f1` -l oz_fd.lst -o oz_fd1 oz.asm
	cat oz_fd1 $(apps) > oz_fd
	rm -f oz_fd1

oz_usb: oz.asm ozirq.asm ozsys.asm _apps makefile
	nasm -DUSB -DAPP_SIZE=`wc -c _apps | cut -d' ' -f1` -l oz_usb.lst -o oz_usb1 oz.asm
	cat oz_usb1 $(apps) > oz_usb
	rm -f oz_usb1
	echo "carefully as root: # dd if=oz_usb of=/dev/sdX"

_apps: $(apps) makefile
	cat $(apps) > _apps

clean:
	rm -f oz_fd oz_fd.img oz_usb *.lst _apps

