oz-x86-32-asm
=============

OZ, A more utopian OS (that thing that lives down under)

Well, at least utopian was the goal when I begain this project.
Written all in the NASM assembler OZ was intended to be a light weight
microkernel/hypervisor for x86 that booted up fast (like in seconds fast).
Got as far as bringing the cpu cores online, defining an ABI, and handling
a few interrupts ...  But then I moved off in a slightly different direction.

Resurrecting OZ 003 here in 2015 we find that the tools are better (qemu just
rocks!) and my pgdb (look for it in my other github projects) makes NASM
debugging way easier.


tested on: Arch Linux 2015


============ Files ============
    oz.asm      - main
    ozirq.asm   - interrupt handlers and setup
    ozsys.asm   - system calls
    oz_fd       - oz binary for floppy discs (first few sectors)
    oz_fd.img   - oz binary for floppy discs (entire 1.44MB image)
    oz_usb      - oz binary for a 129MB usb stick
    usbptbl.inc - the partition table I used for some usb stick
                  (adjust it if you need)

============ Build / Run ======

Install nasm (http://www.nasm.us) version 2.07 at least,
here in 2015 version 2.11 is common - I didn't go back to test with
previous nasm versions and it seems like subtle syntaxy things have
changed so the minimum version might be larger.

    $ nasm -v
    NASM version 2.11.08 compiled on Mar 24 2015
    $ cd oz-x86-32-asm-003
    $ (cd apps/testvga; make)
    $ (cd apps/testfpu; make)
    $ (cd apps/initsmp; make)
    $ make
    $ qemu-system-i386 -smp 8 -boot a -fda oz_fd -monitor stdio

To exit qemu if it has grabbed the cursor and won't give it up,
try <ctrl><alt>2 (or I think they moved it to <ctrl><alt>3 recently)
to switch to monitor mode and then enter the command 'quit'.

On real hardware there are still a few problems - see the makefile for
selecting a trimmed down set of apps that can work on most.  A Centrino
I have manages to run everything, the i7 can't handle the smp startup.

Or to run qemu with PGDB:

    $ cd oz-x86-32-asm-003
    $ qemu-system-i386 -smp 4 -boot a -fda oz_fd -s -S -monitor stdio
    (in another terminal)
    $ cd pgdb
    $ python pgdb.py -nasmlst ../oz-x86-32-asm-003/oz_fd.lst=0,8 -nasmlst ../oz-x86-32-asm-003/apps/initsmp/initsmp.lst=f:400000 -gccmap ../oz-x86-32-asm-003/apps/testvga/testvga32.map=f -gccmap ../oz-x86-32-asm-003/apps/testfpu/testfpu32.map=f
    (you can press 'j' immediately to jump to 0x7c00 since PGDB
    can tell where the origin of the first .lst file is declared)

Running with 4 cpus is reasonably responsive as PGDB doesn't need to fetch
as much cpu data after every single-step/breakpoint/watchpoint.  The kernel
can handle up to 32 before it triple faults, but can only use 8 simultaneous
threads at the moment.




Feel free to donate:
    email: duanev at gmail
    bitcoin: 1MhLdkqstABtGGTuw1rr66Xytg1nZpAKvk

