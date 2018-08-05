oz-x86-32-asm
=============

OZ, A more utopian OS (that thing that lives down under)

Well, at least utopian was the goal when I begain this project.
Written all in the NASM assembler OZ was intended to be a light weight
microkernel/hypervisor for x86 that booted up fast (like in seconds fast).
Got as far as bringing the cpu cores online, defining an ABI, and handling
a few interrupts ... but then I moved off in a different direction.

Resurrecting OZ 003 here in 2015 we find that the tools are better (qemu
just rocks!, and my pgdb (look for it in my other github projects) makes
NASM debugging way easier), and adding hardware thread support was fun.

tested recently on: Arch Linux ~2018


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

Sooo, a bunch of changes to qemu since v2.4.50 have broken pgdb for
recent qemu versions (the gdb rdb api was in flux and people are fixing
them - but not the proper way yet, so pgdb requires 2.4.50)

To exit qemu if it has grabbed the cursor and won't give it up,
try *ctrl-alt*2 (or I think they moved it to *ctrl-alt*3 recently)
to switch to monitor mode and then enter the command 'quit'.

Or to run qemu with PGDB:

    $ cd oz-x86-32-asm-003
    $ qemu-system-i386 -smp 4 -boot a -fda oz_fd -s -S -monitor stdio
    (in another terminal)
    $ cd pgdb
    $ python pgdb.py -nasmlst ../oz-x86-32-asm-003/oz_fd.lst=0,8 -nasmlst ../oz-x86-32-asm-003/apps/initsmp/initsmp.lst=3b -gccmap ../oz-x86-32-asm-003/apps/testvga/testvga32.map=3b -gccmap ../oz-x86-32-asm-003/apps/testfpu/testfpu32.map=3b
    (you can press 'j' immediately to jump to 0x7c00 since PGDB
    can tell where the origin of the first .lst file is declared.
    qemu will say 'Booting from Floppy...' and pause)

PGDB primer (assumes your have built Oz with testfpu,initsmp,testvga):
    - press '/' to search, then 'tasksel'<cr>, then 'n' to skip to the call tasksel_u00 (this is the first app)
    - press home to reposition the listing
    - press 'j' to jump to the call tasksel_u00 ('j' goes until the white highlighted instruction executes)
    - press 's' to step into the testfpu app (pgdb window will swap to the testfpga source)
    - press 'j' to skip the finit (or 's' repeatedly to step through the kernel initialization of the fpu)
    - press 's' to step into the call to testfpu.c:main()
    - press 's' a few times until you step into sum()
    - press 's' twice more to see edi get loaded (the array of floats we will sum)
    - press 'm' then 'edi'<cr> to display the array (the memory pointed to by edi)
    (unfortunately the floating point registers can't be displayed because the gdb rdp api doesn't define them)
    - press 's' until returning to main()
    - press 'j' to jump over the print_double() function (notice the first 16 bytes of array get changed)
    - press 's' through the iret ending the testfpu app
    - press up-arrow until the call tasksel_u00 is white, then 'j' to jump to the call to the next app
    - press 'j' again to skip the app or 's' to step into the app


There is a youtube video about pgdb in the pgdb git repo.

Running with 4 cpus lets PGDB remain reasonably responsive as the fetch
of cpu data after every single-step/breakpoint/watchpoint is smaller.
The oz kernel can handle up to 70 or so before qemu starts acting weird...

Note that Oz is a multi-threaded kernel, but not a multi-tasking, lol,
there is exactly one init app running at a time.

It's fun to make the cpu0 blip go faster by mashing the keyboard ^^

Pressing the DEL key during oz will reboot oz.


============ Bugs etc. ========

All fixed here in 2018!  :)



Feel free to donate:

    email: duanev at gmail
    btc: 36sPuVeeXieeks4uYc94qo5iw6GANekjVn
    eth: 0x2628862F8D0e89783b477ED503468Ebb7cA64763
    ltc: MMpTJyMirfnnpv6BWnBj8qLyNSBLkvk1Td

