/* references:
 *   https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/4/html/Using_ld_the_GNU_Linker/sections.html
 *   http://www.math.utah.edu/docs/info/ld_3.html
 *   (ok its for ROM code but ...)  http://hertaville.com/a-sample-linker-script.html
 */

ENTRY (main)

SECTIONS{
    . = 0x400000;

    hdrsec : {
        *(hdrsec)
    }

    .rodata : {
        *(.rodata)
    }

    .text : {
        *(.text)
    }

    . = 0x401000;

    .data : {
        *(.data)
    }

    .bss : {
        sbss = .;
        *(COMMON)
        *(.bss)
        ebss = .;
    }

    endsec : {
        *(endsec)
        /* the thread stacks are using all 4k of the bss
           and aligning endsec for us */
	/* BYTE(0) */
	/* . = ALIGN(0x1000); */
        _end = .;
    }

}

