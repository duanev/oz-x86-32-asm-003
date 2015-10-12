/*
 * The header for all OZ processes/apps
 *
 * #include this header at the *end* of a .c file
 */

#include "types.h"
CC_VERIFY

struct ozapp {
    unsigned char magic[5];
    u8  reserved1;
    u8  reserved2;
    u8  reserved3;
    u16 arch;
    u16 subarch;
    u16 res0;
    u16 res1;

#if MACHINE_WORD_SIZE == 32
    u32 end;
    u32 xx0;
    u32 entry;
    u32 xx1;
#else
    u64 end;
    u64 entry;
#endif
};

// from here on its all undocumented ld voodoo.
// don't rearrange ANY of this stuff

void _start(void);
extern void * _end;

__attribute__((section ("hdrsec")))

#define ARCH_X86                15

#define SUBARCH_OZ_PM_4GB_FLAT  31

struct ozapp app_header = { {'o','z','a','p','p'}, 0, 0, 0,
                            ARCH_X86, SUBARCH_OZ_PM_4GB_FLAT, 0, 0,
#if MACHINE_WORD_SIZE == 32
                            (u32)&_end, 0, (u32)&_start, 0
#else
                            (u64)&_end, (u64)&_start
#endif
                            };
void
_start(void)
{
//  asm("    finit;      \n");      // include if using floating point
    asm("    call main;  \n");
    asm("1:  iret;       \n");
    asm("    jmp 1;      \n");
}

