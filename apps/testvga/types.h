/*
 * OZ C types
 */

#ifndef __typesh
#define __typesh

typedef unsigned long long u64;
typedef unsigned short u16;
typedef unsigned char u8;

// humm, u32 is the problem.
// and wow, despite *decades* of discussion, NO ONE has stepped up to fix this!
// look folks, C is often used to compile KERNELS.  kernels are, by definition,
// machine dependent.  let's admit we need machine aware C compilers and hard
// code i8, i16, i32, i64, u8, u16, u32, and u64 into the standard!

// for now YOU must choose the correct implementation,
// pick one of these in your source file
#if MACHINE_WORD_SIZE == 64
typedef unsigned int u32;
#elif MACHINE_WORD_SIZE == 32
typedef unsigned int u32;
#elif MACHINE_WORD_SIZE == 16
typedef unsigned long u32;
#endif

#define CC_VERIFY \
    _Static_assert(sizeof(u64) == 8, "u64 is not 64 bits"); \
    _Static_assert(sizeof(u32) == 4, "u32 is not 32 bits"); \
    _Static_assert(sizeof(u16) == 2, "u16 is not 16 bits"); \
    _Static_assert(sizeof(u8) == 1, "u8 is not 8 bits");

#endif /* __typesh */
