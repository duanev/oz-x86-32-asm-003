/*
 * write about the vga device to prove things are working
 */

#include "types.h"

long
syscall3(int op, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
    long ret;
    asm volatile(
        "int $0xff"
        : "=a" (ret)
        : "0" (op), "d" (arg1), "c" (arg2), "b" (arg3)
        : "cc", "edi", "esi", "memory");
    return ret;
}

long
syscall2(int op, unsigned int arg1, unsigned int arg2)
{
    long ret;
    asm volatile(
        "int $0xff"
        : "=a" (ret)
        : "0" (op), "d" (arg1), "c" (arg2)
        : "cc", "ebx", "edi", "esi", "memory");
    return ret;
}

long
syscall1(int op, unsigned int arg1)
{
    long ret;
    asm volatile(
        "int $0xff"
        : "=a" (ret)
        : "0" (op), "d" (arg1)
        : "cc", "ebx", "edi", "esi", "memory");
    return ret;
}

long
syscall0(int op)
{
    long ret;
    asm volatile(
        "int $0xff"
        : "=a" (ret)
        : "0" (op)
        : "cc", "ebx", "edi", "esi", "memory");
    return ret;
}

void
sleep(int ticks)
{
    //  0x2000 = wait for n timer interrupts
    syscall1(0x2000, ticks);
}

void
pause(void)
{
    //  0x2001 pause : wait for an ipi
    syscall0(0x2001);
}

void
resume(void)
{
    //  0x2002 = resume all cores via ipi
    syscall0(0x2002);
}

void
start_threads(void)
{
    //  0x2003 = ipi all cores
    syscall0(0x2003);
}

#define MAX_THREADS 32
#define STACK_SIZE  256

// a nonzero initializer here pushes 'stack' out of common
// and into .bss where the array is actually initialized.
// (the BYTE(0) in testvga.ld was commented out so we can
// use all 4096 bytes of the array for stack without
// disrupting the app's 8k footprint)
unsigned char stack[MAX_THREADS][STACK_SIZE] = {1};

int
new_thread(void (* func)(int), int thno)
{
    unsigned char * stackend = stack[MAX_THREADS - thno];

    if (thno == 0)
        return -1;

    // push thno onto the new thread's stack
    stackend -= sizeof(thno);
    *((int *)stackend) = thno;
    stackend -= sizeof(void *);     // account for a return ip

    //    0x2100 = new thread
    //      func = function address
    //  stackend = address of the end of a thread specific stack
    //      thno = thread number
    return syscall3(0x2100, (unsigned int)func, (unsigned int)stackend, thno);
}

#define VIDEORAM    ((short *)0xb8000)

void
map_vga_memory()
{
    //   0x2700 = request mem access
    // videoram = base physical address
    //   0x2000 = length in bytes
    syscall2(0x2700, (unsigned int)VIDEORAM, 0x2000);
}

#define TOPLINE 6           // don't write above this line
#define WIDTH   80
#define HEIGHT  (25-TOPLINE)

#define BLACK   0
#define GRAY    8
#define BLUE    9
#define GREEN   10
#define CYAN    11
#define RED     12
#define MAGENTA 13
#define YELLOW  14
#define WHITE   15

void
pokech(int row, int column, int ch, int color)
{
    VIDEORAM[WIDTH * (TOPLINE + row) + column] = (color << 8) | ch;
    //__sync_synchronize();
    //asm volatile ("sfence" ::: "memory");
}

int
peekch(int row, int column)
{
    return VIDEORAM[WIDTH * (TOPLINE + row) + column];
}

void
pokehstr(int row, int column, const char * str, int color)
{
    const char * p = str;
    int i = 0;

    while (*p) {
        pokech(row, column+i, *p, color);
        i++;
        p++;
    }
}

int
pokehint(int row, int column, int n, int color)
{
    int i = 1;
    if (n > 99) {
        pokech(row, column++, '0' + n/100, YELLOW);
        n %= 100;
        i++;
    }
    if (n > 9) {
        pokech(row, column++, '0' + n/10, YELLOW);
        n %= 10;
        i++;
    }
    pokech(row, column++, '0' + n, YELLOW);
    return i;
}

void
pokevstr(int row, int column, const char * str, int color)
{
    const char * p = str;
    int i = 0;

    while (*p) {
        pokech(row+i, column, *p, color);
        i++;
        p++;
    }
}

void
box(int toprow, int leftcolumn, int bottomrow, int rightcolumn, int color)
{
    int saved_rightcolumn = rightcolumn;

    pokech(bottomrow, rightcolumn, 217, color);
    pokech(toprow,    rightcolumn, 191, color);
    pokech(bottomrow, leftcolumn,  192, color);
    pokech(toprow,    leftcolumn,  218, color);
    while (--rightcolumn > leftcolumn) {
        pokech(bottomrow, rightcolumn, 196, color);
        pokech(toprow,    rightcolumn, 196, color);
    }
    rightcolumn = saved_rightcolumn;
    while (--bottomrow > toprow) {
        pokech(bottomrow, leftcolumn,  179, color);
        pokech(bottomrow, rightcolumn, 179, color);
    }
}


/*
 * check for a horizontal collision
 *
 * returns an unblocked direction, or 0 if we can't move,
 * also changes the input arg 'dir' if there is a collision
 * in the primary direction.
 */
int
hcollision(int row, int startcolumn, int * dirp, int len)
{
    int clear_to_left  = ((peekch(row, startcolumn-1) & 0xff) == ' ');
    int clear_to_right = ((peekch(row, startcolumn+len-1) & 0xff) == ' ');

    // are we good to continue?
    if ((*dirp > 0  &&  clear_to_right) || (*dirp < 0  &&  clear_to_left))
        return *dirp;
    // else try the opposite direction
    *dirp = -(*dirp);
    if ((*dirp > 0  &&  clear_to_right) || (*dirp < 0  &&  clear_to_left))
        return *dirp;
    // completely blocked
    return 0;
}

/*
 * check for a vertical collision
 */
int
vcollision(int startrow, int column, int * dirp, int len)
{
    int clear_above = ((peekch(startrow-1, column) & 0xff) == ' ');
    int clear_below = ((peekch(startrow+len-1, column) & 0xff) == ' ');

    // are we good to continue?
    if ((*dirp > 0  &&  clear_below) || (*dirp < 0  &&  clear_above))
        return *dirp;
    // else try the opposite direction
    *dirp = -(*dirp);
    if ((*dirp > 0  &&  clear_below) || (*dirp < 0  &&  clear_above))
        return *dirp;
    // completely blocked
    return 0;
}

const char hblip[] = "-oOo-";
const char vblip[] = "|X|";

#define memcpy __builtin_memcpy

void
hthread(int thno)
{
    char blip[8];
    int row = thno / 2 + 2;
    int xpos = 1;
    int xdir = 1;
    int mdir;

    memcpy(blip, hblip, sizeof(hblip)+1);   // copy hblip to the local stack
    blip[2] = '0' + thno;                   // make our blip unique

    while (1) {
        // write new blip
        pokehstr(row, xpos, blip, CYAN);

        if (thno == 0) {
            sleep(1);                       // the control threads sleeps
            resume();
        } else {
            pause();                        // other threads wait
        }

        mdir = hcollision(row, xpos, &xdir, sizeof(hblip));

        // blank part of old blip
        if (mdir > 0)
            pokech(row, xpos, ' ', BLACK);
        else if (mdir < 0)
            pokech(row, xpos+sizeof(hblip)-2, ' ', BLACK);

        xpos += mdir;
    }
}

void
vthread(int thno)
{
    char blip[8];
    int column = thno * 2 + 20;
    int ypos = 1;
    int ydir = 1;
    int mdir;

    memcpy(blip, vblip, sizeof(vblip)+1);   // copy vblip to the local stack
    blip[1] = '0' + thno;                   // make our blip unique

    while (1) {
        // write new blip
        pokevstr(ypos, column, blip, CYAN);
        pause();

        mdir = vcollision(ypos, column, &ydir, sizeof(vblip));

        // blank part of old blip
        if (mdir > 0)
            pokech(ypos, column, ' ', BLACK);
        else if (mdir < 0)
            pokech(ypos+sizeof(vblip)-2, column, ' ', BLACK);

        ypos += mdir;
    }
}

int
main(void)
{
    int i;
    int ncpus;

    map_vga_memory();

    ncpus = syscall0(0x1000);       //  0x1000 = ncpus

    if (ncpus > MAX_THREADS)
        ncpus = MAX_THREADS;

    // create boundaries for blips
    box(0, 0, HEIGHT-1, WIDTH-1, GRAY);

    for (i = 1; i < ncpus; i++) {
        if (i & 1)
            new_thread(vthread, i);
        else
            new_thread(hthread, i);
    }
    start_threads();                // currently an ipi is needed to start threads

    pokehstr(1, 1, "using", YELLOW);
    pokehint(1, 7, i, YELLOW);
    pokehstr(1, 10, "of", YELLOW);
    i = 14;
    i += pokehint(1, 13, ncpus, YELLOW);
    pokehstr(1, i, "cpus", YELLOW);

    // main is thread 0
    hthread(0);

    // no return
}

#include "ozapp.h"

