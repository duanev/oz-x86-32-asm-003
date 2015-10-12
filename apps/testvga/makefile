
CFLAGS64 = -m64 -DMACHINE_WORD_SIZE=64
CFLAGS32 = -m32 -DMACHINE_WORD_SIZE=32
# use -O0 if debugging with the .lst file
# (-O2 rearranges things pretty seriously for optimization)
CFLAGS = -g -O0

OPTS = -Wall -Wextra -Werror

all: testvga32 testvga32.lst

testvga32: testvga.c ozapp.h testvga.ld
	gcc $(CFLAGS32) $(CFLAGS) -o testvga32.o -c testvga.c
	ld -m elf_i386 --oformat binary -T testvga.ld -Map testvga32.map -o testvga32 testvga32.o

testvga32.lst: testvga32.o
	objdump -S -m i386:intel testvga32.o > testvga32.lst

clean:
	-rm -f *.o *.lst testvga32
