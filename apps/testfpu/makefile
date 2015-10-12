
CFLAGS64 = -m64 -DMACHINE_WORD_SIZE=64
CFLAGS32 = -m32 -DMACHINE_WORD_SIZE=32
CFLAGS = -O2

# gcc's -g debug option causes objdump -S to include
# source code that is incorrect

OPTS = -Wall -Wextra -Werror


all: testfpu32

testfpu32: testfpu.c ozapp.h testfpu.ld sum32.o makefile
	gcc $(CFLAGS32) $(CFLAGS) $(OPTS) -o testfpu32.o -c testfpu.c
	objdump -S testfpu32.o > testfpu32.lst
	ld -m elf_i386 --oformat binary -T testfpu.ld -Map testfpu32.map -o testfpu32 testfpu32.o sum32.o

 # $(OPTS2) $(OPTS3) $(OPTS32)

sum32.o: sum.asm
	nasm -felf32 sum.asm -o sum32.o -l sum32.lst

clean:
	-rm -f *.o *.lst testfpu32
