; OZ - A more utopian OS   x86-32 startup
; ex: set expandtab softtabstop=4 shiftwidth=4 nowrap :
;
; Copyright (C) 2015-2018  Duane Voth
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU Affero General Public License as
;   published by the Free Software Foundation, either version 3 of the
;   License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU Affero General Public License for more details.
;
;   You should have received a copy of the GNU Affero General Public License
;   along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>
;
;
;
; usage:
;	$ qemu-system-i386 -boot a -fda oz_fd -monitor stdio
;
; requires: nasm-2.07  or later from: http://www.nasm.us
;
; credits:
;       many thanks to the folks at wiki.osdev.org who archive great info.
;
; contributors:
;        djv - Duane Voth
;
; history:
; 2007/03/03 - 0.00.01 - djv - begin with various web examples
;                      http://linuxgazette.net/issue82/misc/raghu/code.asm.txt
;                      http://www.osdever.net/tutorials/brunmar/simple_asm.txt
; 2007/03/04 - 0.00.02 - djv - add timer interrupt support with stray int dbg
; 2007/03/05 - 0.00.03 - djv - remove stray int dbg, add mbr data struc back
; 2007/03/11 - 0.00.04 - djv - debug USB boot problem
; 2007/03/28 - 0.01.00 - djv - add a real-mode stage2 loader in the mbr to
;                              load the sectors past the mbr on the boot
;                              device into the memory behind the mbr.  so even
;                              tho the read from the device happens in two
;                              parts, memory contains a linear image of the
;                              first few sectors of the boot device.
; 2009/02/01 - 0.02.00 - djv - begin to define application and dynamic library
;                              binary formats with headers detailing size and
;                              entry points.
; 2009/08/01 - 0.03.00 - djv - create the OZ app execution container using
;                              TSSes, paging, and rings 0 and 3.  begin to
;                              play with system calls.
; 2015/10/26 - 0.03.01 - djv - cleanup, add smp usermode tss structs, sleep,
;                              wakeup, and ipi for user thread creation.
; 2017/08/05 - 0.03.02 - djv - fixed smp races, restructured thread creation,
;                              handle 128 cores, move stacks to safer places

%ifdef USB
[map symbols oz_usb.map]
%else
[map symbols oz_fd.map]
%endif

; -------- stage 1 ---------------------------------------------------------
; A classic x86 Master Boot Record

section .text start=0x7c00  ; PC BIOS boot loader entry point
textstart :

bios_entry :
    cli
    jmp 0:load_stage2       ; load cs, skip over mbr data struct

times 6-($-$$)  db 0
oemid db "oz"

times 11-($-$$)  db 0

; compute the size of the kernel image in 512 byte sectors
total_size equ (kernel_text_size + kernel_data_size)
kisectors  equ (total_size + 512)/512 + (APP_SIZE + 512)/512
; compute the end of the kernel image (with apps attached)
kilast equ 0x7c00 + kisectors * 512

; MS MBR  (http://support.microsoft.com/kb/140418)
%ifdef FLOPPY
    dw 512                  ; Bytes per sector
    db 1                    ; Sectors per cluster
    dw kisectors            ; Number of reserved sectors
    db 2                    ; Number of FATs
    dw 0x00e0               ; Number of dirs in root
    dw 0x0b40               ; Number of sectors in volume
    db 0xf0                 ; Media descriptor
    dw 9                    ; Number of sectors per FAT
    dw 18                   ; Number of sectors per track
    dw 2                    ; Number of heads
    dd 0                    ; Number of hidden sectors
    dd 0                    ; Large Sectors
%endif

%ifdef USB
    dw 0                    ; Bytes per sector
    db 0                    ; Sectors per cluster
    dw kisectors            ; Number of reserved sectors
    db 0                    ; Number of FATs
    dw 0                    ; Number of dirs in root
    dw 0                    ; Number of sectors in volume
    db 0                    ; Media descriptor
    dw 0                    ; Number of sectors per FAT
    dw 0                    ; Number of sectors per track
    dw 0                    ; Number of heads
    dd 0                    ; Number of hidden sectors
    dd 0                    ; Large Sectors
%endif

; -------- stage 2 loader ------------
bits 16
alignb 2

load_stage2 :
    mov  ax,kstack_loc0+kstack_size0
    mov  sp,ax
    xor  ax,ax
    mov  ss,ax
    mov  es,ax
    mov  ds,ax
    mov  fs,ax
    mov  gs,ax
    cld

    ; debug - pattern the stack so we can see what gets used
    mov  eax,0x11111111
    mov  di,kstack_loc0
    mov  cx,kstack_size0/4
    rep stosd

    push dx                 ; save BIOS boot drive number

    mov  ax,0x0600          ; ah=06h : scroll window up, if al = 0 clrscr
    mov  cx,0x0000          ; clear window from 0,0 
    mov  dx,0x174f          ; to 23,79
    mov  bh,0xf             ; fill with hi white
    int  0x10               ; clear screen for direct writes to video memory

    mov  si,bootmsg
    xor  bx,bx
    call puts_vga_rm
                            ; puts_vga_rm leaves gs pointing at video mem
    mov  byte [gs:1],0xE    ; turn the first two chars yellow
    mov  byte [gs:3],0xE

    ;F - white              
    ;E - yellow             
    ;D - magenta            
    ;C - red                
    ;B - cyan               
    ;A - green              
    ;9 - blue               
    ;8 - dark grey          

    ; -------- stage2 boot loader --------

;   mov  ax,[8400h]         ; was everything loaded? (crdoms only do 3 sectors?)
;   cmp  ax,0               ; FIXME  hax hax hax ...
;   jnz  stage2_present

    ; omg, chs was such a disaster ... floppys give 64k dma errors ...
    ; is there any One Way to Read Them All?  So the 64k DMA issue
    ; forces us to read one sector at a time...

    pop  dx                 ; recover boot drive number
    xor  dh,dh              ; extend it to 32b
    push dx
    xor  di,di
    push di
    pop  es
    mov  ax,800h            ; get CHS parms for the boot drive
    int  13h
    and  cx,3fh             ; ignore cylinder count
    inc  dh                 ; we need number of heads
    mov  ch,dh
    pop  dx                 ; recover boot drive again
    push cx                 ; save N heads & N sectors on the stack

    xor  dh,dh              ; starting head
    push dx
    mov  cx,1h              ; starting cyl & sector (0 based) skip the mbr
    push cx
    mov  bx,stage2          ; load address
    shr  bx,4               ; in segment form
    push bx
    mov  ax,kisectors       ; number of sectors to read
    push ax
    mov  ebp,esp

        ; stack is now
        ;    boot drive number
        ;    N heads & N sectors for this drive
        ;    next head
        ;    next cyl & sector
        ;    load address (segment)
        ; bp number of sectors to read

stage2_ldr :
        ;    bx = load address segment
        ;    cl = starting sector on this cyl & head
        ;    ch = cylinder number
        ;    dl = drive number
        ;    dh = head number

    push bx
    pop  es
    xor  bx,bx
    inc  cl                 ; silly BIOS function uses 1 based sectors ...
    mov  ax,0201h           ; one sector at a time
    int  13h
    jc   ioerr

    pop  cx                 ; remaining sectors
    sub  cx,ax              ; subtract what we just read

    pop  bx
    shl  ax,(9-4)           ; * 512  but its in segment form
    add  bx,ax              ; compute next load address

    xchg ax,cx

    pop  cx
    pop  dx
    push bx                 ; need a reg for calc
    mov  bx,[ebp+8]         ; last head & sector
    inc  cl
    cmp  cl,bl
    jb   stage2_next
    xor  cl,cl              ; sector to zero
    inc  dh                 ; next head
    cmp  dh,bh
    jb   stage2_next
    xor  dh,dh              ; head to zero
    inc  ch                 ; next cyl
stage2_next :
    pop  bx                 ; restore addr, don't disturb flags
    cmp  ax,0
    jbe  stage2_done
    push dx
    push cx
    push bx
    push ax
    jmp  stage2_ldr

stage2_done :

    ; ---- make sure second stage actually got loaded

    mov  ax,[stage2]        ; check the signature byte
    add  ax,[stage2+2]
    cmp  ax,0x7a6f+0x32
    jnz  s2err
stage2_present :
    jmp  0:start_stage2     ; use an absolute jump so stage 1
                            ; can be position independent

ioerr :                     ; ah has status...
    mov  si,ioerrmsg
    jmp  print_err

s2err :
    mov  si,s2errmsg
print_err :
    mov  bx,160
    call puts_vga_rm
    mov  byte [gs:1],0xC    ; turn the first two vga chars red
    mov  byte [gs:3],0xC

hang :
    hlt
    jmp  hang

; ----------------------------
;   puts_vga_rm - write a null delimited string to the VGA controller
;                 in real mode
;
;    enter:
;            esi - address of string
;            ebx - screen location (2 bytes per char, 160 bytes per line)
;    exit:
;            eax - destroyed
;             gs - set to video memory selector

puts_vga_rm :
    mov  ax,0xb800      ; point gs at video memory
    mov  gs,ax
puts_vga_rm_loop :
    lodsb
    cmp  al,0
    jz   puts_vga_rm_done
    mov  [gs:bx],al
    add  ebx,2
    jmp  puts_vga_rm_loop
puts_vga_rm_done :
    ret

bootmsg     db      "OZ v0.03.02 - 2017/08/22 ",0
s2errmsg    db      "stage 2 load failure ",0
ioerrmsg    db      "i/o error loading stage 2 ",0

times 446-($-$$) db 0       ; fill with zeros up to partition table

; If the kernel is loaded from a disk (including usb) the MBR
; must include a partition table based on the device geometry.
; This gets fed into oz.asm here via usbptbl.inc
%ifdef USB
%include "usbptbl.inc"
%else
    ; If the kernel is placed in memory by some other means
    ; (PXE, pxelinux, etc.) the partition table is irrelevant
    ; but the space is still required.  Make the default ptbl
    ; match a 1.44MB floppy.
    db 0x80,0x01,0x01,0x00,0x06,0x01,0x12,0x4f
    db 0x12,0x00,0x00,0x00,0x2e,0x0b,0x00,0x00
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
%endif

%ifdef NEWUSB
new usb?  gpt correct?
0000 01B0: 00 00 00 00 00 2C 44 63  E0 36 03 00 00 00 80 20  .....,Dc .6.....
0000 01C0: 21 00 83 9D 11 4C 00 08  00 00 00 C0 12 00 00 9D  !....L.. ........
0000 01D0: 12 4C 83 57 25 F2 00 C8  12 00 00 A0 28 00 00 00  .L.W%... ....(...
0000 01E0: 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ........ ........
0000 01F0: 00 00 00 00 00 00 00 00  00 00 00 00 00 00 55 AA  ........ ......U.
%endif


times 510-($-$$) db 0       ; fill with zeros up to MBR signature

            dw 0x0aa55      ; write aa55 in bytes 511,512 to indicate
                            ; that it is a boot sector. 

; end of MBR
;
; -------- stage 2 ---------------------------------------------------------
;
; If a boot loader only loads 512 bytes in the first pass, the following
; code won't appear in memory until the code above has completed.  Other
; loaders however can load the entire OZ kernel image into memory in one
; shot. (pxe)

bits 16

stage2 :
    db "oz2",0                      ; stage2 signature

non_boot_cpu_ljmp_instruction :     ; place this in 16 bit code land
    jmp word 0:0                    ; so we get the right opcode

; adjust this if you want to change the supported number of cpus
max_ncpus      equ 128

; adjust this to change the size of the initial boot stacks for each cpu
kstack_size_percpu_l2 equ 8     ; log2 stack size for each cpu (256B)

; boot stacks for all the cpus follow the kernel image
kistacks equ ((kilast+0x1000) & 0xfffff000)    ; align to 4k
; pages for memory allocation follow the boot stacks
first_free_page equ ((kistacks + max_ncpus * (1 << kstack_size_percpu_l2)) >> 12)

kstack_size equ ((first_free_page << 12) - kistacks)
kstack_loc  equ (kistacks)

; adjust these if you want to move other things around
kstack_loc0 equ 0x1000      ; base for boot loader stack
kstack_size0 equ (1 << 13)
tss_f08_stk equ 0x6000      ; stack for double fault (grows down from ...)
tss_f10_stk equ 0x7000      ; stack for tss fault (grows down from ...)
sipi_vector equ 0x7000      ; where the non-boot cpus will start

; ---------------------

start_stage2 :

    ; ---- initialize the 8259's while in real mode

    call irq_init_hardware

; ------------ main kernel entry point ------------
; all cpus enter here
main :
    cli                     ; appears to stabilize recent machines a bit

    ; ---- enable processor features?

    mov  eax,1
    cpuid
    and  edx,20h            ; is rdmsr supported
    jz   no_msr
    ; arch/x86/include/uapi/asm/msr-index.h

%ifdef SETMSR
    ; don't need this, mwait is on by default
    mov  ecx,1a0h           ; ia32_misc
    rdmsr
    or   eax,(1<<18)
    wrmsr                   ; attemt to enable monitor/mwait
%endif

no_msr :

    ; -------- enter protected mode --------

    lgdt [gdtr]             ; initialize the gdt
    mov  eax,cr0
    or   al,0x21            ; set the protected mode bit (lsb of cr0)
    mov  cr0,eax            ;   and enable the native FPU exceptions ...
    jmp  codesel:flush_ip1  ; flush the cpu instruction pipeline
flush_ip1:
bits 32                     ; instructions after this point are 32bit

    mov  ax,datasel
    mov  ds,ax              ; initialize the data segments
    mov  es,ax

    mov  eax,1
    lock xadd [ncpus],eax   ; create our unique cpu number
                            ; could use the lapic id if available
    mov  esi,eax
    mov  ax,stacksel        ; setup a restricted stack segment
    mov  ss,ax
    mov  esp,kstack_size    ; start at the top of the reserved stack space
    mov  eax,esi
    shl  eax,kstack_size_percpu_l2
    sub  esp,eax

    ; ---- add a marker on the vga

    mov  eax,esi
    push eax                ; save cpu index
    mov  ebx,eax
    mov  eax,160            ; vga line length
    shr  ebx,5              ; 32 cpus per vga line
    inc  ebx
    mul  bl
    mov  edi,eax
    sub  edi,2              ; last chracter on first line of vga
    pop  eax
    push eax
    mov  ebx,eax
    add  bl,'0'             ; boot cpu announces via ascii 0
    shl  eax,1
    and  eax,3fh
    sub  edi,eax
    mov  ax,videosel        ; point gs at video memory
    mov  gs,ax
    mov  [gs:edi],bl        ; announce cpu presence
    pop  eax

    or   eax,eax            ; are we the boot cpu?
    jg   non_boot_init      ; if not, do non_boot_init

    ; ---- establish a "pool" of free pyhsical memory

    mov  eax,first_free_page
    mov  [next_free_page],eax

    ; ---- setup the paging tables

    call mem_alloc_kernel_page  ; get a page for pgdir
    shl  eax,12                 ; convert pgno to physical addr
    mov  edi,eax
    mov  [pgdirp],eax
    mov  [tss_f08_cr3],eax
    mov  [tss_f10_cr3],eax
    mov  [tss0_cr3],eax
    mov  [tss1_cr3],eax
    call mem_alloc_kernel_page  ; get a page for pgtb0
    shl  eax,12
    mov  edx,eax
    mov  [pgtb0p],eax
    call mem_alloc_kernel_page  ; get a page for pgtb1
    shl  eax,12
    mov  ecx,eax
    mov  [pgtb1p],eax

            ; first the page directory

    mov  cr3,edi            ; install the page directory
    mov  eax,edx
    add  eax,7              ; page table 0: present, pl=3, r/w
    stosd                   ; ... pl=3 for now (simplify vga access)
    mov  eax,ecx
    add  eax,7              ; page table 1: present, pl=3, r/w
    stosd                   ; ... app memory
    xor  eax,eax            ; invalidate the rest of the app laddr space
    mov  ecx,0x400-2        ; (yeah, only one pgdir for kernel+apps for now)
    rep stosd

            ; pgtb0 is the page table for kernel memory

    mov  edi,edx
    stosd                   ; access to page 0 will always cause a fault
    mov  eax,0x1000 + 3     ; rest are direct map: present, pl=0, r/w
    mov  ecx,0x400-1
pgtb0_fill :
    stosd                   ; kernel gets to touch anything it wants < 4MB
    add  eax,0x1000
    loop pgtb0_fill

            ; pgtb1 is the first page table for app code/data/stack it is
            ; already all zeros (invalid - we'll fill in what we need later)

            ; enable paging - if we've done it all right, we won't crash

    mov  eax,cr0
    or   eax,0x80000000     ; msb of cr0
    mov  cr0,eax
    jmp  flush_ip2          ; flush the cpu instruction pipeline
flush_ip2:

    ; ---- build the interrupt descriptor table

    call mem_alloc_kernel_page  ; get a page for the idt
    shl  eax,12                 ; convert pgno to physical addr
    mov  edx,eax
    mov  [idtr_addr],eax

    mov  esi,irq_setup_table
    mov  ecx,irq_setup_table_size
irq_init :
    xor  eax,eax
    mov  [edx],eax                  ; clear the idt entry
    mov  [edx+2],eax
    lodsw
    mov  bx,ax
    and  ax,strict 0xfffc           ; separate config flags
    and  bx,3                       ; from selector/offset value
    cmp  bx,irqt_task               ; task gates are different
    jz   irq_init_task_gate
    mov  [edx],ax                   ; store the handler offset
    mov  ax,codesel
irq_init_task_gate :
    mov  word [edx+2],ax
    mov  ah,byte [bx+irq_types]
    xor  al,al
    mov  word [edx+4],ax
    add  edx,8
    loop irq_init

    lidt [idtr]                     ; install the idt

    ; ---- let's see what kind of interrupt hardware we've got

    mov  byte [enabled_lapic],0x0
    mov  eax,1
    cpuid
    or   eax,eax
    jb   no_lapic
    and  edx,1 << 9         ; lapic feature
    jz   no_lapic
    mov  byte [enabled_lapic],0x1
no_lapic :

            ; if enabled, create page table entry for the lapic

    mov  al,[enabled_lapic]
    or   al,al
    jz   no_lapic_init

    mov  edx,0xfee00000     ; phys address
    mov  ecx,0x1000         ; length
    call map_pmem

    ; do the ioapic while we're at it
    mov  edx,0xfec00000     ; phys address
    push edx
    mov  ecx,0x1000         ; length
    call map_pmem
    pop  edx
    mov  dword [edx],1
    mov  eax,[0xfec00010]

    call irq_init_bsp_apic_hardware

no_lapic_init :

    ; ---- setup entry point for non_boot_cpus

    mov  al,[non_boot_cpu_ljmp_instruction]     ; get the ljmp instruction
    mov  [sipi_vector],al                       ; place it at a 4k phys mem boundary
    mov  dword [sipi_vector+1],main

    ; ---- establish a current task

    xor  eax,eax
    mov  al,tasksel_k00
    ltr  ax

    ; ---- check for init apps

    mov  ebx, kend
app_loop :
    mov  eax, [ebx+0x18]    ; load the entry address
    cmp  dword [ebx], 0x70617a6f  ; check for "ozap"
    jz   have_an_app
    xor  eax, eax           ; no app
have_an_app :

    cmp  eax, 0             ; is there an app to run?
    jz   idle               ; if not, idle right away

    ; ---- setup the init task entry point

    mov  edi,tss1_eip
    stosd

    ; reuse the same tss, and page tables for all the
    ; init apps - this means they run serialy - each has
    ; to exit for the next one to run

    xor   eax,eax
    mov   [tss1],eax        ; clear out previous task link
    stosd                   ; and some registers (flags)
    stosd                   ; (eax)
    stosd                   ; (ecx)
    stosd                   ; (edx)
    stosd                   ; (ebx)
    mov   eax, [ebx+0x10]   ; get the end of the app
    ;add   eax,0x8001000     ; (this tests app stack page faults)
    stosd                   ; (esp)
    xor   eax,eax
    stosd                   ; (ebp)
    stosd                   ; (esi)
    stosd                   ; (edi)

    mov  edi,[pgtb1p]       ; rewrite the app's page table
    mov  eax,ebx
    or   eax,5              ; init app code at 0x400000 (4Mb) present and r/o
    mov  ecx,[ebx+0x20]     ; get the start of the data segment
    sub  ecx,0x400000
    shr  ecx,12
    ; FIXME need to cap ecx here in case app image is corrupted
init_pgtb1_ro :
    stosd
    add  eax,0x1000
    loop init_pgtb1_ro

    mov  ecx,[ebx+0x10]     ; get the end of the app
    sub  ecx,[ebx+0x20]     ; subtract the start of the data segment
    shr  ecx,12
    ; FIXME need to cap ecx here in case app image is corrupted
    add  eax,2              ; the data/bss/stack pages are r/w
init_pgtb1_rw :
    stosd
    add  eax,0x1000
    loop init_pgtb1_rw

    ; ---- debug marker
    mov  byte [gs:1],0xA    ; turn the first two chars green
    mov  byte [gs:3],0xA

    ; ---- use our current stack for system interrupts during the app

    push ebx
    mov  edi,tss1_esp0
    mov  eax,esp
    stosd

    ; ---- start the app

    sti
    call tasksel_u00:0
    pop  ebx

    ; ---- point to the end of this init app

    mov  eax,0x400000       ; FIXME 80386 needs to reload cr3
    mov  ecx,[ebx+0x10]     ; get the end of the app
    sub  ecx,eax
    shr  ecx,12
    ; FIXME need to cap ecx here in case app image is corrupted
invl_app :
    invlpg [eax]
    add  eax,0x1000
    loop invl_app

    mov  eax, [ebx+0x10]    ; load the app end address
    sub  eax,0x400000
    add  ebx,eax            ; point ebx to the next app
    jmp  app_loop

; -------- non-boot cpu initialization --------

non_boot_init :

    lidt [idtr]             ; install the global idt

    ; ---- enable paging

    mov  edi,[pgdirp]       ; load this cpu's paging register
    mov  cr3,edi

    push eax
    mov  eax,cr0            ; enable paging
    or   eax,0x80000000
    mov  cr0,eax
    jmp  flush_ip3          ; flush the instruction pipeline
flush_ip3 :
    pop  eax

    ; ---- limit the number of threads we support here

    ;cmp  eax,2
    cmp  eax,max_threads
    jae  idle

    ; ---- establish a current task

    mov  ebx,eax            ; move cpu number to ebx
    call create_tss_pair
    cmp  eax,0              ; memory alloc errs nix this core
    jz   idle

    push ebx
    shl  ebx,4              ; 8 byte selectors in pairs
    add  ebx,tasksel_k00
    ltr  bx                 ; establish a current task
    mov  edi,ebx

    ; ---- init the lapic

    call irq_init_ap_apic_hardware
    pop  ebx

    ; setup smbase?

    ; test kernel page fault handler
    ;mov  [321],eax

    add  edi,8              ; point edi at tasksel_uXX

nb_idle :
    sti
    ; debug
    ; to test with only one core, uncomment the cli which
    ; will lock all non-boot cores at the subsquent hlt ...
    ;cli
    ; debug end
    hlt                     ; wait for something to do

    cli			; likely unnecessary
    mov  esi,[gdt+edi+2]    ; lookup our tss base address
    and  esi,0xffffff
    mov  al,[gdt+edi+7]
    shl  eax,24
    or   esi,eax
    jz   nb_idle            ; tss not initialized
    cmp  dword [esi+(tss_eip-tss)],0  ; test eip to see if task is ready
    jz   nb_idle

    mov  eax,[esi+(tss_cr3-tss)]
    mov  cr3,eax            ; refresh local cpu tlb (required on real hw)

    mov  eax,edi            ; build a far call on the stack
    push eax
    xor  al,al
    push eax
    call far [esp]          ; the call works, but the return is untested!
                            ; (i.e. no app using more than the boot cpu has
                            ;  yet exited any thread ...)
    add  esp,8
    jmp nb_idle

    ; -------- boot cpu (and excess cpu) idle task --------

idle :
    sti
    hlt                     ; wait for interrupts
    jmp  idle

; ----------------------------
;    puts_vga - write a null delimited string to the VGA controller
;               in protected mode
;    enter:
;         esi - address of string
;         ebx - screen location (2 bytes per char, 160 bytes per line)
;    exit:
;         eax - destroyed
;         ebx - next screen location
;          gs - set to video memory selector
bits 32

puts_vga :
    mov  ax,videosel        ; point gs at video memory
    mov  gs,ax
puts_vga_loop :
    mov  al,[cs:esi]        ; use cs so irq handlers don't
    inc  esi                ; have to load kernel ds
    cmp  al,0
    jz   puts_vga_done
    mov  [gs:ebx],al
    add  ebx,2
    jmp  puts_vga_loop
puts_vga_done :
    ret

; ----------------------------
;   putx_vga - write the contents of eax in hex to the VGA controller
;              (in protected mode)
;   putbx_vga - write the contents of al in hex to the VGA controller
;
;   enter:
;       eax - value to convert to hex
;       ebx - screen location (2 bytes per char, 160 bytes per line)
;   exit:
;       ebx - next screen location
;        gs - set to video memory selector
bits 32

putbx_vga :
    push ecx
    mov  ecx,2
    and  eax,0xff
    rol  eax,24
    jmp putx_vga_loop

putx_vga :
    push ecx
    mov  ecx,8
putx_vga_loop :
    rol  eax,4
    push eax
    and  al,0xf
    cmp  al,9
    ja   putx_vga_hexdigit
    add  al,'0'
    jmp short putx_vga_putc
putx_vga_hexdigit :
    add  al,'a'-10
putx_vga_putc :
    mov  [gs:ebx],al
    add  ebx,2
    pop  eax
    and  eax,0xfffffff0
    loop putx_vga_loop
    mov  byte [gs:ebx],' '
    add  ebx,2
    pop  ecx
    ret

;------------------------------------------------------------------
;   mem_alloc_kernel_page - return the 4k page number of 1 page of
;                           memory from the kernel page pool
;
;   smp safe
;
;   returns:    eax = page number (zero will mean ... no pages left)

mem_alloc_kernel_page :
    ; FIXME need to expand this above 720K ...
    mov  eax,[next_free_page]
    cmp  eax,0xb0               ; stop under the vga
    mov  eax,0
    ja   mem_alloc_kernel_page_fail
    mov  eax,1
    lock xadd [next_free_page],eax   ; atomic, making this re-entrant
    push eax
    push ecx
    push edi
    shl  eax,12
    mov  ecx,0x1000/4
    mov  edi,eax
    xor  eax,eax
    rep stosd                   ; zero the page
    pop  edi
    pop  ecx
    pop  eax

    ; debug - show the last used address on the vga
    push eax
    push ebx
    mov  ebx, 39*2
    shl  eax,12
    call putx_vga
    pop  ebx
    pop  eax
    ; debug end

mem_alloc_kernel_page_fail :
    ret

;------------------------------------------------------------------
;   create a pair of tss structs for a new cpu
;
;   FIXME: I'm pretty sure I don't need two - a single tss should be fine.
;          partially a hold over from using interrupt gates to manage task
;          threads, a pair of tss structs provided register storage for both
;          a kernel thread and a user thread...
;
;   I'm using the tss eip field as a flag to indicate a valid tss.
;   set eip to zero to prevent use of the tss gate (code elsewhere
;   must test the eip field before using the gate).
;
;   enter:
;       ebx - cpu number
;   exit:
;       ebx - cpu number

create_tss_pair :
    call mem_alloc_kernel_page
    or   eax,eax
    jz   create_tss_pair_fail
    shl  eax,12                 ; convert pgno to physical addr
    mov  edi,eax                ; first tss is the kernel ring 0 thread

    mov  esi,edi
    add  esi,tss_len            ; second tss is the user ring 3 thread

    ; edi = ring 0 tss          (all the offsets will be calculated using the
    ; esi = ring 3 tss           tss0 values but they are just offsets :)

    mov  eax,[pgdirp]
    mov  [edi+(tss_cr3-tss)],eax
    mov  [esi+(tss_cr3-tss)],eax

    ; user tss gets ring3 selectors
    mov  eax,datasel3+3
    mov  [esi+(tss_es-tss)],eax
    mov  [esi+(tss_ss-tss)],eax
    mov  [esi+(tss_ds-tss)],eax
    mov  [esi+(tss_fs-tss)],eax
    mov  [esi+(tss_gs-tss)],eax
    mov  eax,codesel3+3
    mov  [esi+(tss_cs-tss)],eax

    ; sloppy: datasel maps all of physical ram, but its easier
    ; than having to set up a separate gdt stacksel for each cpu.
    ; pray the apps are trustworthy ...

    mov  eax,datasel
    mov  [esi+(tss_ss0-tss)],eax
    mov  eax,edi
    add  eax,0xc00                  ; towards the end of the tss page
    mov  [esi+(tss_esp0-tss)],eax

    mov  eax,edi
    add  eax,0x400
    mov  [edi+(tss_esp0-tss)],eax   ; in case the ring0 esp0 is ever used

    ; patch the tss addresses into our reserved gdt selectors

    mov  edx,ebx
    shl  edx,4                      ; 8 byte selectors in pairs
    add  edx,tasksel_k00

    mov  eax,edi
    or   edi,0x89000000             ; available tss
    shr  eax,24
    mov  [gdt+edx+2],edi            ; base 0-23 and flags
    mov  [gdt+edx+7],al             ; base 24-32

    add  edx,8h                     ; move to tasksel_uxx

    mov  eax,esi
    or   esi,0x89000000             ; available tss
    shr  eax,24
    mov  [gdt+edx+2],esi            ; base 0-23 and flags
    mov  [gdt+edx+7],al             ; base 24-32

; here is a twisty way to write an entire gdt entry at once,
; but apparently there is no race condition here
; so it is not needed.

    ;sub  esp,8

    ;mov  eax,edi
    ;or   edi,0x89000000             ; available tss
    ;mov  word [esp],(tss_len-1)
    ;mov  [esp+2],edi
    ;shr  eax,16
    ;mov  al,0x40
    ;mov  [esp+6],ax
    ;movq mm0,[esp]
    ;movntq [gdt+edx],mm0

    ;add  edx,8h                     ; move to tasksel_uxx

    ;mov  eax,esi
    ;or   esi,0x89000000             ; available tss
    ;mov  [esp+2],esi
    ;shr  eax,16
    ;mov  al,0x40
    ;mov  [esp+6],ax
    ;movq mm0,[esp]
    ;movntq [gdt+edx],mm0

    ;add  esp,8

    or   eax,1

create_tss_pair_fail :
    ret


; -------- interrupt handlers --------
%include "ozirq.asm"

; -------- system calls --------
%include "ozsys.asm"


align 64, db 0  ; 64 seems to allow us to guess total_size correctly
kernel_text_size equ ($-textstart)

; ---------------------------------------------------------------------------
section .data
datastart :

; -------- descriptors --------------
; Intel SW dev manual 3a, 3.4.5, pg 103
;
; In my opinion, macros for descriptor entries
; don't make the code any more readable.

descriptor_size equ 8

gdt :
nullsel equ $-gdt           ; nullsel = 0h
    dd 0,0                  ; first descriptor per convention is 0

codesel equ $-gdt           ; codesel = 8h  4Gb flat over all logical mem
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x9a                 ; present, dpl=0, code e/r
    db 0xcf                 ; 4k granular, 32bit/8bit, limit 16-19
    db 0x00                 ; base 24-31

datasel equ $-gdt           ; datasel = 10h  4Gb flat over all logical mem
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x92                 ; present, dpl=0, data r/w
    db 0xcf                 ; 4k granular, 32bit/8bit, limit 16-19
    db 0x00                 ; base 24-31

kstkbalo equ (kstack_loc & 0xffff)
kstkbami equ ((kstack_loc >> 16) & 0xff)
kstkbahi equ ((kstack_loc >> 24) & 0xff)
kstkszlo equ ((kstack_size-1) & 0xffff)
kstkszhi equ ((kstack_size-1) >> 16 & 0xf)

stacksel equ $-gdt          ; stacksel = 18h  small limited stack
    dw kstkszlo             ; limit 0-15
    dw kstkbalo             ; base  0-15
    db kstkbami             ; base 16-23
    db 0x92                 ; present, dpl=0, data, r/w
    db 0x40 + kstkszhi      ; byte granular, 32bit/8bit, limit 16-19
    db kstkbahi             ; base 24-31

videosel equ $-gdt          ; videosel = 20h
    dw 3999                 ; limit 80*25*2-1
    dw 0x8000               ; base 0xb8000
    db 0x0b
    db 0x92                 ; present, dpl=0, data, r/w
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

    ; useful for BIOS calls someday?

rmcssel equ $-gdt           ; real mode CS selector = 28h
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x9a                 ; present, dpl=0, code e/r
    db 0x0f                 ; byte granular, 16bit, limit 16-19
    db 0x00                 ; base 24-31

rmdssel equ $-gdt           ; real mode DS selector = 30h
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x92                 ; present, dpl=0, data r/w
    db 0x0f                 ; byte granular, 16bit, limit 16-19
    db 0x00                 ; base 24-31

    ; ring 3 selectors in the gdt - no need for an ldt

    ; FIXME the apps don't need access to all memory right?
    ; or maybe page level protections are enough!

codesel3 equ $-gdt          ; codesel3 = 38h
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0xfa                 ; present, dpl=3, code e/r
    db 0xcf                 ; 4k granular, 32bit/8bit, limit 16-19
    db 0x00                 ; base 24-31

datasel3 equ $-gdt          ; datasel = 40h  4Gb flat over all logical mem
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0xf2                 ; present, dpl=3, data r/w
    db 0xcf                 ; 4k granular, 32bit/8bit, limit 16-19
    db 0x00                 ; base 24-31

tasksel_f08 equ $-gdt       ; the double fault task selector
    dw (tss_len-1)          ; tss length
    dw tss_f08              ; tss physical address
    db 0
    db 0x89                 ; present, dpl=0, tss32
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

tasksel_f10 equ $-gdt       ; the invalid tss task selector
    dw (tss_len-1)          ; tss length
    dw tss_f10              ; tss physical address
    db 0
    db 0x89                 ; present, dpl=0, tss32
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

; there is one kernel thread tss (ring 0) and one user thread tss (ring 3)
; per cpu.  memory for tss structs for the non-boot cpus are allocated as
; each non-boot cpu comes online (see create_tss_pair).
; tasksel_uXX tsses are initialized in new_thread.

gdt_tasks :
tasksel_k00 equ $-gdt
                        dw (tss_len-1), tss0, 0x8900, 0x40
tasksel_u00 equ $-gdt
                        dw (tss_len-1), tss1, 0x8900, 0x40
  times 2*(max_ncpus-1) dw (tss_len-1),    0, 0x8900, 0x40

max_threads equ ($-gdt_tasks)/descriptor_size/2

gdt_end :

; ---------------------
; the tss that handles double fault exceptions

tss_f08 :                   ; intel sw 3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
    dd tss_f08_stk-0x100    ; esp0
    dw datasel,0            ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
tss_f08_cr3 :
    dd 0                    ; cr3
    dd int_handler_tg_dblflt ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
    dd tss_f08_stk          ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
    dw datasel,0            ; es
    dw codesel,0            ; cs
    dw datasel,0            ; ss
    dw datasel,0            ; ds
    dw 0,0                  ; fs
    dw videosel,0           ; gs
    dw 0,0                  ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap

; ---------------------
; the tss that handles invalid tss exceptions

tss_f10 :                   ; intel sw 3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
    dd tss_f10_stk-0x100    ; esp0
    dw datasel,0            ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
tss_f10_cr3 :
    dd 0                    ; cr3
    dd int_handler_tg_invtss ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
    dd tss_f10_stk          ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
    dw datasel,0            ; es
    dw codesel,0            ; cs
    dw datasel,0            ; ss
    dw datasel,0            ; ds
    dw 0,0                  ; fs
    dw videosel,0           ; gs
    dw 0,0                  ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap

; ---------------------
; tss0 and tss1 are cpu0's pair

tss :
tss0 :                      ; intel swdev3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
tss_esp0 :
    dd 0                    ; esp0
tss_ss0 :
    dw 0,0                  ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
tss_cr3 :
tss0_cr3 :
    dd 0                    ; cr3
tss_eip :
    dd 0                    ; eip
    dd 0                    ; eflags
tss_eax :
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
tss_esp :
    dd 0                    ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
tss_es :
    dw 0,0                  ; es
tss_cs :
    dw 0,0                  ; cs
tss_ss :
    dw 0,0                  ; ss
tss_ds :
    dw 0,0                  ; ds
tss_fs :
    dw 0,0                  ; fs
tss_gs :
    dw videosel,0           ; gs
    dw 0,0                  ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap
tss_end :

tss_len equ tss_end-tss

; user tss

tss1 :                      ; intel sw 3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
tss1_esp0 :
    dd 0                    ; esp0  (int and irq support)
    dw stacksel,0           ; ss0   (filled in when running init apps)
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
tss1_cr3 :
    dd 0                    ; cr3
tss1_eip :
    dd 0                    ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
tss1_esp :
    dd 0                    ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
    dw datasel3+3,0         ; es
    dw codesel3+3,0         ; cs
    dw datasel3+3,0         ; ss
    dw datasel3+3,0         ; ds
    dw datasel3+3,0         ; fs
    dw datasel3+3,0         ; gs
    dw 0,0                  ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap

; ---------------------

gdtr :
    dw gdt_end - gdt - 1    ; gdt length
    dd gdt                  ; gdt physical address

idtr :
    ; irq_setup_table_size == the number of gates in the idt
    dw irq_setup_table_size * 8 - 1     ; idt limit (should be 2k)
idtr_addr :
    dd 0                                ; address of the idt

ncpus           dd 0        ; number of running cpus

next_free_page  dd 0        ; initialized after page tables are setup

sleepers        times max_ncpus/8 db 0  ; one bit per cpu (except cpu0)

pgdirp          dd 0
pgtb0p          dd 0
pgtb1p          dd 0

enabled_lapic   db 0        ; set to 1 if an lapic is present and in use

; isolate a cache line for monitor/mwait
align 64, db 0
wait_addr       times 64 db 0

dbgwall         db 0xaa,0x55

align 64, db 0  ; 64 seems to allow us to guess total_size correctly
kernel_data_size equ ($-datastart)

; ---------------------
; Need to align to a physical page boundary here so that appended init apps
; are always page aligned.  The problem is our text section starts 1k below
; a page boundary (0x7c00), and so an 'align 4096' doesn't match up with
; physical memory.

section .fill

fill :
    times (4096 - ((0x7c00 + total_size) % 4096)) db 0

kend :

