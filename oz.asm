; OZ - A more utopian OS   x86-32 startup 
; ex: set expandtab softtabstop=4 shiftwidth=4 nowrap :
;
; Copyright (C) 2015  Duane Voth
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
;	$ qemu-system-i386 -boot a -fda oz_fd
;
; requires: nasm-2.07  or later from: http://www.nasm.us
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
; 2015/10/12 - 0.03.01 - djv - cleanup, add smp usermode tss structs, sleep,
;                              wakeup, and ipi for user thread creation.

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
    jmp short load_stage2   ; jump to stage2 loader, skip mbr data struct

times 6-($-$$)  db 0
oemid db "oz"

times 11-($-$$)  db 0

; compute the size of the kernel image in 512 byte sectors
total_size equ (kernel_text_size + kernel_data_size)
kisectors  equ (total_size)/512 + (APP_SIZE + 512)/512
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
    push dx                 ; save BIOS drive number

    mov  ax,0x0600          ; ah=6 scroll window up, if al = 0 clrscr
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

    mov  ax,[stage2]         ; check the signature byte
    add  ax,[stage2+2]
    cmp  ax,0x7a6f+0x32
    jz   stage2_present

    ; -------- stage2 boot loader --------

    ; Assume that the kernel is smaller than whatever space
    ; is provided prior to file system data structures on the
    ; boot device, and that it can immediately follow the MBR.

    mov  ah,02h
    mov  al,kisectors-1     ; number of sectors to load
    mov  bx,stage2
    mov  cx,2
    pop  dx                 ; recover BIOS drive number
    push cs
    pop  es
    int  13h
    jc   ioerr

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

bootmsg     db      "OZ v0.03.01 - 2015/10/07 ",0
s2errmsg    db      "stage 2 load failure ",0
ioerrmsg    db      "i/o error loading stage 2 ",0

times 446-($-$$) db 0       ; fill with zeros up to partition table

tmpstk      equ     $

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
; shot.

bits 16

stage2 :
    db "oz2",0                      ; stage2 signature

non_boot_cpu_ljmp_instruction :     ; place this in 16 bit code land
    jmp word 0:0                    ; so we get the right opcode

kstack_loc  equ 0x1000      ; must be on a 4k pysical page boundary
kstack_size equ 4096*2

pgdir equ 0x3000            ; use some of the free memory below us
pgtb0 equ 0x4000            ; code below assumes this follows pgdir
pgtb1 equ 0x5000            ; code below assumes this follows pgtb0

idt   equ 0x7000            ; use some of the free memory below us

; ---------------------

start_stage2 :

    mov  ax,kstack_loc+kstack_size  ; setup the kernel stack
    mov  sp,ax

    ; ---- initialize the 8259's while in real mode

    call irq_init_hardware

; ------------ main kernel entry point ------------
; all cpus enter here
main :

    ; -------- enter protected mode --------

    lgdt [gdtr]             ; initialize the gdt
    mov  eax,cr0
    or   al,0x01            ; set the protected mode bit (lsb of cr0)
    mov  cr0,eax
    jmp  codesel:flush_ip1  ; flush the cpu instruction pipeline
flush_ip1: 
bits 32                     ; instructions after this point are 32bit

    mov  eax,1
    xadd [ncpus],eax        ; get our unique cpu number
                            ; could use the lapic id if available
    mov  esi,eax
    mov  ax,datasel   
    mov  ds,ax              ; initialize the data segments
    mov  es,ax
    mov  ax,stacksel        ; setup a restricted stack segment
    mov  ss,ax
    mov  esp,kstack_size
    mov  eax,esi
    shl  eax,8              ; divvy up the stack, 256 bytes per cpu
    sub  esp,eax

    mov  eax,esi
    push eax
    mov  ebx,eax
    add  bl,'0'             ; boot cpu announces via ascii 0
    mov  edi,160-2          ; last chracter on first line of vga
    shl  eax,1
    sub  edi,eax
    mov  ax,videosel        ; point gs at video memory
    mov  gs,ax
    mov  [gs:edi],bl        ; announce cpu presence
    pop  eax

    or   eax,eax            ; are we the boot cpu?
    jg   non_boot_init      ; if not, do non_boot_init

    call irq_init_bsp_apic_hardware

    ; ---- debug marker
    mov  byte [gs:1],0xA    ; turn the first two chars green
    mov  byte [gs:3],0xA

    ; ---- build the interrupt descriptor table

    mov  esi,irq_setup_table
    mov  ecx,irq_setup_table_size
    mov  edx,idt
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

    ; ---- setup entry point for non_boot_cpus

    mov  al,[non_boot_cpu_ljmp_instruction]     ; get the ljmp instruction
    mov  [kstack_loc],al                        ; place it at a 4k phys mem boundary
    mov  dword [kstack_loc+1],main

    ; ---- setup the paging tables

    mov  edi,pgdir          ; first the page directory
    mov  cr3,edi            ; install the page directory
    mov  eax,pgtb0 + 7      ; page table 0: present, pl=3, r/w
    stosd                   ; ... pl=3 for now (simplify vga access)
    mov  eax,pgtb1 + 7      ; page table 1: present, pl=3, r/w
    stosd                   ; ... app memory
    xor  eax,eax            ; invalidate the rest of the app laddr space
    mov  ecx,0x400-2        ; (yeah, only one pgdir for kernel+apps for now)
    rep stosd

            ; assume pgtb0 physically follows the pg dir
            ; pgtb0 is the page table for kernel memory

    stosd                   ; access to page 0 will always cause a fault
    mov  eax,0x1000 + 3     ; rest are direct map: present, pl=0, r/w
    mov  ecx,0x400-1
pgtb0_fill :
    stosd
    add  eax,0x1000
    loop pgtb0_fill

            ; assume pgtb1 physically follows pgtb0
            ; pgtb1 is the first page table for app code/data/stack

    xor  eax,eax            ; invalidate the app logical address space
    mov  ecx,0x400          ; (we'll fill in what we need later)
    rep stosd

            ; enable paging

    mov  eax,cr0
    or   eax,0x80000000     ; msb of cr0
    mov  cr0,eax
    jmp  flush_ip2          ; flush the cpu instruction pipeline
flush_ip2: 

            ; establish a "pool" of free pyhsical memory

    mov  eax,((kilast+0x1000) >> 12)    ; include a buffer zone
    mov  [next_free_page],eax

            ; if enabled, create page table entry for the lapic

    mov  al,[enabled_lapic]
    or   al,al
    jz   no_lapic_init

    mov  edx,0xfee00000     ; phys address
    mov  ecx,0x1000         ; length
    call map_pmem

no_lapic_init :

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

    ; cheat: reuse the same tss, ldt, and page tables for all
    ; the init apps - this means they run serialy - each has
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

    mov  edi,pgtb1          ; rewrite the app's page table
    mov  eax,ebx
    or   eax,5              ; init app code at 0x400000 (4Mb) present and r/o
    stosd                   ; assume all the init apps are < 4k
    add  eax,0x1000 + 2     ; add one page for data/bss/stack
    stosd

    ; ---- start the app

    push ebx
    sti
    call tasksel_u00:0
    pop  ebx

    ; ---- point to the end of this init app

    invlpg [0x400000]       ; FIXME 80386 needs to reload cr3
    invlpg [0x401000]

    mov  eax, [ebx+0x10]    ; load the app end address
    sub  eax,0x400000
    add  ebx,eax            ; point ebx to the next app
    jmp  app_loop

; -------- non-boot cpu initialization --------

non_boot_init :

    lidt [idtr]             ; install the global idt

    ; ---- enable paging

    mov  edi,pgdir          ; load this cpu's paging register
    mov  cr3,edi

    push eax
    mov  eax,cr0            ; enable paging
    or   eax,0x80000000
    mov  cr0,eax
    jmp  flush_ip3          ; flush the cpu instruction pipeline
flush_ip3 : 
    pop  eax

    ; ---- limit the number of cpus we support here

    cmp  eax,16
    jae  nb_idle

    ; ---- init the lapic

    call irq_init_ap_apic_hardware

    ; setup smbase?

    ; ---- establish a current task

    mov  ebx,eax            ; move cpu number to ebx
    call create_tss_pair
    push ebx
    shl  ebx,4              ; 16x (selector size x2)
    add  ebx,tasksel_k00
    ltr  bx                 ; establish a current task
    pop  ebx

    ; test kernel page fault handler
    ;mov  [321],eax

nb_idle :
    sti
    hlt                     ; wait for something to do
    jmp  nb_idle            ; (see new_thread)

    ; -------- boot cpu idle task --------
    ; could be combined with nb_idle but separating
    ; these can allow for easier debug

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
;   mem_alloc_kernel_page - return the 4k page number of 1 page of memory
;                           from the kernel page pool
;
;   returns:    eax = page number, zero means no pages left

mem_alloc_kernel_page :
    mov  eax,[next_free_page]
    inc  dword [next_free_page]
    ; probably should check for the end of something ...
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
    ret

;------------------------------------------------------------------
;   create a pair of tss structs for a new cpu
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
    mov  edi,eax

    mov  esi,edi
    add  esi,(tss0_end-tss0)

    mov  eax,pgdir
    mov  [edi+(tss0_cr3-tss0)],eax
    mov  [esi+(tss0_cr3-tss0)],eax

    mov  eax,datasel1+7
    mov  [esi+(tss0_es-tss0)],eax
    mov  [esi+(tss0_ss-tss0)],eax
    mov  [esi+(tss0_ds-tss0)],eax
    mov  eax,codesel1+7
    mov  [esi+(tss0_cs-tss0)],eax
    mov  eax,ldtsel1+3
    mov  [esi+(tss0_ldt-tss0)],eax

    ; setup the tss structs (edi = kernel, esi = user).
    ; there are three stacks total
    ;   (1) kernel tss esp0 - placed at the end of this page
    ;   (2) user tss esp0 (for interrupt handling) - end minus 1k
    ;   (3) user tss esp3 (esp) - will be set up by new_thread

    mov  eax,edi
    add  eax,0x1000
    mov  [edi+(tss0_esp0-tss0)],eax ; (1) kernel tss esp0
    sub  eax,0x400
    mov  [esi+(tss0_esp0-tss0)],eax ; (2) user tss esp0
    ;mov  eax,stacksel
    mov  eax,datasel
    mov  [edi+(tss0_ss0-tss0)],eax
    mov  [esi+(tss0_ss0-tss0)],eax

    ; patch the tss addresses into the reserved gdt selectors

    mov  edx,ebx
    shl  edx,4                      ; 16x because selectors are in pairs
    add  edx,tasksel_k00

    mov  eax,edi
    shr  eax,24
    mov  byte [gdt+edx+7],al        ; base 24-32
    and  edi,0xffffff
    or   edi,[gdt+edx+2]            ; or in flags
    mov  [gdt+edx+2],edi            ; base 0-23 and flags

    add  edx,8                      ; move to tasksel_uxx

    mov  eax,esi
    shr  eax,24
    mov  byte [gdt+edx+7],al        ; base 24-32
    and  esi,0xffffff
    or   esi,[gdt+edx+2]            ; or in flags
    mov  [gdt+edx+2],esi            ; base 0-23 and flags

create_tss_pair_fail :
    ret

; -------- interrupt handlers --------
%include "ozirq.asm"

; -------- system calls --------
%include "ozsys.asm"

align 16, db 0
kernel_text_size equ ($-textstart)

; ---------------------------------------------------------------------------
section .data
datastart :

; -------- descriptors --------------
; Intel SW dev manual 3a, 3.4.5, pg 103
;
; In my opinion, macros for descriptor entries
; don't make the code that much more readable.

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

stacksel equ $-gdt          ; stacksel = 18h  small limited stack
    dw kstack_size-1        ; limit
    dw kstack_loc           ; base
    db 0
    db 0x92                 ; present, dpl=0, data, r/w
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

videosel equ $-gdt          ; videosel = 20h
    dw 3999                 ; limit 80*25*2-1
    dw 0x8000               ; base 0xb8000
    db 0x0b
    db 0x92                 ; present, dpl=0, data, r/w
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

rmcssel equ $-gdt           ; real mode CS selector = 28h
    dw 0x0ffff              ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x9a                 ; present, dpl=0, code e/r
    db 0x0f                 ; byte granular, 16bit, limit 16-19
    db 0x00                 ; base 24-31

rmdssel equ $-gdt           ; real mode DS selector = 30h
    dw 0x0ffff              ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0x92                 ; present, dpl=0, data r/w
    db 0x0f                 ; byte granular, 16bit, limit 16-19
    db 0x00                 ; base 24-31

ldtsel1 equ $-gdt
    dw ldt1_len             ; length of the ldt
    dw ldt1                 ; address of the ldt
    db 0
    db 0x82                 ; present, dpl=0, ldt
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

tasksel_f08 equ $-gdt       ; the double fault task selector
    dw tss_len              ; tss length
    dw tss_f08              ; tss physical address
    db 0
    db 0x89                 ; present, dpl=0, tss32
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

tasksel_f10 equ $-gdt       ; the invalid tss task selector
    dw tss_len              ; tss length
    dw tss_f10              ; tss physical address
    db 0
    db 0x89                 ; present, dpl=0, tss32
    db 0x40                 ; byte granular, 32bit/8bit
    db 0

; there is one kernel thread tss (ring 0) and one user thread tss (ring 1)
; per cpu.  memory for tss structs for the non-boot cpus are allocated as
; each non-boot cpu comes online (see create_tss_pair).  tasksel_uXX tss
; gates are installed in the idt (starting at first_thread_tss_gate),
; they also map 1-to-1 with cpus.  (the thread is launched via an lapic
; vectored interrupt that jumps immediately into user space - and to call a
; task gate from an interrupt requires an existing ring 0 tss to be active)
; tasksel_uXX tsses are initialized in new_thread.

tasksel_k00 equ $-gdt
                        dw tss_len, tss0, 0x8900, 0x40
tasksel_u00 equ $-gdt
                        dw tss_len, tss1, 0x8900, 0x40
tasksel_k01 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u01 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k02 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u02 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k03 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u03 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k04 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u04 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k05 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u05 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k06 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u06 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k07 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u07 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k08 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u08 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k09 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u09 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k10 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u10 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k11 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u11 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k12 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u12 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k13 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u13 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k14 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u14 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_k15 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
tasksel_u15 equ $-gdt
                        dw tss_len, 0, 0x8900, 0x40
gdt_end :

; ---------------------

ldt1 :
nullsel1 equ $-ldt1         ; nullsel1 = 07h
    dd 0,0                  ; first descriptor per convention is 0

codesel1 equ $-ldt1         ; codesel1 = 0fh  4Gb flat over all logical mem
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0xfa                 ; present, dpl=3, code e/r
    db 0xcf                 ; 4k granular, 32bit, limit 16-19
    db 0x00                 ; base 24-31

datasel1 equ $-ldt1         ; datasel1 = 17h  4Gb flat over all logical mem
    dw 0xffff               ; limit 0-15
    dw 0x0000               ; base  0-15
    db 0x00                 ; base 16-23
    db 0xf2                 ; present, dpl=3, data r/w
    db 0xcf                 ; 4k granular, 32bit, limit 16-19
    db 0x00                 ; base 24-31

; gcc wants the ds, es, and ss segment registers to match
;stacksel1 equ $-ldt1        ; stacksel = 1ch  small limited stack
;    dw 0xffff               ; limit
;    dw 0x0000               ; base  0-15
;    db 0x00
;    db 0xf2                 ; present, dpl=3, data, r/w
;    db 0                    ; byte granular, 16 bit
;    db 0

ldt1_end :

ldt1_len equ ldt1_end-ldt1

; ---------------------
; the tss that handles double fault exceptions

tss_f08 :                   ; intel sw 3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
    dd 0                    ; esp0
    dw 0,0                  ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
    dd pgdir                ; cr3
    dd int_handler_tg_dblflt ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
    dd kstack_size/4        ; esp
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
    dd 0                    ; esp0
    dw 0,0                  ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
    dd pgdir                ; cr3
    dd int_handler_tg_invtss ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
    dd kstack_size/2        ; esp
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
; tss0 and tss1 are cpu0's pair, these are also templates for other
; cpus that come online.

tss0 :                      ; intel swdev3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
tss0_esp0 :
    dd 0                    ; esp0
tss0_ss0 :
    dw 0,0                  ; ss0
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
tss0_cr3 :
    dd pgdir                ; cr3
tss0_eip :
    dd 0                    ; eip
    dd 0                    ; eflags
tss0_eax :
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
tss0_esp :
    dd 0                    ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
tss0_es :
    dw 0,0                  ; es
tss0_cs :
    dw 0,0                  ; cs
tss0_ss :
    dw 0,0                  ; ss
tss0_ds :
    dw 0,0                  ; ds
    dw 0,0                  ; fs
    dw videosel,0           ; gs
tss0_ldt :
    dw 0,0                  ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap
tss0_end :

tss_len equ tss0_end-tss0

; user tss

tss1 :                      ; intel sw 3a 7.6  pg 287 of 756
    dw 0,0                  ; previous task link
    dd kstack_size-32       ; esp0  (int and irq support)
    dw datasel,0            ; ss0   (-32 leaves some stack space for main)
    dd 0                    ; esp1
    dw 0,0                  ; ss1
    dd 0                    ; esp2
    dw 0,0                  ; ss2
    dd pgdir                ; cr3
tss1_eip :
    dd 0                    ; eip
    dd 0                    ; eflags
    dd 0                    ; eax
    dd 0                    ; ecx
    dd 0                    ; edx
    dd 0                    ; ebx
    dd 0                    ; esp
    dd 0                    ; ebp
    dd 0                    ; esi
    dd 0                    ; edi
    dw datasel1+7,0         ; es
    dw codesel1+7,0         ; cs
    dw datasel1+7,0         ; ss
    dw datasel1+7,0         ; ds
    dw 0,0                  ; fs
    dw 0,0                  ; gs
    dw ldtsel1+3,0          ; ldt
    dw 0                    ; trap
    dw 0                    ; iomap

; ---------------------

gdtr :
    dw gdt_end - gdt - 1    ; gdt length
    dd gdt                  ; gdt physical address

idtr :
    ; irq_setup_table_size == the number of gates in the idt
    dw irq_setup_table_size * 8 - 1     ; idt limit (should be 2k)
    dd idt                              ; address of the idt

ncpus           dd 0        ; number of running cpus

enabled_lapic   db 0        ; set to 1 if an lapic is present and in use

next_free_page  dd 0        ; initialized after page tables are setup

sleepers        dq 0        ; one bit per cpu

align 16, db 0
kernel_data_size equ ($-datastart)


; ---------------------
; Need to align to a physical page boundary here so that appended init apps
; are always page aligned.  The problem is our text section starts 1k below
; a page boundary, and so an 'align 4096' doesn't match up with physical
; memory.

; Add larger tests here if the times expression turns up negative, to make
; the kernel image size right, but your next problem will be that the boot
; loader will likely refuse to load all these sectors in one pass.

section .fill

fill :

%if total_size > 4096+1024
    times (4096+4096+1024-total_size) db 0
%elif total_size > 1024
    times (4096+1024-total_size) db 0
%endif

kend :

