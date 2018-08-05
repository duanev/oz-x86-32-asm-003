; OZ - A more utopian OS    x86-32 interrupts
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
; IRQ handling
;
; resources:
;       http://forum.osdev.org/viewtopic.php?p=107868#107868

cpumsg      db      "cpu",0

int00msg    db      "divide by zero ",0
int01msg    db      "debug exception ",0
int02msg    db      "nmi ",0
int03msg    db      "breakpoint exception ",0
int04msg    db      "overflow exception ",0
int05msg    db      "bound exceeded ",0
int06msg    db      "invalid opcode ",0
int07msg    db      "device unavailable ",0
int08msg    db      "double fault ",0
int09msg    db      "coprocessor segment overrun ",0
int10msg    db      "invalid tss ",0
int11msg    db      "segment not present ",0
int12msg    db      "stack fault ",0
int13msg    db      "general protection fault ",0
int14kmsg   db      "kernel page fault addr=",0
int14umsg   db      "app page fault addr=",0
int16msg    db      "floating point err ",0
int17msg    db      "alignment check ",0
int18msg    db      "machine check ",0
int19msg    db      "simd floating point err ",0

int34msg    db      "hw int02 ",0
int35msg    db      "hw int03 ",0
int36msg    db      "hw int04 ",0
int37msg    db      "hw int05 ",0
int38msg    db      "hw int06 ",0
int39msg    db      "hw int07 ",0
int40msg    db      "hw int08 ",0
int41msg    db      "hw int09 ",0
int42msg    db      "hw int10 ",0
int43msg    db      "hw int11 ",0
int44msg    db      "hw int12 ",0
int45msg    db      "hw int13 ",0
int46msg    db      "hw int14 ",0
int47msg    db      "hw int15 ",0

spuriousmsg db      "spurious int ",0
apicerrmsg  db      "apicerr int ",0
himsg       db      "hi ",0
int255msg           db  "unknown system call ",0

intvmmsg            db  "vm fault: ",0

  align 4
irq_err_lno dd 0
fault_count dd 0
ten_secs_counter dd 100

; ---- IRQ hardware initialization ----

bits 16

irq_init_hardware :

    ; re-program the 8259's to move the hardware vectors out of the
    ; soft int range ... C'mon, Intel was pretty clear about this!

    mov  al,0x11
    out  0x20,al            ; init the 1st 8259
    mov  al,0x11
    out  0xA0,al            ; init the 2nd 8259
    mov  al,apic0_irqbase
    out  0x21,al            ; base for the 1st 8259
    mov  al,apic1_irqbase
    out  0xA1,al            ; base for the 2nd 8259
    mov  al,0x04
    out  0x21,al            ; set 1st 8259 as master
    mov  al,0x02
    out  0xA1,al            ; set 2nd 8259 as slave
    mov  al,0x01
    out  0x21,al
    mov  al,0x01
    out  0xA1,al
    mov  al,0xfc            ; PIC1 disable all but the timer and kbd
    out  0x21,al
    mov  al,0xff            ; PIC2 disable everything
    out  0xA1,al
    ret

bits 32

; ----------

irq_init_bsp_apic_hardware :
    ;jmp  no_apic

    ; ---- test for an apic

; not needed?  causing problems?
;   mov  eax,[0xfee00370]
;   and  eax,0xffffff00
;   or   eax,apicerr_int
;   mov  [0xfee00370],eax   ; setup LVT3 error vector

    ;mov  eax,0x00000100 + spurious_int    ; enable + spurious int
    mov  eax,0x00000100     ; enable
    mov  [0xfee000f0],eax   ; Spurious interrupt vector reg
    mov  eax,0x01000000
    mov  [0xfee000d0],eax   ; set our LDR
    mov  eax,0xffffffff
    mov  [0xfee000e0],eax   ; set our DFR
    xor  eax,eax
    mov  [0xfee000b0],eax   ; eoi anything outstanding

;    ; ---- enable the local apic via msr
; but apparently not needed ...
;    mov  ecx,0x1b
;    xor  edx,edx
; ;  mov  eax,0xfffff800
;    mov  eax,0x00000800
;    wrmsr
;    mov  eax,[0xfffff030]

    ; ---- visual indicator: lapic active

    mov  ax,videosel        ; point gs at video memory
    mov  gs,ax
    mov  byte [gs:25*2],'+'

    ret

; ----------

irq_init_ap_apic_hardware :
    ; eax = cpu number

    ; ---- mtrr for 0xfee00000 -> strong uncachable (UC) ?

    push eax                ; save cpu number
    mov  ecx,eax
    mov  al,[enabled_lapic]
    or   al,al
    jz  no_lapic_init2

    mov  ebx,0x01000000
    shl  ebx,cl             ; bit mask based on cpu number
    mov  [0xfee000d0],ebx   ; set our LDR

; not needed?  causing problems?
;   mov  eax,[0xfee00370]
;   and  eax,0xffffff00
;   or   eax,apicerr_int
;   mov  [0xfee00370],eax   ; setup LVT3 error vector

    ;mov  eax,0x00000100 + spurious_int  ; enable + spurious int
    mov  eax,0x00000100     ; enable
    mov  [0xfee000f0],eax   ; Spurious interrupt vector reg
    xor  eax,eax
    mov  [0xfee000b0],eax   ; eoi anything outstanding
no_lapic_init2 :
    pop  eax
    ret

; ---- IRQ handlers ----

align 4
int_handler_div0 :
    push esi
    mov  esi,int00msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_debug :
    push esi
    mov  esi,int01msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_nmi :
    push esi
    mov  esi,int02msg
    call irq_print_msg
    pop  esi
    iret
    ;jmp  reboot_on_alt_key

align 4
int_handler_brkp :
    push esi
    mov  esi,int03msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_ovrflw :
    push esi
    mov  esi,int04msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_bound :
    push esi
    mov  esi,int05msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_invop :
    push esi
    mov  esi,int06msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_devna :
    ;push esi
    ;mov  esi,int07msg
    ;call irq_print_msg
    ;pop  esi
    ; FIXME fxsave/fxrestore the fpu/sse/mmx regs
    clts                ; sure! everybody can use the fpu
    iret

align 4
int_handler_cpsego :
    push esi
    mov  esi,int09msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_segnp :
    ; ec = seg selector
    push esi
    mov  esi,int11msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_ec_eip

align 4
int_handler_stkflt :
    ; ec = seg selector
    push esi
    mov  esi,int12msg
    call irq_print_msg
    pop  esi
    mov  ecx,4
    add  ebx,2
int_handler_show_stack_loop :
    pop  eax
    call putx_vga
    loop int_handler_show_stack_loop
    jmp  reboot_on_alt_key

align 4
int_handler_gpf :
    ; ec = various ...
    ;test dword [esp+0xc],1 << 17    ; check the eflags vm bit
    ;jnz  int_handler_vm_gpf

    push esi
    mov  esi,int13msg
    call irq_print_msg
    pop  esi
int_handler_show_ec_eip :
    mov  byte [gs:ebx],'e'
    add  ebx,2
    mov  byte [gs:ebx],'c'
    add  ebx,2
    mov  byte [gs:ebx],'='
    add  ebx,2
    pop  eax                ; ec
    call putbx_vga
    sub  ebx,2
    mov  byte [gs:ebx],' '
    add  ebx,2
int_handler_show_eip :
    mov  byte [gs:ebx],'c'
    add  ebx,2
    mov  byte [gs:ebx],'s'
    add  ebx,2
    mov  byte [gs:ebx],':'
    add  ebx,2
    mov  byte [gs:ebx],'e'
    add  ebx,2
    mov  byte [gs:ebx],'i'
    add  ebx,2
    mov  byte [gs:ebx],'p'
    add  ebx,2
    mov  byte [gs:ebx],'='
    add  ebx,2
    pop  edx                ; eip
    pop  eax                ; cs
    call putx_vga
    sub  ebx,2
    mov  byte [gs:ebx],':'
    add  ebx,2
    mov  eax,edx
    call putx_vga
    jmp  reboot_on_alt_key

int_handler_vm_gpf :
    push ebx
    ;mov  ebx,[esp+8]        ; faulter's eip
    mov  bl,[cs:ebx]        ; get the opcode that caused the fault
    cmp  bl,0xf4            ; 'hlt' is ok
    jz   int_handler_vm_normal_return

    push esi
    push eax
    push ebx
    mov  esi,intvmmsg
    call irq_print_msg
    mov  byte [gs:ebx],'o'
    add  ebx,2
    mov  byte [gs:ebx],'p'
    add  ebx,2
    mov  byte [gs:ebx],'c'
    add  ebx,2
    mov  byte [gs:ebx],'o'
    add  ebx,2
    mov  byte [gs:ebx],'d'
    add  ebx,2
    mov  byte [gs:ebx],'e'
    add  ebx,2
    mov  byte [gs:ebx],'('
    add  ebx,2
    pop  eax
    and  eax,0xff
    call putbx_vga
    add  ebx,2
    mov  byte [gs:ebx],')'
    pop  eax
    pop  esi
int_handler_vm_normal_return :
    pop  ebx

    push dword [esp+0xc]    ; eflags
    popf                    ; restore the VM and NT flags
    iret                    ; chain back via nested task
                            ; from v86 tss to original caller

align 4
int_handler_pgflt :
    pop  eax
    push eax
    test eax,0x4                        ; user mode or supervisor?
    jz   int_handler_pgflt_bad_kaddr    ; if not, kernel failed

int_handler_pgflt_bad_uaddr :
    mov  esi,int14umsg
    jmp  int_handler_pgflt_msg

int_handler_pgflt_bad_kaddr :
    mov  esi,int14kmsg

int_handler_pgflt_msg :
    call irq_print_msg
    mov  eax,cr2
    call putx_vga                       ; print the addres of the fault
    jmp  int_handler_show_ec_eip

align 4
int_handler_fpuerr :
    push esi
    mov  esi,int16msg
    call irq_print_msg
    ; FIXME flags identify error
    ;   IS - FPU stack overflow
    ;   IA - Invalid arithmetic operation
    ;   Z  - Divide by zero
    ;   D  - Source operand is a denormal number
    ;   O  - Overflow in result
    ;   U  - Underflow in result
    ;   P  - Inexact result
    jmp  int_handler_show_eip

align 4
int_handler_algnchk :
    ; ec = zero
    push esi
    mov  esi,int17msg
    call irq_print_msg
    pop  esi
    pop  eax                    ; toss the ec
    jmp  int_handler_show_eip

align 4
int_handler_machchk :
    push esi
    mov  esi,int18msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

align 4
int_handler_simdfpe :
    push esi
    mov  esi,int19msg
    call irq_print_msg
    pop  esi
    jmp  int_handler_show_eip

; indicate that an irq has been serviced
display_irq :
    push eax
    mov  ax,videosel        ; point gs at video memory
    mov  gs,ax
    mov  al,bh              ; display irq "number"
    and  ebx,0xff
    mov  [gs:ebx],al
    mov  al,[gs:ebx+1]
    inc  al
    or   al,0x8             ; avoid black and dark colors
    and  al,0xf
    mov  [gs:ebx+1],al      ; change character color
    pop  eax
    ret


ipi_cpu :
    push eax
    xor  eax,eax
;   shl  eax,24                     ; cpu (apic) number is in eax
    mov  dword [0xfee00310],eax     ; via the destination register ...
;   mov  eax,0x04800 + wakeup_int   ; no shrthnd, fixed, logical, edge

; ah, just kick everybody so each can see if there is something to do
    mov  eax,0xc4800 + wakeup_int   ; all except self, fixed, logical, edge
    mov  dword [0xfee00300],eax
    pop  eax
    ret

align 4
int_handler_timer :
    push eax
    push ebx
    push ecx
    mov  bx,('t' << 8) + 30*2
    call display_irq

    ; ---- freeze on fault: this stops all timer related activity
    ; ---- (also see ozsys.asm)
    cmp  dword [fault_count],0
    jnz  freeze_for_debug

    ; ---- wakeup any sleeping cpus (see syscall_sleep)

    ; resume handles this now ...
    ;call ipi_cpu

    ; debug
    mov eax,[ten_secs_counter]
    dec eax
    ja  dont_reset
    mov eax,100
dont_reset :
    mov [ten_secs_counter],eax
    ; debug end

freeze_for_debug :
    mov  al,0x20
    out  0x20,al            ; signal end of interrupt (eoi)
    pop  ecx
    pop  ebx
    pop  eax
    iret

align 4
int_handler_kbd :
    push ebx
    mov  bx,('k' << 8) + 31*2
    call display_irq
    pop  ebx

    push eax

    push ebx
    mov  ebx,27*2
    in   al,0x60
    push eax
    call putbx_vga
    pop  eax
    pop  ebx

    cmp  al,0x53            ; scan code for the DEL key
    jz   reboot

    mov  al,0x20
    out  0x20,al            ; signal end of interrupt (eoi)

    pop  eax
    iret

align 4
int_handler_hw02 :          ; cascade
    push ebx
    mov  bx,('c' << 8) + 32*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int34msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw03 :          ; serial port 2
    push ebx
    mov  bx,('3' << 8) + 33*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int35msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw04 :          ; serial port 1
    push ebx
    mov  bx,('4' << 8) + 34*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int36msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw05 :          ; parallel port 2 or sound card
    push ebx
    mov  bx,('5' << 8) + 35*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int37msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw06 :          ; floppy disk controller
    push ebx
    mov  bx,('6' << 8) + 36*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int38msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw07 :          ; parallel port 1
    push ebx
    mov  bx,('7' << 8) + 37*2
    call display_irq
    pop  ebx

;   push esi                ; 7 seems to happen a lot on some boxes
;   mov  esi,int39msg
;   call irq_print_msg
;   pop  esi
    mov  al,0x20
    out  0x20,al            ; signal end of interrupt (eoi)
    iret
    ;jmp  reboot_on_alt_key

align 4
int_handler_hw08 :          ; RTC
    push ebx
    mov  bx,('8' << 8) + 38*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int40msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw09 :          ; acpi
    push ebx
    mov  bx,('9' << 8) + 39*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int41msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw10 :
    push ebx
    mov  bx,('a' << 8) + 40*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int42msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw11 :
    push ebx
    mov  bx,('b' << 8) + 41*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int43msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw12 :          ; mouse
    push ebx
    mov  bx,('c' << 8) + 42*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int44msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw13 :          ; co-processor
    push ebx
    mov  bx,('d' << 8) + 43*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int45msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw14 :          ; ata disk controller primary
    push ebx
    mov  bx,('e' << 8) + 44*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int46msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

align 4
int_handler_hw15 :          ; ata disk controller secondary
    push ebx
    mov  bx,('f' << 8) + 45*2
    call display_irq
    pop  ebx

    push esi
    mov  esi,int47msg
    call irq_print_msg
    pop  esi
    jmp  reboot_on_alt_key

; called via the double fault task
align 4
int_handler_tg_dblflt :
    push esi
    mov  esi,int08msg
    call irq_print_msg
    pop  esi
    add  ebx,2
    mov  eax,cr2
    ;mov  eax,[esp]           ; print the value on the stack
    call putx_vga
    jmp  reboot_on_alt_key

; called via the invalid tss task
align 4
int_handler_tg_invtss :
    push esi
    mov  esi,int10msg
    call irq_print_msg
    pop  esi
    ; FIXME need to go back to previous tss to get ec via that stack
    add  ebx,2
    pop  eax                ; ec == invalid tss selector
    call putx_vga
    jmp  reboot_on_alt_key

align 4
int_handler_spurious :
    push esi
    mov  esi,spuriousmsg
    call irq_print_msg
    pop  esi
    iret

align 4
int_handler_apicerr :
    push esi
    mov  esi,apicerrmsg
    call irq_print_msg
    pop  esi
    iret

; serves only to eoi the fixed ipi used for sleep wakeup
align 4
wakeup :
    mov  dword [0xfee000b0],0   ; lapic eoi
    iret


align 4
; %if ($ >= 0x8000) bomb
sysent :
    cmp  eax,0x0200
    jz   syscall_klog
    cmp  eax,0x1000
    jz   syscall_ncpus
    cmp  eax,0x2000
    jz   syscall_sleep
    cmp  eax,0x2001
    jz   syscall_pause
    cmp  eax,0x2002
    jz   syscall_resume
    cmp  eax,0x2003
    jz   syscall_ipi_all
    cmp  eax,0x2100
    jz   syscall_new_thread
    cmp  eax,0x2700
    jz   syscall_request_pmem_access
    cmp  eax,0xfe00
    jz   syscall_sipi_vector
    mov  esi,int255msg
    call irq_print_msg
    xor  eax,eax
    dec  eax
    iret


; ---- IRQ support code ---- 

irq_print_msg :
    mov  eax,1
    xadd [irq_err_lno],eax
    push eax                ; remember line number
    and  eax,0x3            ; only four lines
    inc  eax                ; start with line 1
    mov  ebx,160            ; vga line length
    imul eax,ebx
    mov  ebx,eax

    mov  al,[enabled_lapic]
    or   al,al
    pop  eax
    jz   skip_cpumsg

    push esi
    push eax
    mov  esi,cpumsg
    call puts_vga
    mov  eax,[0xfee00020]   ; print our apic id
    shr  eax,24
    add  eax,'0'
    mov  [gs:ebx],al
    pop  eax                ; recover line number
    shr  al,2               ; provide a rolling effect for
    and  al,0xf             ;     unending irq messages
    or   al,0x8
    mov  [gs:ebx+1],al
    add  ebx,4
    pop  esi

skip_cpumsg :
    jmp  puts_vga


reboot_on_alt_key :
    ;inc  dword [fault_count]
    cli
reboot_on_alt_key_loop :
    in   al,0x60
    cmp  al,0x53            ; scan code for the DEL key
    jnz  reboot_on_alt_key_loop
reboot :
    lidt [reboot_idt]       ; restore boot idt (helps qemu ...)
    jmp  rmcssel:reboot_exit_pmode      ; thankyou hpa
reboot_exit_pmode :
bits 16
    mov  ax,rmdssel
    mov  ds,ax
    mov  es,ax
    mov  ss,ax
    mov  fs,ax
    mov  gs,ax

    mov  eax,cr0
    and  eax,0x7ffffffe
    mov  cr0,eax            ; disable pmode and paging
    jmp  0xffff:0           ; jump to the warm start vector and
                            ; flush the I prefetch queue all at once

reboot_idt :
    dw 0xffff
    dd 0


; ---- IDT initialization table ---- 
;
; One dw (2 bytes per vector) is used which means that all
; int_handler entry points must reside below 0x10000!
;
; And since all int_handler routines are 4 byte aligned,
; the bottom two bits are available to indicate type.
; These bits index into the irq_types table below.

align 4

; note: these are not gdt/ldt descriptors (see swdev3a 6.11 pg 228)

irq_types   db  0x8e            ; dpl=0 32bit interrupt gate (sets IF flag)
            db  0xee            ; dpl=3 32bit app (ring3) interrupt gate
            db  0x85            ; dpl=0 32bit task gate
            db  0x8f            ; dpl=0 32bit trap gate (does not set IF flag)

; all handlers/selectors are 4 byte aligned - this gives us
; two bits to use to designate 1 of 4 types of idt descriptors ...

irqt_intr   equ 0
irqt_app    equ 1
irqt_task   equ 2
irqt_trap   equ 3

irq_setup_table:
    ; cpu defined
    dw  int_handler_div0      + irqt_trap ; 0
    dw  int_handler_debug     + irqt_trap ; 1
    dw  int_handler_nmi       + irqt_intr ; 2
    dw  int_handler_brkp      + irqt_app  ; 3
    dw  int_handler_ovrflw    + irqt_app  ; 4
    dw  int_handler_bound     + irqt_app  ; 5
    dw  int_handler_invop     + irqt_trap ; 6
    dw  int_handler_devna     + irqt_trap ; 7
    dw  tasksel_f08           + irqt_task ; 8   double fault
    dw  int_handler_cpsego    + irqt_trap ; 9
    dw  tasksel_f10           + irqt_task ; 10  invalid tss
    dw  int_handler_segnp     + irqt_trap ; 11
    dw  int_handler_stkflt    + irqt_trap ; 12
    dw  int_handler_gpf       + irqt_trap ; 13
    dw  int_handler_pgflt     + irqt_intr ; 14
    dw  0                                 ; 15  Intel reserved
    dw  int_handler_fpuerr    + irqt_trap ; 16
    dw  int_handler_algnchk   + irqt_trap ; 17
    dw  int_handler_machchk   + irqt_trap ; 18
    dw  int_handler_simdfpe   + irqt_trap ; 19
    dw  0,0,0,0,0,0,0,0,0,0,0,0
    ; hw defined
apic0_irqbase equ ($ - irq_setup_table)/2
    dw  int_handler_timer     + irqt_intr ; 32  0x20
    dw  int_handler_kbd       + irqt_intr ; 33
    dw  int_handler_hw02      + irqt_intr ; 34
    dw  int_handler_hw03      + irqt_intr ; 35
    dw  int_handler_hw04      + irqt_intr ; 36
    dw  int_handler_hw05      + irqt_intr ; 37
    dw  int_handler_hw06      + irqt_intr ; 38
    dw  int_handler_hw07      + irqt_intr ; 39
apic1_irqbase equ ($ - irq_setup_table)/2
    dw  int_handler_hw08      + irqt_intr ; 40
    dw  int_handler_hw09      + irqt_intr ; 41
    dw  int_handler_hw10      + irqt_intr ; 42
    dw  int_handler_hw11      + irqt_intr ; 43
    dw  int_handler_hw12      + irqt_intr ; 44
    dw  int_handler_hw13      + irqt_intr ; 45
    dw  int_handler_hw14      + irqt_intr ; 46
    dw  int_handler_hw15      + irqt_intr ; 47  0x2f
    ; undefined - hw can expand here
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x30
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x40
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x50
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x60
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x70
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x80
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0x90
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0xa0
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0xb0
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0xc0
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; 0xd0
    dw  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0     ; 0xe0
spurious_int equ ($ - irq_setup_table)/2
    dw  int_handler_spurious  + irqt_intr
apicerr_int equ ($ - irq_setup_table)/2
    dw  int_handler_apicerr   + irqt_app
    dw    0,0,0,0,0,0,0,0,0,0,0,0,0       ; 0xf0
    ; sw defined - expand down if needed
wakeup_int equ ($ - irq_setup_table)/2
    dw  wakeup                + irqt_app
    dw  sysent                + irqt_app  ; 256  0xff
irq_setup_table_size equ ($ - irq_setup_table)/2

