; OZ - A more utopian OS
; ex: set expandtab softtabstop=4 shiftwidth=4 nowrap :
;
;
;       initialize x86 cpus -- Semetric Multi Processing start up
;
;
; requires: nasm-2.07  or later from: http://www.nasm.us
;
; contributors:
;        djv - Duane Voth
;
; history:
; 2009/09/04 - 0.00.01 - djv - create an ozapp asm example

[map symbols initsmp.map]

bits 32
org 0x400000                    ; ozapp program load address

; ozapp header

header :
    db  "ozapp",0,0,0
    dw  15              ; arch = x86
    dw  31              ; subarch = oz protected mode, 4gb flat
    dw  0
    dw  0
    dq  end             ;end-header
    dq  start

; r/o data and code - the first 4k -------------------------------

announce   db "initsmp: ", 0
cpu486     db "486 or lower", 0
cpu586     db "586,", 0
cpuintl    db "intel,", 0
cpuamd     db "amd,", 0
cpuxxx     db "unknown vendor", 0
cpunoid    db "no cpuid", 0
cpuapic    db "apic,id=", 0
cpunoapic  db "no apic,", 0
cpulapic   db "lapic,", 0
cpunolapic db "no lapic", 0
cpux2apix  db "x2apic,", 0
cpuvmx     db "vmx-", 0
cpuavil    db "available", 0
cpuunavil  db "unavailable", 0
cpuzzz     db "-", 0

; ----------------------------
;    puts - write a null delimited string to a buffer
;
;    enter:
;         esi - address of string
;         ebx - address of output buffer
;    exit:
;         eax - destroyed
;         ebx - next location in output buffer

puts :
puts_loop :
    lodsb
    cmp  al,0
    jz   puts_done
    mov  [ebx],al
    add  ebx,1
    jmp  puts_loop

puts_done :
    ret

; ----------------------------
;    putx  - write the contents of eax in hex to a buffer (8 digits)
;    putbx - write the contents of  al in hex to a buffer (2 digits)
;
;    enter:
;         eax - value to convert to hex
;         ebx - address of output buffer
;    exit:
;         ebx - next location in output buffer
;         ecx - destroyed

putbx :
    mov  ecx,2
    and  eax,0xff
    rol  eax,24
    jmp putx_loop

putx :
    mov  ecx,8

putx_loop :
    rol  eax,4
    push eax
    and  al,0xf
    cmp  al,9
    ja   putx_hexdigit
    add  al,'0'
    jmp short putx_putc

putx_hexdigit :
    add  al,'a'-10

putx_putc :
    mov  [ebx],al
    add  ebx,1
    pop  eax
    and  eax,0xfffffff0
    loop putx_loop
    mov  byte [ebx],','
    add  ebx,1
    ret

; ----------------------------
;    wait for ipi to complete
;
;    exit:
;       all regs preserved (except flags)

ipi_wait :
    push edx
    push eax

ipi_delay_loop :
    ; always delay one tick
    mov  eax,0x2000         ; oz syscall - sleep 1 tick
    mov  edx,1
    int  0xff

    mov  eax,[0xfee00300]               ; seen 0x0c0602, 0x0c4607
    bt   eax,12                         ; watch delivery status
    jc   ipi_delay_loop

ipi_delay_done :
    pop  eax
    pop  edx
    ret


; ----------------------------
;   main

start :

    mov  ebx,print_buf
    mov  esi,announce
    call puts
    push ebx

    ; ---- check on our processor type

    pushfd
    pop  eax                ; get flags
    mov  ebx,eax            ; save
    mov  ecx,1 << 21        ; ID flag (bit 21)
    xor  eax,ecx
    push eax
    popfd
    pushfd
    pop  eax
    push ebx
    popfd                   ; restore flags
    and  ebx,ecx
    and  eax,ecx
    cmp  eax,ebx            ; if flag is stuck, then 486
    pop  ebx
    mov  esi,cpu486
    jz   cpuid_last_puts
    mov  esi,cpu586
    call puts
    push ebx

    xor  eax,eax
    cpuid                   ; its at least a Pentium, use cpuid
    mov  [level],eax
    mov  [id_buf],ebx
    mov  [id_buf+4],edx
    mov  [id_buf+8],ecx

    mov  esi,cpuamd
    cmp  ecx,0x444d4163     ; test for 'cAMD' in ecx
    jz   cpu_recognized
    mov  esi,cpuintl
    cmp  ecx,0x6c65746e     ; test for 'ntel' in ecx
    jz   cpu_recognized
    pop  ebx
    mov  esi,id_buf
    jmp  cpuid_last_puts

cpu_recognized :
    pop  ebx
    call puts

    push ebx
    mov  eax,1
    cpuid
    mov  [feature_ebx],ebx
    mov  [feature_ecx],ecx  ; save feature flags
    mov  [feature_edx],edx
    cmp  eax,1
    mov  esi,cpunoid
    pop  ebx
    jb   cpuid_last_puts

    push ebx
    and  edx,1 << 9         ; test for apic feature
    mov  esi,cpunoapic
    pop  ebx
    jz   cpuid_last_puts
    mov  esi,cpuapic
    call puts

    mov  eax,[feature_ebx]
    shr  eax,24             ; mask off initial apic ID
    call putbx

    ; ---- mtrr for 0xfee00000 -> strong uncachable (UC)

    ; ---- access the local APIC

    push ebx
    mov  eax,[0xfee00030]
    mov  ecx,eax
    and  eax,0xf0           ; see if it is a local apic
    cmp  eax,0x10
    pop  ebx
    mov  esi,cpunolapic
    jnz  cpuid_last_puts
    mov  esi,cpulapic
    call puts

    mov  eax,[feature_ecx]
    and  eax,1 << 21        ; x2apic feature bit
    jz   no_x2apic
    mov  esi,cpux2apix
    call puts
no_x2apic :

    mov  eax,ecx
    call putx               ; Athlon +1600 says 0x00040010
                            ; Bochs        says 0x00050010
                            ; real intel   says 0x00050014
                            ; qemu         says 0x00050014
                            ; real amd     says 0x80050014
                            ; recent intel says 0x01060015

;   mov  ecx,0x1b
;   rdmsr
;   and  eax,0x100          ; are we the bootstrap processor?
;   jz   i_am_non_boot_cpu

    ; ---- restart other cpus (see swdev3a, sec 10.7, pg 484 / 10-45)

    push ebx

    mov  dword [0xfee00300],0x000c0500  ; INIT (physical, fixed, excluding self)
    call ipi_wait

    mov  eax,0xfe00         ; oz syscall - get sipi_vector
    int  0xff
    shr  eax,12

    or   eax,0xc4600        ; STARTUP
    mov  dword [0xfee00300],eax
    call ipi_wait

;   pop  ebx
;   mov  eax,ecx
;   call putx
;   mov  byte [ebx],'*'
;   inc  ebx
;   push ebx

    ; ---- report on the expected number of logical processors in each core

    mov  eax,[feature_ebx]
    shr  eax,16
    and  eax,0xff           ; number of threads per core
    add  eax,'0'
    pop  ebx
    mov  [ebx],al
    inc  ebx
    mov  byte [ebx],';'
    inc  ebx

    ; ---- check for vmx support

    mov  eax,[feature_ecx]
    call putx

    mov  eax,[feature_ecx]
    and  eax,1 << 5         ; vmx feature bit
    jz   no_vmx
    mov  esi,cpuvmx
    call puts
no_vmx :

; can't rdmsr from ring 3
;   mov  ecx,0x3a           ; IA32_FEATURE_CONTROL_MSR
;   rdmsr
;   call putx

    ; cpuunavil

    ; ---- get the topology

    cmp  dword [level],0xb
    jb   no_topology
    push ebx
    mov  eax,0xb
    xor  ecx,ecx
    cpuid
    ; ....
    pop  ebx

no_topology :

    jmp  cpuid_end

i_am_non_boot_cpu :
    mov  esi,cpuzzz

cpuid_last_puts :
    call puts
cpuid_end :

    mov  eax,0x0200         ; oz syscall opcode - klog print
    mov  esi,print_buf
    int  0xff

    iret

;   align 4096

    times 4096-($-$$) db 0x00


; r/w data - the second 4k ---------------------------------------
section .data

print_buf :                     ; 0x401000 the buffer to be written

    times 512 db 0x00

level       dd 0
feature_ebx dd 0                ; saved feature flags
feature_ecx dd 0
feature_edx dd 0

id_buf :                        ; 0x401240

    ; all boot apps attached to the kernel must be
    ; a multiple of a page in length.  this space
    ; is also used for the application stack

    times 4096*1-($-$$) db 0x00

end :

