; OZ - A more utopian OS    x86-32 system calls
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
; OZ System Calls  (for ozapps)

bits 32

;------------------------------------------------------------------
;   syscall_klog : place a message on the vga line reserved for klog  :D
;
;   entry:
;       esi = message address
;   exit:

syscall_klog :
    push ebx
    mov  ebx,5 * 160        ; line 6
    call puts_vga
    pop  ebx
    xor  eax,eax
    iret

;------------------------------------------------------------------
;   syscall_sipi_vector : get the entry vector for non-boot cpus
;
;   entry:
;   exit:
;       eax = N cpus

syscall_sipi_vector :
    mov  eax,sipi_vector
    iret

;------------------------------------------------------------------
;   syscall_ncpus : get how many cpu threads are running
;
;   entry:
;   exit:
;       eax = N cpus

syscall_ncpus :
    mov  eax,[ncpus]
    iret

;------------------------------------------------------------------
;   syscall_new_thread : ask for another cpu to execute code in
;                        the current app
;
;   This implementation is quite a bit of a hack - apps can basically
;   commendere any specific non-boot cpu they want ...
;
;   entry:
;       edx = function start address in the app
;       ecx = address of the top of the app's new thread stack
;       ebx = app's new thread index
;   exit:
;       eax = 0 success, -1 failure

syscall_new_thread :
    or   ebx,ebx
    jz   new_thread_fail        ; don't get to ask for cpu 0

    xor  eax,eax
    cmp  ebx,((gdt_end - gdt_tasks) / 8 / 2)    ; / sizeof(gdt) / 2 per task
    jae  new_thread_fail        ; we don't have enough gdt entries

    ; setup the tss
    ; slightly squirly - get the tss address from the task selector

    mov  edi,ebx
    shl  edi,4                  ; 8 byte selectors in pairs
    add  edi,tasksel_u00
    mov  esi,[gdt+edi+2]        ; get the tss base address
    and  esi,0xffffff
    mov  al,[gdt+edi+7]
    shl  eax,24
    or   esi,eax
    jz   new_thread_fail        ; task is not yet initialized ...

    ; really should check the previous task link to see if this
    ; cpu is busy ...

    mov  [esi+(tss_esp-tss)],ecx    ; set the app's stack
    mov  [esi+(tss_eip-tss)],edx    ; set the eip to the entry point
    ; ********* set the eip LAST as this unleashes the associated cpu *********

    ; like to eventually ipi only the cpu we just configured for this thread,
    ; but need to figure out how to ipi a single cpu beyond the 8th ...
    ; for now we have to ipi them all at once: the ones in nb_idle with
    ; a new tss will start

    ; and there is no fancy affinity scheduling yet, just let the app
    ; ask for a specific cpu to do the work

    xor  eax,eax
    iret

new_thread_fail :
    dec  eax                    ; -1
    iret

;------------------------------------------------------------------
;   syscall_sleep : wait for N timer interrupts
;
;   entry:
;       edx = N ticks
;   exit:

;n_to_bits :
;    db  00h
;    db  01h
;    db  02h
;    db  04h
;    db  08h
;    db  10h
;    db  20h
;    db  40h
;    db  80h

syscall_sleep :
    mov  al,[enabled_lapic]
    or   al,al
    jz   sleep_loop

; no need for any of this at the moment ...
;   mov  ecx,sleepers
;   xor  ebx,ebx
;   mov  eax,[0xfee00020]
;   shr  eax,24
;   or   eax,eax
;   jz   sleep_loop         ; the boot cpu services the timer int
;   mov  ebx,eax            ; so don't add it to the sleeper list
;   shr  ebx,3
;   and  eax,7h
;   mov  al,[n_to_bits+eax]

sleep_loop :
;   lock or [sleepers+ebx],al   ; announce we are sleeping
    sti
    hlt                     ; wait for an int to wake us up
    dec  edx                ; decrement the tick count
    jnz  sleep_loop

    ; ---- freeze on a fault: stops all timer related activity
    ; ---- and hangs the boot cpu (also see ozirq.asm)
    cmp  dword [fault_count],0
    jnz  sleep_loop

    xor  eax,-1
;   lock and [sleepers+ebx],al  ; renounce sleepiness
    iret

;------------------------------------------------------------------
;   syscall_pause : wait for a resume

syscall_pause :

%ifdef HAVE_MWAIT

    ; two paths through pause, the first forces all cores to synchronize
    ; with wait_addr (using a loop to test the contents of wait_addr),
    ; the other simply uses a single instance of monitor/mwait.
    ; apparently synchronizing on the first resume allows subsequent
    ; mwaits to work correctly - if mwait is used alone without this "sync"
    ; that mwait only rarely works (for like 1 core out of 47).
    ; note that implementation wise pause/resume here only syncs once
    ; the first time it is called by the first app (nothing resets wait_addr to zero),
    ; but it is still unknown if more that one sync is needed.

    mov  edi,[wait_addr]
    cmp  edi,edi
    jnz  no_loop            ; first pause?

    ; sync all the cores together the first time through
    ; after this mwait works reliably

    cli                     ; this would kill "pre-emptive multithreading"
                            ; except that is is not implemented  :D
                            ; only one app runs at a time here so no big deal
false_alarm :
    mov  eax,wait_addr
    ;mfence                  ; workaround for certain Intel P6 family models
    ;clflush [eax]           ; (see X86_BUG_CLFLUSH_MONITOR in linux kernel)
    ;mfence

    xor  ebx,ebx
    xor  ecx,ecx
    xor  edx,edx
    monitor
    ;xor  ecx,ecx
    mov  ecx,1              ; dont let interrupts break mwait (req: cpuid.5:ecx bit1)
    mov  eax,(5<<4)         ; ask for C6
    ;mov  eax,(1<<4)         ; ask for C2
    ;xor  eax,eax            ; use for C1
    mwait

    cmp  edi,[wait_addr]
    jz   false_alarm

    iret

no_loop :
    xor  ebx,ebx
    xor  ecx,ecx
    xor  edx,edx
    monitor
    ;xor  ecx,ecx
    mov  ecx,1              ; dont let interrupts break mwait (req: cpuid.5:ecx bit1)
    mov  eax,(5<<4)         ; ask for C6
    ;mov  eax,(1<<4)         ; ask for C2
    ;xor  eax,eax            ; use for C1
    mwait

    iret

%else
    sti
    hlt                     ; wait for an int to wake us up
    iret

%endif

;------------------------------------------------------------------
;   syscall_resume : unpause every core

syscall_resume :
%ifdef HAVE_MWAIT
    mov  eax,wait_addr
    lock inc dword [eax]
%else
    call ipi_cpu
%endif
    iret

;------------------------------------------------------------------
;   syscall_ipi_all : kick every core out of hlt

syscall_ipi_all :
    call ipi_cpu
    iret

;------------------------------------------------------------------
;   syscall_request_pmem_access : ask for access to a physical
;            memory address
;
;   entry:
;       edx = phys memory address
;
;   exit:
;       (need to return a logical address in eax)  For now just
;       direct maps the physical address into logical addr space.

syscall_request_pmem_access :
    call map_pmem
    iret

;------------------------------------------------------------------
;   map_pmem - add entries to the current page directory/table
;              to direct map the requested physical memory address
;              (logical identical to physical for now)
;
;   entry:
;       edx = phys memory address
;
;   returns:
;       (need to return a logical address in eax)  For now just
;       direct maps the physical address into logical addr space.
;       eax =  0 success
;       eax = -1 fail

map_pmem :
    push ebx
    push edi

    invlpg [edx]                ; necessary on real Intel hw
    push edx
    mov  ebx,cr3
    and  ebx,0xfffff000
    mov  edi,edx
    and  edi,0xffc00000
    shr  edi,(22 - 2)           ; calc page directory index
    and  edx,0x003ff000
    shr  edx,(12 - 2)           ; calc page table index

    mov  eax,[ebx+edi]
    or   eax,eax                ; is there a page table here?
    jnz  map_pmem_have_pgtbl
    call mem_alloc_kernel_page
    or   eax,eax
    jz   map_pmem_fail
    shl  eax,12                 ; convert pgno to pgtbl entry
    mov  [ebx+edi],eax          ; update page table
    invlpg [ebx+edi]

map_pmem_have_pgtbl :
    ; for now, no security check, just direct map the address
    ; and mark the pages and the page table r/w by all
    ;or   dword [ebx+edi],7      ; user, r/w, present
    ; assume for now that all map_pmem calls are for real devices
    or   dword [ebx+edi],1fh     ; cache disable, write through, user, r/w, present

    mov  ebx,[ebx+edi]
    and  ebx,0xfffff000
    pop  eax                    ; recover requested phys mem addr
    ; FIXME yup, big security hole if called by ring 0! And it is ...
    ;or   eax,7                  ; user, r/w, present
    or   eax,1fh                ; cache disable, write through, user, r/w, present
    mov  [ebx+edx],eax          ; update page directory
    invlpg [ebx+edx]
    xor  eax,eax                ; 0
map_pmem_exit :
    pop  edi
    pop  ebx
    ret

map_pmem_fail :
    dec  eax                    ; -1
    jmp  map_pmem_exit

