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
    pusha
    mov  ebx,2 * 160        ; line 3
    call puts_vga
    popa
    xor  eax,eax
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
    mov  al,[enabled_lapic]     ; if the lapics are not enabled,
    or   al,al                  ; none of this is useful
    jz   new_thread_fail

    ; setup the tss
    ; slightly squirly - get the tss address from the task selector
    ; but there could be a race here: ncpus is updated before
    ; create_tss_pair is called, so the task selector could be zero!

    mov  edi,ebx
    shl  edi,4                  ; 8 byte selectors in pairs
    add  edi,tasksel_u00
    mov  esi,[gdt+edi+2]
    and  esi,0xffffff
    xor  eax,eax
    mov  al,[gdt+edi+7]
    shl  eax,24
    or   esi,eax
    jz   new_thread_fail        ; cpu isn't yet ready ...

    ; really should check the previous task link to see if this
    ; cpu is busy ...

    mov  [esi+(tss0_esp-tss0)],ecx  ; set the app's stack
    mov  [esi+(tss0_eip-tss0)],edx  ; set the ip to the entry point

    ; lookup the requested cpu's int/taskgate gdt selector

    mov  edi,ebx
    add  edi,first_thread_tss_gate  ; convert ebx to int/taskgate number

    ; ipi a cpu.  no fancy affinity scheduling yet,
    ; just let the app ask for a specific cpu to do the work

    mov  eax,0x01000000
    mov  ecx,ebx                    ; recover requested cpu number
    shl  eax,cl                     ; form the icr destination field

    ; poke the cpu that matches our thread index - we only get 8 :/

    mov  dword [0xfee00310],eax
    mov  eax,0x4800                 ; no shorthand, fixed, logical, edge
    or   eax,edi                    ; make int/taskgate number the vector
    mov  dword [0xfee00300],eax

    xor  eax,eax
    iret

new_thread_fail :
    dec  eax                    ; -1
    ret

;------------------------------------------------------------------
;   syscall_sleep : wait for N timer interrupts
;
;   entry:
;       edx = N ticks
;   exit:

syscall_sleep :
    mov  al,[enabled_lapic]
    or   al,al
    jz   sleep_loop

    mov  eax,[0xfee00020]
    shr  eax,24
    or   eax,eax
    jz   sleep_loop         ; the boot cpu services the timer int
    mov  cl,al              ; so don't add it to the sleeper list
    mov  eax,1
    shl  eax,cl

sleep_loop :
    lock or [sleepers],eax  ; announce we are sleeping
    sti
    hlt                     ; wait for an int to wake us up
    dec  edx                ; decrement the tick count
    jnz  sleep_loop

    xor  eax,-1
    lock and [sleepers],eax ; renounce sleepiness
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
    pusha
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

map_pmem_have_pgtbl :
    ; for now, no security check, just direct map the address
    ; and mark the pages and the page table r/w by all
    or   dword [ebx+edi],7      ; user, r/w, present

    mov  ebx,[ebx+edi]
    and  ebx,0xfffff000
    pop  eax                    ; recover requested phys mem addr
    ; FIXME yup, big security hole if called by ring 0! And it is ...
    or   eax,7                  ; user, r/w, present
    mov  [ebx+edx],eax          ; update page directory
    popa
    xor  eax,eax                ; 0
    ret

map_pmem_fail :
    dec  eax                    ; -1
    ret

