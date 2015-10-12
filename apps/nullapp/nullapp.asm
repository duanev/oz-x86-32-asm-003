; OZ - A more utopian OS
; ex: set expandtab softtabstop=4 shiftwidth=4 nowrap :
;
;
;       nullapp - marks the end of OZ startup apps
;
;
; requires: nasm-2.07  or later from: http://www.nasm.us
;
; contributors:
;        djv - Duane Voth
;
; history:
; 2015/09/18 - 0.00.01 - djv - original

bits 32
org 0x400000                    ; ozapp program load address

; ozapp header

header :
    db  0,0,0,0,0,0,0,0
    dw  0
    dw  0
    dw  0
    dw  0
    dq  0
    dq  0

    ; all boot apps attached to the kernel must be
    ; a multiple of a page in length.

;   align 4096

;   times 4096-($-$$) db 0x90

;end :

