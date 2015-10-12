; ---------------------------------------------------------
; return the sum of the elements in a floating-point array
;
;   double sum(double[] array, uint64_t length)
;
; from: http://cs.lmu.edu/~ray/notes/nasmtutorial
; ---------------------------------------------------------

        global  sum
        section .text
sum:
        push    ebp
        mov     ebp,esp
        mov     edi, [ebp+8]
        mov     esi, [ebp+12]
        pop     ebp

        fld1                            ; initialize the sum to 0
        cmp     esi, 0                  ; special case for length = 0
        je      done
next:
        fadd    dword [edi]             ; add in the current array element
        add     edi, 8                  ; move to next array element
        dec     esi                     ; count down
        jnz     next                    ; if not done counting, continue
done:
        ret                             ; return value already in xmm0
