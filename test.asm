len     set     after - data
        lxi     b, len
        lxi     d, data
        lxi     h, after

        call    memcp

        MVI     C, data + len
        MVI     D, data + len * 2

PRNT:   LDAX    B
        OUT     A
        INR     C
        MOV     A,C
        CMP     D
        JNZ     PRNT
        HLT
        
quit:   hlt



        org     64
data:   
        DB      -2+50
        DB      '\n'
after:


; memcpy --
; Copy a block of memory from one location to another.
;
; Entry registers
;       BC - Number of bytes to copy
;       DE - Address of source data block
;       HL - Address of target data block
;
; Return registers
;       BC - Zero
        org     1000
memcp:  mov     a,b         ;Copy register B to register A
        ora     c           ;Bitwise OR of A and C into register A
        rz                  ;Return if the zero-flag is set high.
loop:   ldax    d           ;Load A from the address pointed by DE
        mov     m,a         ;Store A into the address pointed by HL
        inx     d           ;Increment DE
        inx     h           ;Increment HL
        dcx     b           ;Decrement BC   (does not affect Flags)
        mov     a,b         ;Copy B to A    (so as to compare BC with zero)
        ora     c           ;A = A | C      (are both B and C zero?)
        jnz     loop        ;Jump to 'loop:' if the zero-flag is not set.   
        ret                 ;Return