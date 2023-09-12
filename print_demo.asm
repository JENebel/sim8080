        MVI     C, STR
        MVI     D, AFTER

PRNT:   LDAX    B
        OUT     0
        INR     C
        MOV     A,C
        CMP     D
        JNZ     PRNT
        HLT

STR: 
        DB      'Dette er en test!\n'
AFTER:
        END