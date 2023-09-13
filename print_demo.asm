LEN     SET     AFTER - STR
        MVI     C, STR
        MVI     D, AFTER

PRNT:   LDAX    B
        OUT     A
        INR     C
        MOV     A,C
        CMP     D
        JNZ     PRNT
        HLT

STR: 
        DB      'Dette er en test!\n'
AFTER:
        END