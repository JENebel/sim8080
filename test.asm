 ; This is a test
; comment

        JMP     1000
FUCK    EQU     0
        DB      1, 5, 3, 4, 5, 6, 7, 8, 9, 10

        ORG     1000

LBL1:   MVI     B, 7
        MOV     B, A
        OUT     0
        
        HLT
        END
; 1. 