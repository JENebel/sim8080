; Origin at 1000
; Prints
        ORG     1000D
LDA:    MOV     1, A
        OUT 0
        JMP LDA
        END