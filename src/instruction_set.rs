use super::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Instruction {
    NOP,
    /// Load 16 bits immediate
    /// 
    /// B <- byte 3, C <- byte 2
    LXI(Register),
    STAX(Register),
    INX(Register),
    INR(Register),
    DCR(Register),
    MVI(Register),
    RLC,
    DAD(Register),
    LDAX(Register),
    DCX(Register),
    RRC,
    RAL,
    RAR,
    SHLD,
    DAA,
    LHLD,
    CMA,
    STA,
    STC,
    LDA,
    CMC,
    MOV(Register, Register),
    HLT,
    ADD(Register),
    ADC(Register),
    SUB(Register),
    SBB(Register),
    ANA(Register),
    XRA(Register),
    ORA(Register),
    CMP(Register),
    RNZ,
    POP(Register),
    JNZ,
    JMP,
    CNZ,
    PUSH(Register),
    ADI,
    RST(u8),
    RZ,
    RET,
    JZ,
    CZ,
    CALL,
    ACI,
    RNC,
    JNC,
    OUT,
    CNC,
    SUI,
    RC,
    JC,
    IN,
    CC,
    SBI,
    RPO,
    JPO,
    XTHL,
    CPO,
    ANI,
    RPE,
    PCHL,
    JPE,
    XCHG,
    CPE,
    XRI,
    RP,
    JP,
    DI,
    CP,
    ORI,
    RM,
    SPHL,
    JM,
    EI,
    CM,
    CPI,
}

impl From<u8> for Instruction {
    fn from(value: u8) -> Self {
        use Instruction::*;
        use Register::*;
        match value {
            0x00 => NOP,
            0x01 => LXI(B),
            0x02 => STAX(B),
            0x03 => INX(B),
            0x04 => INR(B),
            0x05 => DCR(B),
            0x06 => MVI(B),
            0x07 => RLC,
            0x08 => NOP,
            0x09 => DAD(B),
            0x0a => LDAX(B),
            0x0b => DCX(B),
            0x0c => INR(C),
            0x0d => DCR(C),
            0x0e => MVI(C),
            0x0f => RRC,
            0x10 => NOP,
            0x11 => LXI(D),
            0x12 => STAX(D),
            0x13 => INX(D),
            0x14 => INR(D),
            0x15 => DCR(D),
            0x16 => MVI(D),
            0x17 => RAL,
            0x18 => NOP,
            0x19 => DAD(D),
            0x1a => LDAX(D),
            0x1b => DCX(D),
            0x1c => INR(E),
            0x1d => DCR(E),
            0x1e => MVI(E),
            0x1f => RAR,
            0x20 => NOP,
            0x21 => LXI(H),
            0x22 => SHLD,
            0x23 => INX(H),
            0x24 => INR(H),
            0x25 => DCR(H),
            0x26 => MVI(H),
            0x27 => DAA,
            0x28 => NOP,
            0x29 => DAD(H),
            0x2a => LHLD,
            0x2b => DCX(H),
            0x2c => INR(L),
            0x2d => DCR(L),
            0x2e => MVI(L),
            0x2f => CMA,
            0x30 => NOP,
            0x31 => LXI(SP),
            0x32 => STA,
            0x33 => INX(SP),
            0x34 => INR(M),
            0x35 => DCR(M),
            0x36 => MVI(M),
            0x37 => STC,
            0x38 => NOP,
            0x39 => DAD(SP),
            0x3a => LDA,
            0x3b => DCX(SP),
            0x3c => INR(A),
            0x3d => DCR(A),
            0x3e => MVI(A),
            0x3f => CMC,
            0x40 => MOV(B, B),
            0x41 => MOV(B, C),
            0x42 => MOV(B, D),
            0x43 => MOV(B, E),
            0x44 => MOV(B, H),
            0x45 => MOV(B, L),
            0x46 => MOV(B, M),
            0x47 => MOV(B, A),
            0x48 => MOV(C, B),
            0x49 => MOV(C, C),
            0x4a => MOV(C, D),
            0x4b => MOV(C, E),
            0x4c => MOV(C, H),
            0x4d => MOV(C, L),
            0x4e => MOV(C, M),
            0x4f => MOV(C, A),
            0x50 => MOV(D, B),
            0x51 => MOV(D, C),
            0x52 => MOV(D, D),
            0x53 => MOV(D, E),
            0x54 => MOV(D, H),
            0x55 => MOV(D, L),
            0x56 => MOV(D, M),
            0x57 => MOV(D, A),
            0x58 => MOV(E, B),
            0x59 => MOV(E, C),
            0x5a => MOV(E, D),
            0x5b => MOV(E, E),
            0x5c => MOV(E, H),
            0x5d => MOV(E, L),
            0x5e => MOV(E, M),
            0x5f => MOV(E, A),
            0x60 => MOV(H, B),
            0x61 => MOV(H, C),
            0x62 => MOV(H, D),
            0x63 => MOV(H, E),
            0x64 => MOV(H, H),
            0x65 => MOV(H, L),
            0x66 => MOV(H, M),
            0x67 => MOV(H, A),
            0x68 => MOV(L, B),
            0x69 => MOV(L, C),
            0x6a => MOV(L, D),
            0x6b => MOV(L, E),
            0x6c => MOV(L, H),
            0x6d => MOV(L, L),
            0x6e => MOV(L, M),
            0x6f => MOV(L, A),
            0x70 => MOV(M, B),
            0x71 => MOV(M, C),
            0x72 => MOV(M, D),
            0x73 => MOV(M, E),
            0x74 => MOV(M, H),
            0x75 => MOV(M, L),
            0x76 => HLT,
            0x77 => MOV(M, A),
            0x78 => MOV(A, B),
            0x79 => MOV(A, C),
            0x7a => MOV(A, D),
            0x7b => MOV(A, E),
            0x7c => MOV(A, H),
            0x7d => MOV(A, L),
            0x7e => MOV(A, M),
            0x7f => MOV(A, A),
            0x80 => ADD(B),
            0x81 => ADD(C),
            0x82 => ADD(D),
            0x83 => ADD(E),
            0x84 => ADD(H),
            0x85 => ADD(L),
            0x86 => ADD(M),
            0x87 => ADD(A),
            0x88 => ADC(B),
            0x89 => ADC(C),
            0x8a => ADC(D),
            0x8b => ADC(E),
            0x8c => ADC(H),
            0x8d => ADC(L),
            0x8e => ADC(M),
            0x8f => ADC(A),
            0x90 => SUB(B),
            0x91 => SUB(C),
            0x92 => SUB(D),
            0x93 => SUB(E),
            0x94 => SUB(H),
            0x95 => SUB(L),
            0x96 => SUB(M),
            0x97 => SUB(A),
            0x98 => SBB(B),
            0x99 => SBB(C),
            0x9a => SBB(D),
            0x9b => SBB(E),
            0x9c => SBB(H),
            0x9d => SBB(L),
            0x9e => SBB(M),
            0x9f => SBB(A),
            0xa0 => ANA(B),
            0xa1 => ANA(C),
            0xa2 => ANA(D),
            0xa3 => ANA(E),
            0xa4 => ANA(H),
            0xa5 => ANA(L),
            0xa6 => ANA(M),
            0xa7 => ANA(A),
            0xa8 => XRA(B),
            0xa9 => XRA(C),
            0xaa => XRA(D),
            0xab => XRA(E),
            0xac => XRA(H),
            0xad => XRA(L),
            0xae => XRA(M),
            0xaf => XRA(A),
            0xb0 => ORA(B),
            0xb1 => ORA(C),
            0xb2 => ORA(D),
            0xb3 => ORA(E),
            0xb4 => ORA(H),
            0xb5 => ORA(L),
            0xb6 => ORA(M),
            0xb7 => ORA(A),
            0xb8 => CMP(B),
            0xb9 => CMP(C),
            0xba => CMP(D),
            0xbb => CMP(E),
            0xbc => CMP(H),
            0xbd => CMP(L),
            0xbe => CMP(M),
            0xbf => CMP(A),
            0xc0 => RNZ,
            0xc1 => POP(B),
            0xc2 => JNZ,
            0xc3 => JMP,
            0xc4 => CNZ,
            0xc5 => PUSH(B),
            0xc6 => ADI,
            0xc7 => RST(0),
            0xc8 => RZ,
            0xc9 => RET,
            0xca => JZ,
            0xcb => NOP,
            0xcc => CZ,
            0xcd => CALL,
            0xce => ACI,
            0xcf => RST(1),
            0xd0 => RNC,
            0xd1 => POP(D),
            0xd2 => JNC,
            0xd3 => OUT,
            0xd4 => CNC,
            0xd5 => PUSH(D),
            0xd6 => SUI,
            0xd7 => RST(2),
            0xd8 => RC,
            0xd9 => NOP,
            0xda => JC,
            0xdb => IN,
            0xdc => CC,
            0xdd => NOP,
            0xde => SBI,
            0xdf => RST(3),
            0xe0 => RPO,
            0xe1 => POP(H),
            0xe2 => JPO,
            0xe3 => XTHL,
            0xe4 => CPO,
            0xe5 => PUSH(H),
            0xe6 => ANI,
            0xe7 => RST(4),
            0xe8 => RPE,
            0xe9 => PCHL,
            0xea => JPE,
            0xeb => XCHG,
            0xec => CPE,
            0xed => NOP,
            0xee => XRI,
            0xef => RST(5),
            0xf0 => RP,
            0xf1 => POP(PSW),
            0xf2 => JP,
            0xf3 => DI,
            0xf4 => CP,
            0xf5 => PUSH(PSW),
            0xf6 => ORI,
            0xf7 => RST(6),
            0xf8 => RM,
            0xf9 => SPHL,
            0xfa => JM,
            0xfb => EI,
            0xfc => CM,
            0xfd => NOP,
            0xfe => CPI,
            0xff => RST(7),
        }
    }
}

impl Into<u8> for Instruction {
    fn into(self) -> u8 {
        use Register::*;
        use Instruction::*;

        match self {
            NOP => 0x00,
            LXI(B) => 0x01,
            STAX(B) => 0x02,
            INX(B) => 0x03,
            INR(B) => 0x04,
            DCR(B) => 0x05,
            MVI(B) => 0x06,
            RLC => 0x07,
            DAD(B) => 0x09,
            LDAX(B) => 0x0a,
            DCX(B) => 0x0b,
            INR(C) => 0x0c,
            DCR(C) => 0x0d,
            MVI(C) => 0x0e,
            RRC => 0x0f,
            LXI(D) => 0x11,
            STAX(D) => 0x12,
            INX(D) => 0x13,
            INR(D) => 0x14,
            DCR(D) => 0x15,
            MVI(D) => 0x16,
            RAL => 0x17,
            DAD(D) => 0x19,
            LDAX(D) => 0x1a,
            DCX(D) => 0x1b,
            INR(E) => 0x1c,
            DCR(E) => 0x1d,
            MVI(E) => 0x1e,
            RAR => 0x1f,
            LXI(H) => 0x21,
            SHLD => 0x22,
            INX(H) => 0x23,
            INR(H) => 0x24,
            DCR(H) => 0x25,
            MVI(H) => 0x26,
            DAA => 0x27,
            DAD(H) => 0x29,
            LHLD => 0x2a,
            DCX(H) => 0x2b,
            INR(L) => 0x2c,
            DCR(L) => 0x2d,
            MVI(L) => 0x2e,
            CMA => 0x2f,
            LXI(SP) => 0x31,
            STA => 0x32,
            INX(SP) => 0x33,    
            INR(M) => 0x34,
            DCR(M) => 0x35,
            MVI(M) => 0x36,
            STC => 0x37,
            DAD(SP) => 0x39,
            LDA => 0x3a,
            DCX(SP) => 0x3b,
            INR(A) => 0x3c,
            DCR(A) => 0x3d,
            MVI(A) => 0x3e,
            CMC => 0x3f,
            MOV(B, B) => 0x40,
            MOV(B, C) => 0x41,
            MOV(B, D) => 0x42,
            MOV(B, E) => 0x43,
            MOV(B, H) => 0x44,
            MOV(B, L) => 0x45,
            MOV(B, M) => 0x46,
            MOV(B, A) => 0x47,
            MOV(C, B) => 0x48,
            MOV(C, C) => 0x49,
            MOV(C, D) => 0x4a,
            MOV(C, E) => 0x4b,
            MOV(C, H) => 0x4c,
            MOV(C, L) => 0x4d,
            MOV(C, M) => 0x4e,
            MOV(C, A) => 0x4f,
            MOV(D, B) => 0x50,
            MOV(D, C) => 0x51,
            MOV(D, D) => 0x52,
            MOV(D, E) => 0x53,
            MOV(D, H) => 0x54,
            MOV(D, L) => 0x55,
            MOV(D, M) => 0x56,
            MOV(D, A) => 0x57,
            MOV(E, B) => 0x58,
            MOV(E, C) => 0x59,
            MOV(E, D) => 0x5a,
            MOV(E, E) => 0x5b,
            MOV(E, H) => 0x5c,
            MOV(E, L) => 0x5d,
            MOV(E, M) => 0x5e,
            MOV(E, A) => 0x5f,
            MOV(H, B) => 0x60,
            MOV(H, C) => 0x61,
            MOV(H, D) => 0x62,
            MOV(H, E) => 0x63,
            MOV(H, H) => 0x64,
            MOV(H, L) => 0x65,
            MOV(H, M) => 0x66,
            MOV(H, A) => 0x67,
            MOV(L, B) => 0x68,
            MOV(L, C) => 0x69,
            MOV(L, D) => 0x6a,
            MOV(L, E) => 0x6b,
            MOV(L, H) => 0x6c,
            MOV(L, L) => 0x6d,
            MOV(L, M) => 0x6e,
            MOV(L, A) => 0x6f,
            MOV(M, B) => 0x70,
            MOV(M, C) => 0x71,
            MOV(M, D) => 0x72,
            MOV(M, E) => 0x73,
            MOV(M, H) => 0x74,
            MOV(M, L) => 0x75,
            HLT => 0x76,
            MOV(M, A) => 0x77,
            MOV(A, B) => 0x78,
            MOV(A, C) => 0x79,
            MOV(A, D) => 0x7a,
            MOV(A, E) => 0x7b,
            MOV(A, H) => 0x7c,
            MOV(A, L) => 0x7d,
            MOV(A, M) => 0x7e,
            MOV(A, A) => 0x7f,
            ADD(B) => 0x80,
            ADD(C) => 0x81,
            ADD(D) => 0x82,
            ADD(E) => 0x83,
            ADD(H) => 0x84,
            ADD(L) => 0x85,
            ADD(M) => 0x86,
            ADD(A) => 0x87,
            ADC(B) => 0x88,
            ADC(C) => 0x89,
            ADC(D) => 0x8a,
            ADC(E) => 0x8b,
            ADC(H) => 0x8c,
            ADC(L) => 0x8d,
            ADC(M) => 0x8e,
            ADC(A) => 0x8f,
            SUB(B) => 0x90,
            SUB(C) => 0x91,
            SUB(D) => 0x92,
            SUB(E) => 0x93,
            SUB(H) => 0x94,
            SUB(L) => 0x95,
            SUB(M) => 0x96,
            SUB(A) => 0x97,
            SBB(B) => 0x98,
            SBB(C) => 0x99,
            SBB(D) => 0x9a,
            SBB(E) => 0x9b,
            SBB(H) => 0x9c,
            SBB(L) => 0x9d,
            SBB(M) => 0x9e,
            SBB(A) => 0x9f,
            ANA(B) => 0xa0,
            ANA(C) => 0xa1,
            ANA(D) => 0xa2,
            ANA(E) => 0xa3,
            ANA(H) => 0xa4,
            ANA(L) => 0xa5,
            ANA(M) => 0xa6,
            ANA(A) => 0xa7,
            XRA(B) => 0xa8,
            XRA(C) => 0xa9,
            XRA(D) => 0xaa,
            XRA(E) => 0xab,
            XRA(H) => 0xac,
            XRA(L) => 0xad,
            XRA(M) => 0xae,
            XRA(A) => 0xaf,
            ORA(B) => 0xb0,
            ORA(C) => 0xb1,
            ORA(D) => 0xb2,
            ORA(E) => 0xb3,
            ORA(H) => 0xb4,
            ORA(L) => 0xb5,
            ORA(M) => 0xb6,
            ORA(A) => 0xb7,
            CMP(B) => 0xb8,
            CMP(C) => 0xb9,
            CMP(D) => 0xba,
            CMP(E) => 0xbb,
            CMP(H) => 0xbc,
            CMP(L) => 0xbd,
            CMP(M) => 0xbe,
            CMP(A) => 0xbf,
            RNZ => 0xc0,
            POP(B) => 0xc1,
            JNZ => 0xc2,
            JMP => 0xc3,
            CNZ => 0xc4,
            PUSH(B) => 0xc5,
            ADI => 0xc6,
            RST(0) => 0xc7,
            RZ => 0xc8,
            RET => 0xc9,
            JZ => 0xca,
            CZ => 0xcc,
            CALL => 0xcd,
            ACI => 0xce,
            RST(1) => 0xcf,
            RNC => 0xd0,
            POP(D) => 0xd1,
            JNC => 0xd2,
            OUT => 0xd3,
            CNC => 0xd4,
            PUSH(D) => 0xd5,
            SUI => 0xd6,
            RST(2) => 0xd7,
            RC => 0xd8,
            JC => 0xda,
            IN => 0xdb,
            CC => 0xdc,
            SBI => 0xde,
            RST(3) => 0xdf,
            RPO => 0xe0,
            POP(H) => 0xe1,
            JPO => 0xe2,
            XTHL => 0xe3,
            CPO => 0xe4,
            PUSH(H) => 0xe5,
            ANI => 0xe6,
            RST(4) => 0xe7,
            RPE => 0xe8,
            PCHL => 0xe9,
            JPE => 0xea,
            XCHG => 0xeb,
            CPE => 0xec,
            XRI => 0xee,
            RST(5) => 0xef,
            RP => 0xf0,
            POP(PSW) => 0xf1,
            JP => 0xf2,
            DI => 0xf3,
            CP => 0xf4,
            PUSH(PSW) => 0xf5,
            ORI => 0xf6,
            RST(6) => 0xf7,
            RM => 0xf8,
            SPHL => 0xf9,
            JM => 0xfa,
            EI => 0xfb,
            CM => 0xfc,
            CPI => 0xfe,
            RST(7) => 0xff,
            _ => panic!("Invalid instruction: {:?}", self),
        }
    }
}

/*impl Instruction {
    pub fn size(&self) -> u8 {
        use Instruction::*;
        match self {
            LXI(_) => 3,
            MVI(_) => 2,
            SHLD => 3,
            LHLD => 3,
            STA => 3,
            LDA => 3,
            JNZ => 3,
            JMP => 3,
            CNZ => 3,
            ADI => 2,
            JZ => 3,
            CZ => 3,
            JNC => 3,
            OUT => 2,
            CNC => 3,
            SUI => 2,
            JC => 3,
            IN => 2,
            CC => 3,
            SBI => 2,
            JPO => 3,
            CPO => 3,
            ANI => 2,
            JPE => 3,
            CPE => 3,
            XRI => 2,
            JP => 3,
            CP => 3,
            ORI => 2,
            JM => 3,
            CM => 3,
            CPI => 2,
            _ => 1,
        }
    }
}*/