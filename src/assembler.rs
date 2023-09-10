use crate::{Opcode, Register};

enum Token {
    Data(u8),
    Instruction(Inst),
    Label(String),
}

struct Inst {
    opcode: Opcode,
    arg1: Option<u8>,
    arg2: Option<u8>,
}

impl Inst {
    pub fn new_0(opcode: Opcode) -> Self {
        Inst{ opcode, arg1: None, arg2: None }
    }

    pub fn new_1(opcode: Opcode, arg1: u8) -> Self {
        Inst{ opcode, arg1: Some(arg1), arg2: None }
    }

    pub fn new_2(opcode: Opcode, arg1: u8, arg2: u8) -> Self {
        Inst{ opcode, arg1: Some(arg1), arg2: Some(arg2) }
    }

    pub fn new_16_imm(opcode: Opcode, imm: u16) -> Self {
        let bytes = imm.to_le_bytes();
        Inst{ opcode, arg1: Some(bytes[0]), arg2: Some(bytes[1]) }
    }
}

#[derive(Debug, Copy, Clone)]
enum Arg {
    Imm8(u8),
    Imm16(u16),
    Reg(Register),
}

impl Arg {
    fn bytes(&self) -> (u8, Option<u8>) {
        use Arg::*;
        match self {
            Imm8(imm) => (*imm, None),
            Imm16(imm) => (*imm as u8, Some((*imm >> 8) as u8)),
            Reg(r) => (*r as u8, None),
        }
    }

    fn parse(arg: String) -> Self {
        todo!()
    }
}

pub fn assemble(program: &str) {
    let tokens = tokenize(program);
    let labeled = labelize(program);
    let assembled = assemble_tokens(tokens);
}

fn tokenize(program: &str) -> Vec<Token> {
    let mut output = Vec::new();

    for mut line in program.lines() {
        // Cut away comments
        if let Some(pos) = line.find(';') {
            line = &line[..pos].trim();
        }

        // Take label
        if let Some(pos) = line.find(':') {
            let (label, rest) = line.split_at(pos);
            output.push(Token::Label(label.to_string().trim().to_string()));
            line = rest.trim();
        }

        
        let mut ins = line.split_whitespace();
        let opcode = ins.next().unwrap().to_string();
        let args = ins.collect::<String>();
        let mut split = args.split(",");
        let arg1 = split.next().map(|s| Arg::parse(s.trim().to_string()));
        let arg2 = split.next().map(|s| Arg::parse(s.trim().to_string()));
        let inst = Inst::parse(&opcode, arg1, arg2).unwrap();
        output.push(Token::Instruction(inst));
    }

    output
}

fn labelize(program: &str) -> Vec<u8> {
    Vec::new()
}

fn assemble_tokens(tokens: Vec<Token>) -> Vec<u8> {
    Vec::new()
}

impl Inst {
    fn parse(op: &str, arg1: Option<Arg>, arg2: Option<Arg>) -> Result<Self, String> {
        use Opcode::*;
        use Arg::*;

        Ok(match (op, arg1, arg2) {
            ("NOP", None, None) => Inst::new_0(NOP),
            ("LXI", Some(Reg(r)), Some(Imm16(imm))) => {
                assert!(r.is_pair_or_sp());
                Inst::new_16_imm(LXI(r), imm)
            },
            ("STAX", Some(Reg(r)), None) => {
                assert!(r == Register::B || r == Register::D);
                Inst::new_0(STAX(r))
            },
            ("INX", Some(Reg(r)), None) => {
                assert!(r.is_pair_or_sp());
                Inst::new_0(INX(r))
            },
            ("INR", Some(Reg(r)), None) => {
                assert!(r.is_reg_or_mem());
                Inst::new_0(INR(r))
            },
            ("DCR", Some(Reg(r)), None) => {
                assert!(r.is_reg_or_mem());
                Inst::new_0(DCR(r))
            },
            ("MVI", Some(Reg(r)), Some(Imm8(imm))) => {
                assert!(r.is_reg_or_mem());
                Inst::new_1(MVI(r), imm)
            },
            ("DAD", Some(Reg(r)), None) => {
                assert!(r.is_pair_or_sp());
                Inst::new_0(DAD(r))
            },
            ("LDAX", Some(Reg(r)), None) => {
                assert!(r.is_pair());
                Inst::new_0(LDAX(r))
            },
            ("DCX", Some(Reg(r)), None) => {
                assert!(r.is_pair_or_sp());
                Inst::new_0(DCX(r))
            },
            ("RLC", None, None) => {
                Inst::new_0(RLC)
            },
            ("RRC", None, None) => {
                Inst::new_0(RRC)
            },
            ("RAL", None, None) => {
                Inst::new_0(RAL)
            },
            ("RAR", None, None) => {
                Inst::new_0(RAR)
            },
            ("SHLD", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(SHLD, imm)
            },
            ("DAA", None, None) => {
                Inst::new_0(DAA)
            },
            ("LHLD", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(LHLD, imm)
            },
            ("CMA", None, None) => {
                Inst::new_0(CMA)
            },
            ("STA", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(STA, imm)
            },
            ("STC", None, None) => {
                Inst::new_0(STC)
            },
            ("LDA", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(LDA, imm)
            },
            ("CMC", None, None) => {
                Inst::new_0(CMC)
            },
            ("MOV", Some(Reg(src)), Some(Reg(dst))) => {
                assert!(src.is_reg_or_mem() && dst.is_reg_or_mem());
                Inst::new_0(MOV(src, dst))
            },
            ("HLT", None, None) => {
                Inst::new_0(HLT)
            },
            ("ADD", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(ADD(reg))
            },
            ("ADC", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(ADC(reg))
            },
            ("SUB", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(SUB(reg))
            },
            ("SBB", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(SBB(reg))
            },
            ("ANA", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(ANA(reg))
            },
            ("XRA", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(XRA(reg))
            },
            ("ORA", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(ORA(reg))
            },
            ("CMP", Some(Reg(reg)), None) => {
                assert!(reg.is_reg_or_mem());
                Inst::new_0(CMP(reg))
            },
            ("RNZ", None, None) => {
                Inst::new_0(RNZ)
            },
            ("JNZ", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JNZ, imm)
            },
            ("JMP", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JMP, imm)
            },
            ("CNZ", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CNZ, imm)
            },
            ("PUSH", Some(Reg(reg)), None) => {
                assert!(reg.is_pair());
                Inst::new_0(PUSH(reg))
            },
            ("PUSH", None, None) => {
                Inst::new_0(PUSH(Register::PSW))
            },
            ("ADI", Some(Imm8(imm)), None) => {
                Inst::new_1(ADI, imm)
            },
            ("RST", Some(Imm8(n)), None) => {
                assert!(n < 8);
                Inst::new_0(RST(n))
            },
            ("RZ", None, None) => {
                Inst::new_0(RZ)
            },
            ("RET", None, None) => {
                Inst::new_0(RET)
            },
            ("JZ", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JZ, imm)
            },
            ("CZ", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CZ, imm)
            },
            ("CALL", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CALL, imm)
            },
            ("ACI", Some(Imm8(imm)), None) => {
                Inst::new_1(ACI, imm)
            },
            ("RNC", None, None) => {
                Inst::new_0(RNC)
            },
            ("JNC", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JNC, imm)
            },
            ("OUT", Some(Imm8(imm)), None) => {
                // TODO
                Inst::new_1(OUT, imm)
            },
            ("CNC", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CNC, imm)
            },
            ("SUI", Some(Imm8(imm)), None) => {
                Inst::new_1(SUI, imm)
            },
            ("RC", None, None) => {
                Inst::new_0(RC)
            },
            ("JC", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JC, imm)
            },
            ("IN", Some(Imm8(imm)), None) => {
                Inst::new_1(IN, imm)
            },
            ("CC", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CC, imm)
            },
            ("SBI", Some(Imm8(imm)), None) => {
                Inst::new_1(SBI, imm)
            },
            ("RPO", None, None) => {
                Inst::new_0(RPO)
            },
            ("JPO", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JPO, imm)
            },
            ("XTHL", None, None) => {
                Inst::new_0(XTHL)
            },
            ("CPO", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CPO, imm)
            },
            ("ANI", Some(Imm8(imm)), None) => {
                Inst::new_1(ANI, imm)
            },
            ("RPE", None, None) => {
                Inst::new_0(RPE)
            },
            ("PCHL", None, None) => {
                Inst::new_0(PCHL)
            },
            ("JPE", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JPE, imm)
            },
            ("XCHG", None, None) => {
                Inst::new_0(XCHG)
            },
            ("CPE", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CPE, imm)
            },
            ("XRI", Some(Imm8(imm)), None) => {
                Inst::new_1(XRI, imm)
            },
            ("RP", None, None) => {
                Inst::new_0(RP)
            },
            ("JP", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JP, imm)
            },
            ("DI", _, _) => todo!(),
            ("ORI", Some(Imm8(imm)), None) => {
                Inst::new_1(ORI, imm)
            },
            ("RM", None, None) => {
                Inst::new_0(RM)
            },
            ("SPHL", None, None) => {
                Inst::new_0(SPHL)
            },
            ("JM", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(JM, imm)
            },
            ("EI", _, _) => todo!(),
            ("CM", Some(Imm16(imm)), None) => {
                Inst::new_16_imm(CM, imm)
            },
            ("CPI", Some(Imm8(imm)), None) => {
                Inst::new_1(CPI, imm)
            },
            _ => return Err(format!("Illegal instruction: {} {:?},{:?}", op, arg1, arg2))
        })
    }
}