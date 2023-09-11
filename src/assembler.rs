use std::{collections::HashMap, fmt::{Display, Formatter, self}};

use itertools::Itertools;

use crate::{Register, Opcode};

const OPCODES: [&'static str; 83] = [
    "NOP", "LXI",  "STAX", "INX",  "INR",  "DCR",
    "MVI", "RLC",  "DAD",  "LDAX", "DCX",  "RRC",
    "RAL", "RAR",  "SHLD", "DAA",  "LHLD", "CMA",
    "STA", "STC",  "LDA",  "CMC",  "MOV",  "HLT",
    "ADD", "ADC",  "SUB",  "SBB",  "ANA",  "XRA",
    "ORA", "CMP",  "RNZ",  "POP",  "JNZ",  "JMP",
    "CNZ", "PUSH", "ADI",  "RST",  "RZ",   "RET",
    "JZ",  "CZ",   "CALL", "ACI",  "RNC",  "JNC",
    "OUT", "CNC",  "SUI",  "RC",   "JC",   "IN",
    "CC",  "SBI",  "RPO",  "JPO",  "XTHL", "CPO",
    "ANI", "RPE",  "PCHL", "JPE",  "XCHG", "CPE",
    "XRI", "RP",   "JP",   "DI",   "CP",   "ORI",
    "RM",  "SPHL", "JM",   "EI",   "CM",   "CPI",

    // Pseudo instructions
    "ORG", "DB",   "DW",   "DS",   "END",
];

const NAMED_OPCODES: [&'static str; 2] = [
    "EQU", "SET"
];

pub struct AssemblerError {
    pub line_nr: usize,
    pub message: String,
}

pub struct AssemblerWarning {
    pub line_nr: usize,
    pub message: String,
}

#[derive(Debug, Clone)]
struct AsmInst {
    pub line: usize,
    pub address: u16,
    pub length: u16,
    pub opcode: String,
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, Clone)]
enum Asm {
    Empty(u16),
    Inst(AsmInst),
}

impl Display for Asm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Asm::*;
        match self {
            Empty(_) => Ok(()),
            Inst(inst) => if inst.name.is_empty() {
                write!(f, "{}:\t\t{}\t{}", inst.address, inst.opcode, inst.args.join(","))
            } else {
                write!(f, "{}:\t{}\t{}\t{}", inst.address, inst.name, inst.opcode, inst.args.join(","))
            },
        }
    }
}

pub fn assemble(lines: Vec<String>) -> Result<(Vec<(u16, Vec<u8>)>, Vec<AssemblerWarning>), AssemblerError> {
    let mut warnings = Vec::new();
    let (mut asm, labels) = preprocess(lines, &mut warnings)?;
    let labels = measure(&mut asm, labels)?;

    
    println!("----- Assembled program -----");
    for line in asm.clone() {
        if matches!(line, Asm::Empty(_)) {
            continue;
        }
        println!("{}", line);
    }
    println!("\n----- Labels ----------------");
    println!("Labels: {:?}", labels);

    let binary = generate_binary(&mut asm, labels)?;


    println!("\n----- Binary ----------------");
    for (addr, bytes) in binary.clone() {
        println!("{:04X}:\t{}", addr, bytes.iter().map(|b| format!("{:02X}", b)).join(" "));
    }
    println!("\n-----------------------------");

    Ok((binary, warnings))
}

fn validate_name(name: &str, line_nr: usize) -> Result<(), AssemblerError> {
    if name.is_empty() {
        return Err(AssemblerError {
            line_nr,
            message: "Name cannot be empty".to_string(),
        });
    }
    if name.chars().last().unwrap().is_whitespace() {
        return Err(AssemblerError {
            line_nr,
            message: format!("Found whitespace before ':'"),
        });
    }
    if OPCODES.contains(&name) || NAMED_OPCODES.contains(&name) {
        return Err(AssemblerError {
            line_nr,
            message: format!("Cannot use opcode as name: '{}'", name),
        });
    }
    let first_char = name.chars().next().unwrap();
    if !(first_char.is_alphabetic() || first_char == '@' || first_char == '?') {
        return Err(AssemblerError {
            line_nr,
            message: format!("Name must start with letter or one of '?', '@'. found: '{}'", first_char),
        });
    }
    if name.chars().skip(1).any(|c| !c.is_alphanumeric()) {
        return Err(AssemblerError {
            line_nr,
            message: format!("Illegal name: '{}'", name),
        });
    }
    Ok(())
}

fn preprocess(lines: Vec<String>, warnings: &mut Vec<AssemblerWarning>) -> Result<(Vec<Asm>, Vec<(String, usize)>), AssemblerError> {
    use Asm::*;
    
    let mut new_lines = Vec::new();
    let mut labels = Vec::new();

    for (line_nr, line) in lines.iter().enumerate() {
        // Remove comments
        let line = line.split(';').next().unwrap().trim().to_uppercase();

        // Seperate label from instruction
        let instruction = match line.find(':') {
            Some(loc) => {
                let (lab, ins) = (line[..loc].to_string(), line[loc + 1..].to_string());
                validate_name(&lab, line_nr)?;
                if lab.len() > 5 {
                    warnings.push(AssemblerWarning {
                        line_nr,
                        message: format!("Label '{}' is longer than 5 characters. Will be used as '{}'", lab, lab[..5].to_string()),
                    });
                }
                labels.push((lab[..5.min(lab.len())].to_string(), line_nr));
                ins.trim().to_string()
            },
            None => line,
        };

        if instruction.is_empty() {
            new_lines.push(Empty(0));
            continue;
        }

        if NAMED_OPCODES.contains(&instruction.split_whitespace().next().unwrap()) {
            return Err(AssemblerError {
                line_nr,
                message: format!("Opcode {} needs a name to be specified", instruction.split_whitespace().next().unwrap()),
            });
        }

        if OPCODES.contains(&instruction.split_whitespace().next().unwrap()) {
            let (opcode, args) = {
                let mut split = instruction.split_whitespace();

                let opcode = split.next().unwrap();

                // Detect illegal named opcodes
                if let Some(next) = split.clone().peekable().peek() {
                    if NAMED_OPCODES.contains(next) {
                        return Err(AssemblerError {
                            line_nr,
                            message: format!("Cannot use opcode as name: '{}'", opcode),
                        });
                    }
                }

                let args = split.join(" ")
                    .split(',').filter(|arg| !arg.is_empty())
                    .map(|arg| arg.trim().to_string())
                    .collect::<Vec<String>>();

                (opcode, args)
            };

            new_lines.push(Inst(AsmInst {
                line: line_nr,
                address: 0,
                length: 0,
                opcode: opcode.to_string(),
                name: String::new(),
                args: args.clone(),
            }));

        } else {
            // Named pseudo instruction

            let mut split = instruction.split_whitespace();
            let name = split.next().unwrap();
            validate_name(name, line_nr)?;
            let opcode = match split.next() {
                Some(op) => {
                    let op = op.trim();
                    if !NAMED_OPCODES.contains(&op) {
                        return Err(AssemblerError {
                            line_nr,
                            message: format!("Unknown opcode: {}", name),
                        });
                    }

                    op
                },
                None => return Err(AssemblerError {
                    line_nr,
                    message: "Expected an opcode".to_string(),
                })
            };

            let args = split.join(" ")
                .split(',').filter(|arg| !arg.is_empty())
                .map(|arg| arg.trim().to_string())
                .collect::<Vec<String>>();

            new_lines.push(Inst(AsmInst {
                line: line_nr,
                address: 0,
                length: 0,
                opcode: opcode.to_string(),
                name: name.to_string(),
                args: args.clone(),
            }));
        }
    }


    Ok((new_lines, labels))
}

/// Creates the initial variable environment with the registers
fn initial_var_env() -> HashMap<String, u16> {
    let mut vars: HashMap<String, u16> = HashMap::new();
    vars.insert("B".to_string(), 0);
    vars.insert("C".to_string(), 1);
    vars.insert("D".to_string(), 2);
    vars.insert("E".to_string(), 3);
    vars.insert("H".to_string(), 4);
    vars.insert("L".to_string(), 5);
    vars.insert("M".to_string(), 6);
    vars.insert("A".to_string(), 7);
    vars
}

/// Calculates the address of everything, as well as determine variables and labels
fn measure(lines: &mut Vec<Asm>, labels: Vec<(String, usize)>) -> Result<HashMap<String, u16>, AssemblerError> {
    let mut vars = initial_var_env();
    let mut location: u16 = 0;
    let mut labels = labels.into_iter().peekable();
    let mut new_labels: HashMap<String, u16> = HashMap::new();

    for (line_nr, line) in lines.iter_mut().enumerate() {
        if let Some((label, lbl_line_nr)) = labels.peek() {
            if line_nr >= *lbl_line_nr {
                vars.insert(label.clone(), location);
                new_labels.insert(label.clone(), location);
                labels.next();
            }
        }

        match line {
            Asm::Empty(address) => { *address = location},
            Asm::Inst(inst) => {
                inst.address = location;
                match inst.opcode.as_str() {
                    "DS" => {
                        if inst.args.len() != 1 {
                            return Err(AssemblerError {
                                line_nr: inst.line,
                                message: format!("Expected 1 argument, found {}", inst.args.len()),
                            });
                        }
                        let size = eval_arg(&inst.args[0], &vars, line_nr)?;
                        inst.args[0] = size.to_string();
                        inst.length = size;
                        location += size;
                    }
                    "ORG" => {
                        if inst.args.len() != 1 {
                            return Err(AssemblerError {
                                line_nr: inst.line,
                                message: format!("Expected 1 argument, found {}", inst.args.len()),
                            });
                        }
                        location = eval_arg(&inst.args[0], &vars, line_nr)?;
                        inst.args[0] = location.to_string();
                    }
                    "DB" => {
                        let length = inst.args.len() as u16;
                        location += length;
                        inst.length = length;
                    }
                    "DW" => {
                        let length = inst.args.len() as u16 * 2;
                        location += length;
                        inst.length = length;
                    }
                    "SET" => {
                        if inst.args.len() != 1 {
                            return Err(AssemblerError {
                                line_nr: inst.line,
                                message: format!("Expected 1 arguments, found {}", inst.args.len()),
                            });
                        }
                        let value = eval_arg(&inst.args[0], &vars, line_nr)?;
                        inst.args[0] = value.to_string();
                        vars.insert(inst.name.clone(), value);
                    }
                    "EQU" => {
                        if inst.args.len() != 1 {
                            return Err(AssemblerError {
                                line_nr: inst.line,
                                message: format!("Expected 1 arguments, found {}", inst.args.len()),
                            });
                        }
                        if vars.contains_key(&inst.name) {
                            return Err(AssemblerError {
                                line_nr: inst.line,
                                message: format!("Variable '{}' already defined", inst.args[0]),
                            });
                        }
                        let value = eval_arg(&inst.args[0], &vars, line_nr)?;
                        inst.args[0] = value.to_string();
                        vars.insert(inst.name.clone(), value);
                    }
                    "END" => break,
                    _=> {
                        let length = measure_opcode(&inst.opcode);
                        location += length;
                        inst.length = length;
                    }
                }
            },
        }
    }
    
    Ok(new_labels)
}

fn generate_binary(lines: &mut Vec<Asm>, labels: HashMap<String, u16>) -> Result<Vec<(u16, Vec<u8>)>, AssemblerError> {
    let mut var_env = initial_var_env();
    var_env.extend(labels.clone());

    let mut binary: Vec<(u16, Vec<u8>)> = Vec::new();
    let mut temp: Vec<u8> = Vec::new();
    let mut temp_start_address = 0;

    let mut location = 0;

    for (line_nr, line) in lines.into_iter().enumerate() {
        match line {
            Asm::Empty(_) => (),
            Asm::Inst(inst) => {
                let args = inst.args.clone();
                match inst.opcode.as_str() {
                    "ORG" => {
                        if !temp.is_empty() {
                            binary.push((temp_start_address, temp));
                            temp = Vec::new();
                        }
                        location = eval_arg(&args[0], &var_env, line_nr)?;
                        temp_start_address = location;
                    },
                    "DS" => {
                        if !temp.is_empty() {
                            binary.push((temp_start_address, temp));
                            temp = Vec::new();
                        }
                        location += eval_arg(&args[0], &var_env, line_nr)?;
                        temp_start_address = location;
                    },
                    "SET" => {
                        let value = eval_arg(&args[0], &var_env, line_nr)?;
                        var_env.insert(inst.name.clone(), value);
                    },
                    "EQU" => {
                        let value = eval_arg(&args[0], &var_env, line_nr)?;
                        var_env.insert(inst.name.clone(), value);
                    },
                    "END" => break, 
                    opcode => {
                        let inst = parse_inst(opcode, args, &var_env, line_nr)?;
                        location += inst.len() as u16;
                        temp.extend(inst);
                    }
                }
            },
        }
    }

    if !temp.is_empty() {
        binary.push((temp_start_address, temp));
    }

    Ok(binary)
}

/*enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    Not,
    And,
    Or,
    Xor
}

enum ExpTree {
    Empty,
    Node(String),
    BinOp(Operator, Box<ExpTree>, Box<ExpTree>),
    UnOp(Operator, Box<ExpTree>),
}*/

/// Evaluates operand
fn eval_arg(arg: &str, var_env: &HashMap<String, u16>, line_nr: usize) -> Result<u16, AssemblerError> {
    if arg.chars().next().unwrap().is_numeric() {
        Ok(arg.parse::<u16>().unwrap())
    } else {
        if !var_env.contains_key(arg) {
            return Err(AssemblerError {
                line_nr,
                message: format!("Undefined variable: '{}'", arg),
            });
        }
        Ok(*var_env.get(arg).unwrap())
    }
}

pub fn measure_opcode(opcode: &str) -> u16 {
    match opcode {
        "LXI" => 3,
        "MVI" => 2,
        "SHLD" => 3,
        "LHLD" => 3,
        "STA" => 3,
        "LDA" => 3,
        "JNZ" => 3,
        "JMP" => 3,
        "CNZ" => 3,
        "ADI" => 2,
        "JZ" => 3,
        "CZ" => 3,
        "JNC" => 3,
        "OUT" => 2,
        "CNC" => 3,
        "SUI" => 2,
        "JC" => 3,
        "IN" => 2,
        "CC" => 3,
        "SBI" => 2,
        "JPO" => 3,
        "CPO" => 3,
        "ANI" => 2,
        "JPE" => 3,
        "CPE" => 3,
        "XRI" => 2,
        "JP" => 3,
        "CP" => 3,
        "ORI" => 2,
        "JM" => 3,
        "CM" => 3,
        "CPI" => 2,

        "DS" | "SET" | "EQU" => panic!(),

        "END" => 0,

        _ => 1,
    }
}

fn parse_register(reg: &str, var_env: &HashMap<String, u16>, line_nr: usize) -> Result<Register, AssemblerError> {
    match eval_arg(reg, var_env, line_nr)? {
        0 => Ok(Register::B),
        1 => Ok(Register::C),
        2 => Ok(Register::D),
        3 => Ok(Register::E),
        4 => Ok(Register::H),
        5 => Ok(Register::L),
        6 => Ok(Register::M),
        7 => Ok(Register::A),
        _ => Err(AssemblerError {
            line_nr: 0,
            message: format!("Illegal register: {}", reg),
        }),
    }
}

fn parse_reg_pair_or_sp(arg: &str, line_nr: usize) -> Result<Register, AssemblerError> {
    match arg {
        "B" => Ok(Register::B),
        "D" => Ok(Register::D),
        "H" => Ok(Register::H),
        "SP" => Ok(Register::SP),
        _ => Err(AssemblerError {
            line_nr,
            message: format!("Illegal register pair: {}", arg),
        })
    }
}

fn parse_inst(op: &str, args: Vec<String>, var_env: &HashMap<String, u16>, line_nr: usize) -> Result<Vec<u8>, AssemblerError> {
    use Opcode::*;

    let assert_arg_cnt = |cnt: usize| {
        if cnt != args.len() {
            Err(AssemblerError {
                line_nr,
                message: format!("Expected {} arguments, found {}", cnt, args.len()),
            })
        } else {
            Ok(())
        }
    };

    Ok(match op {
        "DB" => {
            let mut bytes = Vec::new();
            for arg in args {
                bytes.push(eval_arg(&arg, var_env, line_nr)? as u8);
            }
            bytes
        },
        "DW" => {
            let mut bytes = Vec::new();
            for arg in args {
                let imm = eval_arg(&arg, var_env, line_nr)?;
                bytes.push(imm as u8);
                bytes.push((imm >> 8) as u8);
            }
            bytes
        },
        "NOP" => {
            assert_arg_cnt(0)?;
            vec![NOP.into()]
        },
        "LXI" => {
            assert_arg_cnt(2)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            let imm = eval_arg(&args[1], var_env, line_nr)?;
            vec![LXI(r).into(), imm as u8, (imm >> 8) as u8]
        },
        "STAX" => {
            assert_arg_cnt(1)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            if r != Register::B && r != Register::D {
                return Err(AssemblerError {
                    line_nr,
                    message: format!("Illegal register: {}", &args[0]),
                });
            }
            vec![STAX(r).into()]
        },
        "INX" => {
            assert_arg_cnt(1)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            vec![INX(r).into()]
        },
        "INR" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![INR(r).into()]
        },
        "DCR" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![DCR(r).into()]
        },
        "MVI" => {
            assert_arg_cnt(2)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            let imm = eval_arg(&args[1], var_env, line_nr)?;
            vec![MVI(r).into(), imm as u8]
        },
        "DAD" => {
            assert_arg_cnt(1)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            vec![DAD(r).into()]
        },
        "LDAX" => {
            assert_arg_cnt(1)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            if r != Register::B && r != Register::D {
                return Err(AssemblerError {
                    line_nr,
                    message: format!("Illegal register: {}", &args[0]),
                });
            }
            vec![LDAX(r).into()]
        },
        "DCX" => {
            assert_arg_cnt(1)?;
            let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
            vec![DCX(r).into()]
        },
        "RLC" => {
            assert_arg_cnt(0)?;
            vec![RLC.into()]
        },
        "RRC" => {
            assert_arg_cnt(0)?;
            vec![RRC.into()]
        },
        "RAL" => {
            assert_arg_cnt(0)?;
            vec![RAL.into()]
        },
        "RAR" => {
            assert_arg_cnt(0)?;
            vec![RAR.into()]
        },
        "SHLD" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![SHLD.into(), imm as u8, (imm >> 8) as u8]
        },
        "DAA" => {
            assert_arg_cnt(0)?;
            vec![DAA.into()]
        },
        "LHLD" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![LHLD.into(), imm as u8, (imm >> 8) as u8]
        },
        "CMA" => {
            assert_arg_cnt(0)?;
            vec![CMA.into()]
        },
        "STA" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![STA.into(), imm as u8, (imm >> 8) as u8]
        },
        "STC" => {
            assert_arg_cnt(0)?;
            vec![STC.into()]
        },
        "LDA" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![LDA.into(), imm as u8, (imm >> 8) as u8]
        },
        "CMC" => {
            assert_arg_cnt(0)?;
            vec![CMC.into()]
        },
        "MOV" => {
            assert_arg_cnt(2)?;
            let src = parse_register(&args[0], var_env, line_nr)?;
            let dst = parse_register(&args[1], var_env, line_nr)?;
            vec![MOV(src, dst).into()]
        },
        "HLT" => {
            assert_arg_cnt(0)?;
            vec![HLT.into()]
        },
        "ADD" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![ADD(r).into()]
        },
        "ADC" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![ADC(r).into()]
        },
        "SUB" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![SUB(r).into()]
        },
        "SBB" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![SBB(r).into()]
        },
        "ANA" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![ANA(r).into()]
        },
        "XRA" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![XRA(r).into()]
        },
        "ORA" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![ORA(r).into()]
        },
        "CMP" => {
            assert_arg_cnt(1)?;
            let r = parse_register(&args[0], var_env, line_nr)?;
            vec![CMP(r).into()]
        },
        "RNZ" => {
            assert_arg_cnt(0)?;
            vec![RNZ.into()]
        },
        "JNZ" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JNZ.into(), imm as u8, (imm >> 8) as u8]
        },
        "JMP" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JMP.into(), imm as u8, (imm >> 8) as u8]
        },
        "CNZ" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CNZ.into(), imm as u8, (imm >> 8) as u8]
        },
        "PUSH" => {
            if args.len() == 1 {
                let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
                vec![PUSH(r).into()]
            } else {
                assert_arg_cnt(0)?;
                vec![PUSH(Register::PSW).into()]
            }
        },
        "POP" => {
            if args.len() == 1 {
                let r = parse_reg_pair_or_sp(&args[0], line_nr)?;
                vec![POP(r).into()]
            } else {
                assert_arg_cnt(0)?;
                vec![POP(Register::PSW).into()]
            }
        },
        "ADI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![ADI.into(), imm as u8]
        },
        "RST" => {
            assert_arg_cnt(1)?;
            let n = eval_arg(&args[0], var_env, line_nr)?;
            if n > 7 {
                return Err(AssemblerError {
                    line_nr,
                    message: format!("RST number must be less than 8, found '{}'", n),
                });
            }
            vec![RST(n as u8).into()]
        },
        "RZ" => {
            assert_arg_cnt(0)?;
            vec![RZ.into()]
        },
        "RET" => {
            assert_arg_cnt(0)?;
            vec![RET.into()]
        },
        "JZ" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JZ.into(), imm as u8, (imm >> 8) as u8]
        },
        "CZ" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CZ.into(), imm as u8, (imm >> 8) as u8]
        },
        "CALL" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CALL.into(), imm as u8, (imm >> 8) as u8]
        },
        "ACI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![ACI.into(), imm as u8]
        },
        "RNC" => {
            assert_arg_cnt(0)?;
            vec![RNC.into()]
        },
        "JNC" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JNC.into(), imm as u8, (imm >> 8) as u8]
        },
        "OUT" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![OUT.into(), imm as u8]
        },
        "CNC" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CNC.into(), imm as u8, (imm >> 8) as u8]
        },
        "SUI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![SUI.into(), imm as u8]
        },
        "RC" => {
            assert_arg_cnt(0)?;
            vec![RC.into()]
        },
        "JC" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JC.into(), imm as u8, (imm >> 8) as u8]
        },
        "IN" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![IN.into(), imm as u8]
        },
        "CC" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CC.into(), imm as u8, (imm >> 8) as u8]
        },
        "SBI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![SBI.into(), imm as u8]
        },
        "RPO" => {
            assert_arg_cnt(0)?;
            vec![RPO.into()]
        },
        "JPO" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JPO.into(), imm as u8, (imm >> 8) as u8]
        },
        "XTHL" => {
            assert_arg_cnt(0)?;
            vec![XTHL.into()]
        },
        "CPO" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CPO.into(), imm as u8, (imm >> 8) as u8]
        },
        "ANI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![ANI.into(), imm as u8]
        },
        "RPE" => {
            assert_arg_cnt(0)?;
            vec![RPE.into()]
        },
        "PCHL" => {
            assert_arg_cnt(0)?;
            vec![PCHL.into()]
        },
        "JPE" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JPE.into(), imm as u8, (imm >> 8) as u8]
        },
        "XCHG" => {
            assert_arg_cnt(0)?;
            vec![XCHG.into()]
        },
        "CPE" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CPE.into(), imm as u8, (imm >> 8) as u8]
        },
        "XRI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![XRI.into(), imm as u8]
        },
        "RP" => {
            assert_arg_cnt(0)?;
            vec![RP.into()]
        },
        "JP" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JP.into(), imm as u8, (imm >> 8) as u8]
        },
        "DI" => {
            assert_arg_cnt(0)?;
            vec![DI.into()]
        },
        "ORI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![ORI.into(), imm as u8]
        },
        "RM" => {
            assert_arg_cnt(0)?;
            vec![RM.into()]
        },
        "SPHL" => {
            assert_arg_cnt(0)?;
            vec![SPHL.into()]
        },
        "JM" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![JM.into(), imm as u8, (imm >> 8) as u8]
        },
        "EI" => {
            assert_arg_cnt(0)?;
            vec![EI.into()]
        },
        "CM" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CM.into(), imm as u8, (imm >> 8) as u8]
        },
        "CPI" => {
            assert_arg_cnt(1)?;
            let imm = eval_arg(&args[0], var_env, line_nr)?;
            vec![CPI.into(), imm as u8]
        },
        op => {
            println!("op: {op} was unhandled");
            panic!()
        }
    })
}