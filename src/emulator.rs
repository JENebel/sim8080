use core::panic;

use crate::Opcode;

/// Representation of the 8080 processor
pub struct Emulator {
    registers: [u8; 8],
    sp: u16,
    memory: [u8; 0x10000],
    pc: u16,
}

pub enum Flag {
    Carry =     0b00000001,
    Parity =    0b00000100,
    AuxCarry =  0b00010000,
    Zero =      0b01000000,
    Sign =      0b10000000,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    A = 0, PSW = 1, // A
    B = 2, C = 3,   // B
    D = 4, E = 5,   // D
    H = 6, L = 7,   // M
    /// Stack pointer
    SP = 8,
    /// HL register pair
    M = 255
}

impl Register {
    pub fn is_pair(&self) -> bool {
        use Register::*;
        match self {
            B | D | H => true,
            _ => false,
        }
    }

    pub fn is_reg(&self) -> bool {
        use Register::*;
        match self {
            A | B | C | D | E | H | L => true,
            _ => false,
        }
    }

    pub fn is_reg_or_mem(&self) -> bool {
        use Register::*;
        match self {
            A | B | C | D | E | H | L | M => true,
            _ => false,
        }
    }

    pub fn is_spw_or_reg(&self) -> bool {
        use Register::*;
        match self {
            SP | M => true,
            _ => false,
        }
    }

    pub fn is_pair_or_sp(&self) -> bool {
        use Register::*;
        match self {
            B | D | H | SP => true,
            _ => false,
        }
    }
}

impl From<u8> for Register {
    fn from(val: u8) -> Self {
        use Register::*;
        match val {
            0 => A,
            1 => PSW,
            2 => B,
            3 => C,
            4 => D,
            5 => E,
            6 => H,
            7 => L,
            8 => SP,
            _ => panic!("Invalid register value: {}", val),
        }
    }
}

impl Emulator {
    /// Creates a new emulator with all registers and memory initialized to 0
    pub fn new() -> Self {
        Emulator {
            registers: [0; 8],
            memory: [0; 0x10000],
            pc: 0,
            sp: u16::MAX,
        }
    }

    /// Loads a program into memory. Overwrites any existing memory.
    pub fn load(&mut self, program: Vec<(u16, Vec<u8>)>) {
        self.memory = [0; 0x10000];
        for (addr, bytes) in program {
            for (i, byte) in bytes.iter().enumerate() {
                self.memory[(addr as usize) + i] = *byte;
            }
        }
    }

    pub fn run(&mut self) {
        while (self.pc as usize) <= self.memory.len() {
            let instruction = Opcode::from(self.fetch_byte());
            if instruction == Opcode::HLT {
                break;
            }
            self.execute_instruction(instruction);
            // Insert delay to match targeted cycle time (usually 2MHz)
        }
        println!("\nProgram finished")
    }

    /// Fetches the next byte from memory and increments the program counter
    fn fetch_byte(&mut self) -> u8 {
        let value = self.memory[self.pc as usize];
        self.pc = self.pc.wrapping_add(1);
        value
    }

    /*fn fetch_word(&mut self) -> u16 {
        let low = self.fetch_byte();
        let high = self.fetch_byte();
        ((high as u16) << 8) | (low as u16)
    }*/

    /// Sets the register/memory location to the given value
    /// 
    /// M is a special register that represents the memory location pointed to by the HL register pair
    fn write_loc(&mut self, reg: Register, value: u8) {
        match reg {
            Register::M => {
                let addr = ((self.read_loc(Register::H) as u16) << 8) | (self.read_loc(Register::L) as u16);
                self.memory[addr as usize] = value;
            }
            _ => self.registers[reg as usize] = value
        }
    }
    /// Gets the register/memory location to the given value
    /// 
    /// M is a special register that represents the memory location pointed to by the HL register pair
    fn read_loc(&self, reg: Register) -> u8 {
        match reg {
            Register::M => {
                let addr = ((self.read_loc(Register::H) as u16) << 8) | (self.read_loc(Register::L) as u16);
                self.memory[addr as usize]
            },
            _ => self.registers[reg as usize]
        }
    }

    fn get_reg_pair(&self, reg: Register) -> (Register, Register) {
        use Register::*;
        match reg {
            B => (B, C),
            D => (D, E),
            H => (H, L),
            _ => panic!("Invalid register pair for get_reg_pair: {:?}", reg),
        }
    }

    fn get_flag(&self, flag: Flag) -> bool {
        (self.read_loc(Register::PSW) & (flag as u8)) != 0
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        let mut psw = self.read_loc(Register::PSW);
        if value {
            psw |= flag as u8;
        } else {
            psw &= !(flag as u8);
        }
        self.write_loc(Register::PSW, psw);
    }

    fn update_flags_with(&mut self, value: u8) {
        use Flag::*;
        self.set_flag(Zero, value == 0);
        self.set_flag(Sign, value & 0b1000_0000 != 0);
        self.set_flag(Parity, value.count_ones() % 2 == 0);
    }

    fn call_subroutine(&mut self, addr: u16) {
        self.memory[(self.sp.wrapping_sub(1)) as usize] = (self.pc >> 8) as u8;
        self.memory[(self.sp.wrapping_sub(2)) as usize] = self.pc as u8;
        self.sp = self.sp.wrapping_sub(2);
        self.pc = addr;
    }

    fn return_from_subroutine(&mut self) {
        let pcl = self.memory[self.sp as usize];
        let pch = self.memory[(self.sp.wrapping_add(1)) as usize];
        self.pc = ((pch as u16) << 8) | (pcl as u16);
        self.sp += 2;
    }

    fn execute_instruction(&mut self, instruction: Opcode) {
        use Opcode::*;
        use Register::*;
        use Flag::*;

        match instruction {
            NOP => (),
            LXI(reg) => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                if reg == SP {
                    self.sp = ((byte3 as u16) << 8) | (byte2 as u16);
                    return
                }
                let (hi, lo) = self.get_reg_pair(reg);
                self.write_loc(hi, byte3);
                self.write_loc(lo, byte2);
            },
            STAX(reg) => {
                let (hi, lo) = self.get_reg_pair(reg);
                let addr = ((self.read_loc(hi) as u16) << 8) | (self.read_loc(lo) as u16);
                self.memory[addr as usize] = self.read_loc(A);
            },
            INX(reg) => {
                if reg == SP {
                    self.sp = self.sp.wrapping_add(1);
                    return;
                }
                let (hi, lo) = self.get_reg_pair(reg);
                let value = ((self.read_loc(hi) as u16) << 8) | (self.read_loc(lo) as u16);
                let value = value.wrapping_add(1);
                self.write_loc(hi, (value >> 8) as u8);
                self.write_loc(lo, value as u8);
            },
            INR(reg) => {
                let value = self.read_loc(reg);
                let result = value.wrapping_add(1);
                self.write_loc(reg, result);
                self.set_flag(Zero, result == 0);
                self.set_flag(Sign, result & 0b1000_0000 != 0);
                self.set_flag(Parity, result.count_ones() % 2 == 0);
                self.set_flag(AuxCarry, (value & 0b0000_1111) + 1 > 0x0f);
            },
            DCR(reg) => {
                let value = self.read_loc(reg);
                let result = value.wrapping_sub(1);
                self.write_loc(reg, result);
                self.set_flag(Zero, result == 0);
                self.set_flag(Sign, result & 0x80 != 0);
                self.set_flag(Parity, result.count_ones() % 2 == 0);
                self.set_flag(AuxCarry, (value & 0b0000_1111) == 0);
            },
            MVI(reg) => {
                let value = self.fetch_byte();
                self.write_loc(reg, value);
            },
            DAD(reg) => {
                let (hi, lo) = self.get_reg_pair(reg);
                let value = if reg == SP {
                    self.sp
                } else {
                    ((self.read_loc(hi) as u16) << 8) | (self.read_loc(lo) as u16)
                };
                let hl = ((self.read_loc(H) as u16) << 8) | (self.read_loc(L) as u16);
                let (result, carry) = value.overflowing_add(hl);
                self.write_loc(H, (result >> 8) as u8);
                self.write_loc(L, result as u8);
                self.set_flag(Carry, carry);
            },
            LDAX(reg) => {
                let (hi, lo) = self.get_reg_pair(reg);
                let addr = ((self.read_loc(hi) as u16) << 8) | (self.read_loc(lo) as u16);
                self.write_loc(A, self.memory[addr as usize]);
            },
            DCX(reg) => {
                let (hi, lo) = self.get_reg_pair(reg);
                let value = ((self.read_loc(hi) as u16) << 8) | (self.read_loc(lo) as u16);
                let value = value.wrapping_sub(1);
                self.write_loc(hi, (value >> 8) as u8);
                self.write_loc(lo, value as u8);
            },
            RLC => {
                let value = self.read_loc(A);
                let result = value.rotate_left(1);
                self.write_loc(A, result);
                self.set_flag(Carry, value & 0b10000000 != 0);
            },
            RRC => {
                let value = self.read_loc(A);
                let result = value.rotate_right(1);
                self.write_loc(A, result);
                self.set_flag(Carry, value & 0b0000_0001 != 0);
            },
            RAL => {
                let value = self.read_loc(A);
                let carry = self.get_flag(Carry);
                let result = (value << 1) | (carry as u8);
                self.write_loc(A, result);
                self.set_flag(Carry, value & 0b1000_0000 != 0);
            },
            RAR => {
                let value = self.read_loc(A);
                let carry = self.get_flag(Carry);
                let result = (value >> 1) | ((carry as u8) << 7);
                self.write_loc(A, result);
                self.set_flag(Carry, value & 0b0000_0001 != 0);
            },
            SHLD => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.memory[addr as usize] = self.read_loc(L);
                self.memory[(addr.wrapping_add(1)) as usize] = self.read_loc(H);
            },
            DAA => {
                let mut value = self.read_loc(A);
                if value & 0x0f > 0x09 || self.get_flag(AuxCarry) {
                    let (new, aux_carry) = value.overflowing_add(0x06);
                    self.set_flag(AuxCarry, aux_carry);
                    value = new;
                }
                if value & 0xf0 > 0x90 || self.get_flag(Carry) {
                    let (new, c) = value.overflowing_add(0x60);
                    self.set_flag(Carry, c);
                    value = new;
                }
                self.write_loc(A, value);
                self.update_flags_with(value);
            },
            LHLD => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.write_loc(L, self.memory[addr as usize]);
                self.write_loc(H, self.memory[(addr.wrapping_add(1)) as usize]);
            },
            CMA => {
                let value = self.read_loc(A);
                self.write_loc(A, !value);
            },
            STA => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.memory[addr as usize] = self.read_loc(A);
            },
            STC => {
                self.set_flag(Carry, true);
            },
            LDA => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.write_loc(A, self.memory[addr as usize])
            },
            CMC => {
                self.set_flag(Carry, !self.get_flag(Carry))
            },
            MOV(dst, src) => {
                let value = self.read_loc(src);
                self.write_loc(dst, value)
            },
            HLT => panic!("HLT instruction should never be executed"),
            ADD(reg) => {
                let value = self.read_loc(reg);
                let a_val = self.read_loc(A);
                let (result, carry) = a_val.overflowing_add(value);
                self.write_loc(A, result);
                self.set_flag(Carry, carry);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) + (value & 0b0000_1111) > 0x0f);
                self.update_flags_with(result);
            },
            ADC(reg) => {
                let a_val = self.read_loc(A);
                let value = self.read_loc(reg);
                let (result, carry) = a_val.overflowing_add(value);
                let (result, carry2) = result.overflowing_add(self.get_flag(Carry) as u8);
                self.write_loc(A, result);
                self.set_flag(Carry, carry || carry2);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) + (value & 0b0000_1111) + (self.get_flag(Carry) as u8) > 0x0f);
                self.update_flags_with(result);
            },
            SUB(reg) => {
                let a_val = self.read_loc(A);
                let value = self.read_loc(reg);
                let (result, carry) = a_val.overflowing_sub(value);
                self.write_loc(A, result);
                self.set_flag(Carry, carry);
                self.update_flags_with(result);
            },
            SBB(reg) => {
                let a_val = self.read_loc(A);
                let value = self.read_loc(reg);
                let (result, carry) = a_val.overflowing_sub(value);
                let (result, carry2) = result.overflowing_sub(self.get_flag(Carry) as u8);
                self.write_loc(A, result);
                self.set_flag(Carry, carry || carry2);
                self.update_flags_with(result);
            },
            ANA(reg) => {
                let value = self.read_loc(reg);
                let result = self.read_loc(A) & value;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, ((self.read_loc(A) | value) & 0b0000_1000) != 0);
                self.update_flags_with(result);
            },
            XRA(reg) => {
                let value = self.read_loc(reg);
                let result = self.read_loc(A) ^ value;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, false);
                self.update_flags_with(result);
            },
            ORA(reg) => {
                let value = self.read_loc(reg);
                let result = self.read_loc(A) | value;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, false);
                self.update_flags_with(result);
            },
            CMP(reg) => {
                let value = self.read_loc(reg);
                let a_val = self.read_loc(A);
                let (result, _) = a_val.overflowing_sub(value);
                self.set_flag(Carry, a_val < value);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) < (value & 0b0000_1111));
                self.update_flags_with(result);
                // Zero flag must be last as it is alsso set in update_flags_with
                self.set_flag(Zero, a_val == value);
            }
            RNZ => {
                if !self.get_flag(Zero) {
                    self.return_from_subroutine();
                }
            },
            POP(reg) => {
                match reg {
                    PSW => {
                        self.write_loc(PSW, self.memory[self.sp as usize] & 0b11010101);
                        self.write_loc(A, self.memory[(self.sp.wrapping_add(1)) as usize]);
                    },
                    reg => {
                        let (rh, rl) = self.get_reg_pair(reg);
                        self.write_loc(rl, self.memory[self.sp as usize]);
                        self.write_loc(rh, self.memory[(self.sp.wrapping_add(1)) as usize]);
                    }
                };
                self.sp = self.sp.wrapping_add(2);
            }
            JNZ => {
                if self.get_flag(Zero) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.pc = addr;
            },
            JMP => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.pc = addr;
            },
            CNZ => {
                if self.get_flag(Zero) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            PUSH(reg) => {
                match reg {
                    PSW => {
                        self.memory[(self.sp.wrapping_sub(1)) as usize] = self.read_loc(A);
                        self.memory[(self.sp.wrapping_sub(2)) as usize] = self.read_loc(PSW) & 0b11010111;
                    },
                    reg => {
                        let (rh, rl) = self.get_reg_pair(reg);
                        self.memory[(self.sp.wrapping_sub(1)) as usize] = self.read_loc(rh);
                        self.memory[(self.sp.wrapping_sub(2)) as usize] = self.read_loc(rl);
                    }
                };
                self.sp = self.sp.wrapping_sub(2);
            },
            ADI => {
                let byte2 = self.fetch_byte();
                let a_val = self.read_loc(A);
                let (result, carry) = a_val.overflowing_add(byte2);
                self.write_loc(A, result);
                self.set_flag(Carry, carry);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) + (byte2 & 0b0000_1111) > 0x0f);
                self.update_flags_with(result);
            },
            RST(n) => {
                assert!(n < 8);
                self.call_subroutine(n as u16 * 8);
            },
            RZ => {
                if self.get_flag(Zero) {
                    self.return_from_subroutine();
                }
            },
            RET => {
                self.return_from_subroutine();
            },
            JZ => {
                if !self.get_flag(Zero) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.pc = addr;
            },
            CZ => {
                if !self.get_flag(Zero) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            CALL => {
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            ACI => {
                let byte2 = self.fetch_byte();
                let a_val = self.read_loc(A);
                let (result, carry) = a_val.overflowing_add(byte2);
                let (result, carry2) = result.overflowing_add(self.get_flag(Carry) as u8);
                self.write_loc(A, result);
                self.set_flag(Carry, carry || carry2);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) + (byte2 & 0b0000_1111) + (self.get_flag(Carry) as u8) > 0x0f);
                self.update_flags_with(result);
            },
            RNC => {
                if !self.get_flag(Carry) {
                    self.return_from_subroutine();
                }
            },
            JNC => {
                if self.get_flag(Carry) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            OUT => {
                // TODO
                let _ = self.fetch_byte();
                println!("{}", self.read_loc(A) as char);
                // print!("{}", self.read_loc(A) as char);
            },
            CNC => {
                if self.get_flag(Carry) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            SUI => {
                let byte2 = self.fetch_byte();
                let a_val = self.read_loc(A);
                let (result, carry) = a_val.overflowing_sub(byte2);
                self.write_loc(A, result);
                self.set_flag(Carry, carry);
                self.update_flags_with(result);
            },
            RC => {
                if !self.get_flag(Carry) {
                    self.return_from_subroutine();
                }
            },
            JC => {
                if !self.get_flag(Carry) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            IN => {
                // TODO
                todo!();
            },
            CC => {
                if !self.get_flag(Carry) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            SBI => {
                let byte2 = self.fetch_byte();
                let a_val = self.read_loc(A);
                let (result, carry) = a_val.overflowing_sub(byte2);
                let (result, carry2) = result.overflowing_sub(self.get_flag(Carry) as u8);
                self.write_loc(A, result);
                self.set_flag(Carry, carry || carry2);
                self.update_flags_with(result);
            }
            RPO => {
                if !self.get_flag(Parity) {
                    self.return_from_subroutine();
                }
            },
            JPO => {
                if !self.get_flag(Parity) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            XTHL => {
                let l = self.read_loc(L);
                let h = self.read_loc(H);
                let sp = self.sp;
                self.write_loc(L, self.memory[sp as usize]);
                self.write_loc(H, self.memory[(sp + 1) as usize]);
                self.memory[sp as usize] = l;
                self.memory[(sp + 1) as usize] = h;
            },
            CPO => {
                if !self.get_flag(Parity) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            ANI => {
                let byte2 = self.fetch_byte();
                let result = self.read_loc(A) & byte2;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, ((self.read_loc(A) | byte2) & 0b0000_1000) != 0);
                self.update_flags_with(result);
            },
            RPE => {
                if self.get_flag(Parity) {
                    self.return_from_subroutine();
                }
            },
            PCHL => {
                let l = self.read_loc(L);
                let h = self.read_loc(H);
                self.pc = ((h as u16) << 8) | (l as u16);
            },
            JPE => {
                if self.get_flag(Parity) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            XCHG => {
                let d = self.read_loc(D);
                let e = self.read_loc(E);
                self.write_loc(D, self.read_loc(H));
                self.write_loc(E, self.read_loc(L));
                self.write_loc(H, d);
                self.write_loc(L, e);
            },
            CPE => {
                if self.get_flag(Parity) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            XRI => {
                let byte2 = self.fetch_byte();
                let result = self.read_loc(A) ^ byte2;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, false);
                self.update_flags_with(result);
            },
            RP => {
                if !self.get_flag(Sign) {
                    self.return_from_subroutine();
                }
            },
            JP => {
                if !self.get_flag(Sign) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            DI => {
                // TODO: interrupt disable
                todo!();
            },
            CP => {
                if !self.get_flag(Sign) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            ORI => {
                let byte2 = self.fetch_byte();
                let result = self.read_loc(A) | byte2;
                self.write_loc(A, result);
                self.set_flag(Carry, false);
                self.set_flag(AuxCarry, false);
                self.update_flags_with(result);
            },
            RM => {
                if self.get_flag(Sign) {
                    self.return_from_subroutine();
                }
            },
            SPHL => {
                self.sp = ((self.read_loc(H) as u16) << 8) | (self.read_loc(L) as u16);
            },
            JM => {
                if self.get_flag(Sign) { return };
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                self.pc = ((byte3 as u16) << 8) | (byte2 as u16);
            },
            EI => {
                // TODO: interrupt enable
                todo!();
            },
            CM => {
                if self.get_flag(Sign) { return }
                let byte2 = self.fetch_byte();
                let byte3 = self.fetch_byte();
                let addr = ((byte3 as u16) << 8) | (byte2 as u16);
                self.call_subroutine(addr);
            },
            CPI => {
                let byte2 = self.fetch_byte();
                let a_val = self.read_loc(A);
                let (result, _) = a_val.overflowing_sub(byte2);
                self.set_flag(Carry, a_val < byte2);
                self.set_flag(AuxCarry, (a_val & 0b0000_1111) < (byte2 & 0b0000_1111));
                self.update_flags_with(result);
                // Zero flag must be last as it is alsso set in update_flags_with
                self.set_flag(Zero, a_val == byte2);
            },
        }
    }
}