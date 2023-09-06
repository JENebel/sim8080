use sim8080::{Instruction, Register};

use Instruction::*;
use Register::*;

fn main() {
    let prog: Vec<u8> = vec![
        MVI(E).into(), 5,  // E = 5
        MOV(E, A).into(),  // E = A
        OUT.into(),
        HLT.into(),
    ];

    let mut emulator = sim8080::Emulator::new();
    emulator.load(prog.as_slice());
    emulator.run();
}
