use sim8080::{Opcode, Register};

use Opcode::*;
use Register::*;

fn main() {
    let prog: Vec<u8> = vec![
        MVI(H).into(), 0, 
        MVI(L).into(), 60,
        MVI(M).into(), 5,
        LDA.into(), 60, 0,
        ADI.into(), 7,
        OUT.into(),
        HLT.into(),
    ];

    let mut emulator = sim8080::Emulator::new();
    emulator.load(prog.as_slice());
    emulator.run();
}