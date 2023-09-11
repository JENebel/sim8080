use std::{env, fs::File, io::{self, BufRead, Write}};

use sim8080::{assemble, Emulator};

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = &args[1];
    match command.as_str() {
        "assemble" | "asmbl" => {
            let file_name = &args[2];

            match args.get(3).map(|s| s.as_str()) {
                Some("-check") => {
                    match assemble_file(file_name) {
                        Ok(_) => println!("No errors"),
                        Err(_) => (),
                    }
                },
                Some("-f") => {
                    let program = match assemble_file(file_name) {
                        Ok(p) => p,
                        Err(_) => return,
                    };
                    let mut file = File::create(
                        &args.get(4).expect("Expected a filename")
                    ).expect("Failed to create file");
                    file.write_all(gen_hex(program).as_bytes()).expect("Failed to write to file");
                },
                Some(_) => {
                    println!("Unknown flag: {}", args.get(3).unwrap());
                },
                None => {
                    let program = match assemble_file(file_name) {
                        Ok(p) => p,
                        Err(_) => return,
                    };
                    // Save as inputname.out
                    let mut file = File::create(
                        &args.get(2).unwrap().replace(".asm", ".hex")
                    ).expect("Failed to create file");
                    file.write_all(gen_hex(program).as_bytes()).expect("Failed to write to file");
                },
            }
        },
        "run" => {
            let file_name = &args[2];
            let program = match assemble_file(file_name) {
                Ok(p) => p,
                Err(_) => return,
            };

            let mut cpu = Emulator::new();
            cpu.load(program);
            cpu.run();
        }
        _ => println!("Unknown command: {}", command),
    }
}

fn gen_hex(program: Vec<(u16, Vec<u8>)>) -> String {
    let mut hex = String::new();
    for (addr, bytes) in program {
        // Write out with max 16 bytes per line
        for chunk in bytes.chunks(16) {
            // Write byte count with 2 hex pairs
            hex.push_str(&format!(":{:02X}", chunk.len()));
            // Write address with 2 hex pairs
            hex.push_str(&format!("{:04X}", addr));
            // Write record type with 2 hex pairs
            hex.push_str("00");
            // Write data with 2 hex pairs per byte
            for i in 0..chunk.len() {
                hex.push_str(&format!("{:02X}", chunk[i]));
            }
            // Write checksum with 2 hex pairs
            let sum: u32 = chunk.len() as u32 + addr as u32 + 0x00 + chunk.iter().map(|b| *b as u32).sum::<u32>();
            let checksum = (!(sum)) + 1;
            hex.push_str(&format!("{:02X}", checksum as u8));

            hex.push('\n');
        }
    }

    // Write end of file record
    hex.push_str(":00000001FF");
    
    hex
}

fn assemble_file(filename: &str) -> Result<Vec<(u16, Vec<u8>)>, ()> {
    let file = File::open(filename).expect("Failed to open file");
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    let (program, warnings) = match assemble(lines) {
        Ok(p) => p,
        Err(e) => {
            println!("Error at line {}: {}", e.line_nr + 1, e.message);
            return Err(());
        },
    };

    for warning in warnings {
        println!("Warning at line {}: {}", warning.line_nr + 1, warning.message);
    }

    Ok(program)
}