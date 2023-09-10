use std::{env, fs::File, io::{self, BufRead, Write}};

use sim8080::assemble;

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = &args[1];
    match command.as_str() {
        "assemble" | "asmbl" => {
            let file = File::open(
                &args.get(2).expect("Expected a filename")
            ).expect("Failed to open file");
            let lines = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect::<Vec<String>>();
            let program = match assemble(lines) {
                Ok(p) => p,
                Err(e) => {
                    println!("Error at line {}: {}", e.line + 1, e.message);
                    return;
                },
            };

            match args.get(3).map(|s| s.as_str()) {
                Some("-p") => todo!(),
                Some("-f") => {
                    let mut file = File::create(
                        &args.get(4).expect("Expected a filename")
                    ).expect("Failed to create file");
                    file.write_all(&program).expect("Failed to write to file");
                },
                Some(_) => {
                    // Save as inputname.out
                    let mut file = File::create(
                        &args.get(2).unwrap().replace(".asm", ".out")
                    ).expect("Failed to create file");
                    file.write_all(&program).expect("Failed to write to file");
                },
                None => todo!(),
            }
        },
        _ => println!("Unknown command: {}", command),
    }
}