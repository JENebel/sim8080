use core::panic;
use std::{env, fs::File, io::{self, BufRead, Write, Read}, path::Path};

use itertools::Itertools;
use sim8080::{assemble, Emulator};

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = &args[1];
    match command.as_str() {
        "assemble" | "asmbl" => {
            let file_name = &args[2];
            let name = Path::new(file_name).file_stem().unwrap().to_str().unwrap().to_string();

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
                    
                    write_hex(&args.get(4).unwrap().to_string(), &program);
                    write_com(&args.get(4).unwrap().to_string(), &program);
                },
                Some(_) => {
                    println!("Unknown flag: {}", args.get(3).unwrap());
                },
                None => {
                    let program = match assemble_file(file_name) {
                        Ok(p) => p,
                        Err(_) => return,
                    };
                    write_hex(&name, &program);
                    write_com(&name, &program);
                },
            }
        },
        "run" => {
            let file_name = &args[2];
            let extension = Path::new(file_name).extension();
            let program = match extension.map(|s| s.to_str().unwrap()) {
                Some("asm") => match assemble_file(file_name) {
                    Ok(p) => p,
                    Err(_) => return,
                },
                Some("hex") => {
                    match load_hex(file_name.to_string(), false) {
                        Ok(p) => p,
                        Err(e) => {
                            println!("Failed to load hex file: {}", e);
                            return
                        },
                    }
                },
                Some("com") => {
                    match load_com(file_name.to_string(), false) {
                        Ok(p) => p,
                        Err(e) => {
                            println!("Failed to load com file: {}", e);
                            return
                        },
                    }
                },
                Some(_) => {
                    println!("Unsupported file type");
                    return
                },
                None => todo!(), // Binary
            };

            let mut cpu = Emulator::new();
            cpu.load(program);
            cpu.run();
        },
        "dump" => {
            let file_name = &args[2];
            let extension = Path::new(file_name).extension();
            match extension.unwrap().to_str().unwrap() {
                "hex" => match load_hex(file_name.to_string(), true) {
                    Ok(_) => {},
                    Err(e) => {
                        println!("Failed to load hex file: {}", e);
                        panic!()
                    },
                },
                "com" => match load_com(file_name.to_string(), true) {
                    Ok(_) => {},
                    Err(e) => {
                        println!("Failed to load com file: {}", e);
                        panic!()
                    },
                },
                _ => panic!(),
            }
        }
        _ => println!("Unknown command: {}", command),
    }
}

fn write_hex(filename: &String, program: &Vec<(u16, Vec<u8>)>) {
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
            let sum: u32 = chunk.len() as u32 + *addr as u32 + 0x00 + chunk.iter().map(|b| *b as u32).sum::<u32>();
            let checksum = (!(sum)) + 1;
            hex.push_str(&format!("{:02X}", checksum as u8));

            hex.push('\n');
        }
    }

    // Write end of file record
    hex.push_str(":00000001FF\n");
    
    // Write to file
    let mut file = File::create(format!("{filename}.com")).expect("Failed to create file");
    file.write_all(hex.as_bytes()).expect("Failed to write to file");
}

fn load_hex(filename: String, print: bool) -> Result<Vec<(u16, Vec<u8>)>, String> {
    let file = File::open(filename).expect("Failed to open file");
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect::<Vec<String>>();

    let mut res = Vec::new();

    for line in lines {
        let (_, line) = line.split_once(':').unwrap();
        if line == "00000001FF" {
            break;
        }
        let mut bytes = line.chars().chunks(2).into_iter()
            .map(|c| u8::from_str_radix(&c.collect::<String>(), 16).unwrap())
            .collect::<Vec<u8>>();
        
        let byte_count = bytes.remove(0);
        let address = ((bytes.remove(0) as u16) << 8) | bytes.remove(0) as u16;
        let record_type = bytes.remove(0);
        let checksum = bytes.pop().unwrap();
        let data = bytes;
        let sum = data.iter().map(|b| *b as u32).sum::<u32>() + address as u32 + record_type as u32 + byte_count as u32 + checksum as u32;
        assert!(sum as u8 == 0);

        if print {
            println!("{}:\t{}", address, data.iter().map(|b| format!("{:02X}", b)).join(" "));
        }

        res.push((address, data));
    }

    Ok(res)
}

fn write_com(filename: &String, program: &Vec<(u16, Vec<u8>)>) {
    let mut file = File::create(format!("{filename}.com")).expect("Failed to create file");
    let mut loc = 0;
    for (addr, bytes) in program {
        if *addr > loc {
            file.write_all(&vec![0; (addr - loc) as usize]).expect("Failed to write to file");
            loc = *addr;
        }
        
        for byte in bytes {
            file.write_all(&[*byte]).expect("Failed to write to file");
            loc += bytes.len() as u16;
        }
    }
}

fn load_com(filename: String, print: bool) -> Result<Vec<(u16, Vec<u8>)>, String> {
    let file = File::open(filename).expect("Failed to open file");
    // Read raw bytes to array
    let mut reader = io::BufReader::new(file);
    let mut bytes = Vec::new();
    let _ = reader.read_to_end(&mut bytes).unwrap();

    if print {
        println!("{}", bytes.iter().map(|b| format!("{:02X}", b)).join(" "));
    }

    Ok(vec![(0, bytes)])
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