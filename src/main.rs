use std::{env, fs::File, io::{self, BufRead, Write, Read}, path::Path};

use itertools::Itertools;
use sim8080::{assemble, Emulator};

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = &args[1];
    let file = Path::new(&args[2]);

    let print = args.contains(&String::from("-p"));
    let silent = args.contains(&String::from("-s"));

    match command.as_str() {
        "assemble" => {
            let _ = assemble_file(file, print, silent, true);
        },
        "run" => {
            let prog = match file.extension().unwrap().to_str().unwrap() {
                "asm" => {
                    if let Ok(_) = assemble_file(file, print, silent, true) {
                        let file = file.with_extension("com");
                        println!(" Running {}", file.display());
                        load_com(&file, false)
                    } else {
                        return;
                    }
                },
                "com" => {
                    let com = load_com(file, false);
                    println!(" Running {}", file.display());
                    com
                },
                "hex" => {
                    let hex = load_hex(file, false);
                    println!(" Running {}", file.display());
                    hex
                },
                _ => {
                    println!("Can only execute .asm, .hex and .com files");
                    return;
                }
            };
            if let Ok(prog) = prog {
                let mut emulator = Emulator::new();
                emulator.load(prog);
                emulator.run()
            }
        },
        "check" => {
            let _ = assemble_file(file, print, silent, false);
        },
        "dump" => {
            // Print out hex / com file
            match file.extension().unwrap().to_str().unwrap() {
                "com" => { let _ = load_com(file, true); },
                "hex" => { let _ = load_hex(file, true); },
                _ => println!("Can only dump .hex and .com files")
            };
        },
        _ => {
            println!("Unknown command: '{}'", command);
            println!("Commands:");
            println!("  assemble <file> [-f <output file>] [-p] [-s]");
            println!("  run <file> [-f <output file>] [-p] [-s]");
            println!("  check <file> [-p]");
            println!("  dump <file> [-p]");
        },
    }
}

fn write_hex(filename: &Path, program: &Vec<(u16, Vec<u8>)>) {
    let mut hex = String::new();
    for (mut addr, bytes) in program {
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

            addr += chunk.len() as u16;
        }
    }

    // Write end of file record
    hex.push_str(":00000001FF\n");
    
    // Write to file
    let mut file = File::create(filename).expect("Failed to create file");
    file.write_all(hex.as_bytes()).expect("Failed to write to file");
}

fn load_hex(filename: &Path, print: bool) -> Result<Vec<(u16, Vec<u8>)>, ()> {
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

fn write_com(filename: &Path, program: &Vec<(u16, Vec<u8>)>) {
    let mut file = File::create(filename).expect("Failed to create file");
    let mut max = 0;
    let mut prog = [0; 0x10000];

    for (addr, bytes) in program {
        max = max.max(addr + bytes.len() as u16);
        
        for i in 0..bytes.len() {
            prog[(*addr + i as u16) as usize] = bytes[i];
        }
    }

    file.write_all(&prog[..max as usize]).expect("Failed to write to file");
}

fn load_com(filename: &Path, print: bool) -> Result<Vec<(u16, Vec<u8>)>, ()> {
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

fn assemble_file(filename: &Path, print: bool, silent: bool, write: bool) -> Result<Vec<(u16, Vec<u8>)>, ()> {
    let before = std::time::Instant::now();
    let args = env::args().collect::<Vec<String>>();
    let file = File::open(filename).expect("Failed to open file");
    let lines = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    let (program, warnings) = match assemble(lines, print) {
        Ok(p) => p,
        Err(e) => {
            println!("Error at line {}: {}", e.line_nr + 1, e.message);
            return Err(());
        },
    };

    if write {
        let dst = args.iter().enumerate().find(|(_, a)| a.as_str() == "-f");
        match dst {
            Some((i, _)) => {
                let dst_file = match args.get(i + 1) {
                    Some(file) => Path::new(file),
                    None => todo!(),
                };
                match dst_file.extension().unwrap().to_str().unwrap() {
                    "com" => {
                        write_com(Path::new(&filename.with_extension("com")), &program)
                    },
                    "hex" => {
                        write_com(Path::new(&filename.with_extension("hex")), &program)
                    }
                    _ => {
                        println!("File extension must be .com or .hex, found: '.{}'", filename.extension().unwrap().to_str().unwrap())
                    }
                }
            
            },
            None => {
                write_com(Path::new(&filename.with_extension("com")), &program);
                write_hex(Path::new(&filename.with_extension("hex")), &program);
            },
        };        
    }

    let after = std::time::Instant::now();
    
    if !silent {
        for warning in warnings {
            println!("Warning at line {}: {}", warning.line_nr + 1, warning.message);
        }

        println!("Assembled in {:?}", (after - before));
    }

    Ok(program)
}