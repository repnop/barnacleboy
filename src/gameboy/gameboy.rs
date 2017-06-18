use cpu::cpu::{Cpu, CpuError};
use interconnect::Interconnect;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std;
use std::io::BufRead;

pub struct Gameboy {
    cpu: Cpu,
    ic: Interconnect,
    dmg: String,
    rom: String,
    debug: bool,
    verify: bool
}

impl Gameboy {
    pub fn new(dmg: String, r: String, d: bool, v: bool) -> Gameboy {
        Gameboy {
            cpu: Cpu::new(),
            ic: Interconnect::new(),
            dmg: dmg,
            rom: r,
            debug: d,
            verify: v
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        let rom = File::open(&self.rom);
        let dmg = File::open(&self.dmg);

        if let Ok(mut rom) = rom {
            let mut buf = Vec::new();

            if rom.read_to_end(&mut buf).is_err() {
                return Err("Error reading ROM file".to_string());
            }
            self.ic.mem_write_buffer(0x100, buf.as_slice());
        } else {
            return Err(format!("Could not find the specified ROM file: {}", &self.dmg));
        }

        if let Ok(mut dmg) = dmg {
            let mut buf = Vec::new();

            if dmg.read_to_end(&mut buf).is_err() {
                return Err("Error reading BOOTROM file".to_string());
            }
            self.ic.mem_write_buffer(0x00, buf.as_slice());
        } else {
            return Err(format!("Could not find the specified BOOTROM file: {}", &self.dmg));
        }

        loop {
            print!("\n>");
            std::io::stdout().flush();
            let stdin = std::io::stdin();
            let numtimes = stdin.lock().lines().next().unwrap().unwrap();
            let numtimes = numtimes.parse::<u32>().unwrap_or(1);
            let mut result = Ok(());

            for _ in 0..numtimes {
                result = self.cpu.step(&mut self.ic);
            }

            if result.is_err() {
                match result.err().unwrap() {
                    (CpuError::GPError, ins) => return Err(format!("GPError @ {}: {}", self.cpu.program_counter, ins.disassemble())),
                    (CpuError::UnknownOpcode, ins) => return Err(format!("Unknown Opcode: {}", ins.opcode())),
                    _ => { },
                };
            }
        }

        return Ok(());
    }
}