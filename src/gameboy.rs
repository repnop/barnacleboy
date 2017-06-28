use cpu::{Cpu};
use interconnect::Interconnect;
use rom::{Rom, BootRom};

use std;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::io::BufRead;

use glutin;
use gfx_window_glutin;
use gfx;
use gfx::Device;

pub type ColorFormat = gfx::format::Rgba8;
pub type DepthFormat = gfx::format::DepthStencil;

pub struct Gameboy {
    cpu: Cpu,
    ic: Interconnect,
    dmg: String,
    rom: String,
    debug: bool,
    verify: bool,
}

impl Gameboy {
    pub fn new(df: String, rf: String, d: bool, v: bool) -> Result<Gameboy, String> {
        let romfile = File::open(&rf);
        let dmgfile = File::open(&df);
        let rom;
        let dmg;

        if let Ok(mut ok_rom) = romfile {
            let mut buf = Vec::new();

            if ok_rom.read_to_end(&mut buf).is_err() {
                return Err("Error reading ROM file".to_string());
            }

            rom = Rom::new(&buf[..]);
        } else {
            return Err(format!("Could not find the specified ROM file: {}", &rf));
        }

        if let Ok(mut ok_dmg) = dmgfile {
            let mut buf = Vec::new();

            if ok_dmg.read_to_end(&mut buf).is_err() {
                return Err("Error reading BOOTROM file".to_string());
            }
            dmg = BootRom::new(&buf[..]);
        } else {
            return Err(format!("Could not find the specified BOOTROM file: {}", &df));
        }

        Ok(Gameboy {
            cpu: Cpu::new(),
            ic: Interconnect::new(rom, dmg),
            dmg: df,
            rom: rf,
            debug: d,
            verify: v,
        })
    }

    pub fn run(&mut self) -> Result<(), String> {

        let builder = glutin::WindowBuilder::new()
            .with_title(String::from("BarnacleBoy - ") + &self.rom)
            .with_dimensions(160, 144)
            .with_vsync();

        let (window, mut device, mut factory, main_color, mut main_depth) =
            gfx_window_glutin::init::<ColorFormat, DepthFormat>(builder);

        'run: loop {
            /*print!("\n>");
            std::io::stdout().flush();
            let stdin = std::io::stdin();
            let numtimes = stdin.lock().lines().next().unwrap().unwrap();
            let numtimes = numtimes.parse::<u32>().unwrap_or(1);*/
            /*let mut result = Ok(());

            //for _ in 0..numtimes {
            //result = self.cpu.step(&mut self.ic);
            //self.screen.update(&mut self.ic);
            //}

            if result.is_err() {
                match result.err() {
                    Some(e) => {
                        /*match e {
                            (CpuError::GPError, ins) => return Err(format!("GPError @ {}: {}", self.cpu.program_counter, ins.disassemble())),
                            (CpuError::UnknownOpcode, ins) => return Err(format!("Unknown Opcode: {}", ins.opcode())),
                            _ => { },
                        };*/
                    }
                    None => {}
                };
            }*/

            for event in window.poll_events() {
                match event {
                    glutin::Event::Closed => break 'run,
                    _ => {}
                }
            }

            window.swap_buffers().unwrap();
            device.cleanup();
        }

        return Ok(());
    }
}
