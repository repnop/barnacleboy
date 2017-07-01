use cpu::{Cpu};
use interconnect::Interconnect;
use rom::{Rom, BootRom};

use std;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::io::BufRead;
use std::thread;
use std::time::Duration;

use minifb::{Key, WindowOptions, Window, Scale};

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

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

        let mut buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];

        let mut window = Window::new("Test - ESC to exit",
                                    WIDTH,
                                    HEIGHT,
                                    WindowOptions {
                                        scale: Scale::X2, 
                                    .. WindowOptions::default() }).unwrap_or_else(|e| {
            panic!("{}", e);
        });

        const COLORS: [u32; 4] = [0x00FFFFFF, 0x00606060, 0x00C0C0C0, 0x00000000];
        let mut clrs: [u32; 4] = [0x00FFFFFF, 0x00606060, 0x00C0C0C0, 0x00000000];
        while window.is_open() && !window.is_key_down(Key::Escape) {
            'lp: for i in 0..18 {
                    for k in 0..8 {
                        for c in 0..20 {
                            let (scy, scx) = (self.ic.mem_read_byte(0xFF42) as u16, self.ic.mem_read_byte(0xFF43) as u16);
                            let mapindex = self.ic.mem_read_byte(0x9800 + (i * 20) + scy + (c * 16) + scx + k * 2);
                            let tile = [self.ic.mem_read_byte(0x8000 + mapindex as u16), self.ic.mem_read_byte(0x8001 + mapindex as u16)];
                            buffer[(i * 1280 + c * 8 + k * 160 + 0) as usize] = clrs[(((tile[1] & 0x80) >> 6) | (tile[0] & 0x80) >> 7) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 1) as usize] = clrs[(((tile[1] & 0x40) >> 5) | (tile[0] & 0x40) >> 6) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 2) as usize] = clrs[(((tile[1] & 0x20) >> 4) | (tile[0] & 0x20) >> 5) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 3) as usize] = clrs[(((tile[1] & 0x10) >> 3) | (tile[0] & 0x10) >> 4) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 4) as usize] = clrs[(((tile[1] & 0x08) >> 2) | (tile[0] & 0x08) >> 3) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 5) as usize] = clrs[(((tile[1] & 0x04) >> 1) | (tile[0] & 0x04) >> 2) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 6) as usize] = clrs[(((tile[1] & 0x02) >> 0) | (tile[0] & 0x02) >> 1) as usize];
                            buffer[(i * 1280 + c * 8 + k * 160 + 7) as usize] = clrs[(((tile[1] & 0x01) << 1) | (tile[0] & 0x01) << 0) as usize];

                            if !window.is_open() {
                                break 'lp;
                            }
                        }

                        self.cpu.step(&mut self.ic);
                        //thread::sleep(Duration::from_millis(150));

                        let bgp = self.ic.mem_read_byte(0xFF47);

                        clrs[0] = COLORS[(bgp & 0x3) as usize];
                        clrs[1] = COLORS[((bgp >> 2) & 0x3) as usize];
                        clrs[1] = COLORS[((bgp >> 4) & 0x3) as usize];
                        clrs[1] = COLORS[((bgp >> 6) & 0x3) as usize];

                        
                    }
                }
                let mut ly = self.ic.mem_read_byte(0xFF44);
                if ly == 144 && self.cpu.interrupts_enabled {
                    let pc = self.cpu.pc;
                    self.cpu.push16(&mut self.ic, pc);
                    self.cpu.pc = 0x40;
                    println!("!!!VBLANK!!!");
                }

                if ly + 1 > 153 {
                    ly = 0;
                } else {
                    ly += 1;
                }

                self.ic.mem_write_byte(0xFF44, ly);
                window.update_with_buffer(&buffer);
            }

        return Ok(());
    }
}
