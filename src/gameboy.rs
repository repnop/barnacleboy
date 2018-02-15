use cpu::Cpu;
use interconnect::Interconnect;
use rom::{BootRom, Rom};
use gpu::Gpu;
use constants::*;
use debugger::Debugger;

use std;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::io::BufRead;
use std::thread;
use std::time::Duration;

use minifb::{Key, KeyRepeat, Scale, Window, WindowOptions};

pub struct Gameboy {
    cpu: Cpu,
    gpu: Gpu,
    ic: Interconnect,
    debugger: Debugger,
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

            if v {
                dmg = BootRom::new(&buf[..]);
            } else {
                dmg = BootRom::new(&[0; 0x100]);
            }
        } else {
            return Err(format!(
                "Could not find the specified BOOTROM file: {}",
                &df
            ));
        }

        Ok(Gameboy {
            cpu: Cpu::new(),
            gpu: Gpu::new(),
            ic: Interconnect::new(rom, dmg),
            debugger: Debugger::new(),
            dmg: df,
            rom: rf,
            debug: d,
            verify: v,
        })
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut buffer = vec![0xFFFFFFFF; SCREEN_WIDTH * SCREEN_HEIGHT];
        let mut window = Window::new(
            "Test - ESC to exit",
            SCREEN_WIDTH,
            SCREEN_HEIGHT,
            WindowOptions {
                scale: Scale::X2,
                ..WindowOptions::default()
            },
        ).unwrap_or_else(|e| {
            panic!("{}", e);
        });

        if !self.verify {
            println!("No verify. Skipping bootrom");
            self.cpu.regs.a = 0x01;
            self.cpu.regs.f = 0xB0;
            self.cpu.regs.b = 0x00;
            self.cpu.regs.c = 0x13;
            self.cpu.regs.d = 0x00;
            self.cpu.regs.e = 0xD8;
            self.cpu.regs.h = 0x01;
            self.cpu.regs.l = 0x4D;
            self.cpu.sp = 0xFFFE;

            self.ic.mem_write_byte(0xFF05, 0x00);
            self.ic.mem_write_byte(0xFF06, 0x00);
            self.ic.mem_write_byte(0xFF07, 0x00);
            self.ic.mem_write_byte(0xFF10, 0x80);
            self.ic.mem_write_byte(0xFF11, 0xBF);
            self.ic.mem_write_byte(0xFF12, 0xF3);
            self.ic.mem_write_byte(0xFF14, 0xBF);
            self.ic.mem_write_byte(0xFF16, 0x3F);
            self.ic.mem_write_byte(0xFF17, 0x00);
            self.ic.mem_write_byte(0xFF19, 0xBF);
            self.ic.mem_write_byte(0xFF1A, 0x7F);
            self.ic.mem_write_byte(0xFF1B, 0xFF);
            self.ic.mem_write_byte(0xFF1C, 0x9F);
            self.ic.mem_write_byte(0xFF1E, 0xBF);
            self.ic.mem_write_byte(0xFF20, 0xFF);
            self.ic.mem_write_byte(0xFF21, 0x00);
            self.ic.mem_write_byte(0xFF22, 0x00);
            self.ic.mem_write_byte(0xFF23, 0xBF);
            self.ic.mem_write_byte(0xFF24, 0x77);
            self.ic.mem_write_byte(0xFF25, 0xF3);
            self.ic.mem_write_byte(0xFF26, 0xF1);
            self.ic.mem_write_byte(0xFF40, 0x91);
            self.ic.mem_write_byte(0xFF42, 0x00);
            self.ic.mem_write_byte(0xFF43, 0x00);
            self.ic.mem_write_byte(0xFF45, 0x00);
            self.ic.mem_write_byte(0xFF47, 0xFC);
            self.ic.mem_write_byte(0xFF48, 0xFF);
            self.ic.mem_write_byte(0xFF49, 0xFF);
            self.ic.mem_write_byte(0xFF4A, 0x00);
            self.ic.mem_write_byte(0xFF4B, 0x00);
            self.ic.mem_write_byte(0xFFFF, 0x00);

            self.cpu.finished_bootrom = true;
            self.ic.disable_internal_rom();
            self.cpu.pc = 0x100;
            self.cpu.interrupts_enabled = true;
        }

        while window.is_open() && !window.is_key_down(Key::Escape) {
            let mut clrs: [u32; 4] = [0x00FFFFFF, 0x00606060, 0x00C0C0C0, 0x00000000];

            let mode = self.ic.mem_read_byte(0xFF41) & 0x3;

            let mut cycles_to_execute: i16 = match mode {
                0 => 204,
                1 => 4560,
                2 => 80,
                3 => 172,
                _ => 204,
            };

            /*if self.debug {
                println!("Processing {} cycles", cycles_to_execute);
            }*/

            if self.debug & window.is_key_pressed(Key::F1, KeyRepeat::No) {
                let pc = self.cpu.pc - 1;
                self.debugger
                    .enter_debug_mode(&mut self.ic, &mut self.cpu, pc, false);
            }

            while cycles_to_execute > 0 {
                if !self.cpu.stopped {
                    if !self.cpu.halted {
                        let (brkpt, cycles_done) = self.cpu.step(&mut self.ic, self.debug);
                        cycles_to_execute -= cycles_done as i16;

                        if brkpt {
                            let pc = self.cpu.pc - 1;
                            self.debugger
                                .enter_debug_mode(&mut self.ic, &mut self.cpu, pc, true);
                        }
                    }
                }

                let mut joypadreg = self.ic.mem_read_byte(0xFF00);

                // Direction buttons
                if window.is_key_pressed(Key::Up, KeyRepeat::Yes) {
                    joypadreg &= !((1 << 4) | (1 << 2));
                } else if window.is_key_pressed(Key::Down, KeyRepeat::Yes) {
                    joypadreg &= !((1 << 4) | (1 << 3));
                } else if window.is_key_pressed(Key::Left, KeyRepeat::Yes) {
                    joypadreg &= !((1 << 4) | (1 << 1));
                } else if window.is_key_pressed(Key::Right, KeyRepeat::Yes) {
                    joypadreg &= !((1 << 4) | (1 << 0));
                }

                if window.is_key_pressed(Key::Z, KeyRepeat::No) {
                    joypadreg &= !((1 << 5) | (1 << 3));
                } else if window.is_key_pressed(Key::X, KeyRepeat::No) {
                    joypadreg &= !((1 << 5) | (1 << 3));
                }

                self.ic.mem_write_byte(0xFF00, joypadreg);
            }

            //self.cpu.check_and_handle_interrupts(&mut self.ic, self.debug);
            self.gpu.update(&mut self.ic);

            if window.is_key_pressed(Key::F1, KeyRepeat::No) {
                let pc = self.cpu.pc - 1;
                self.debugger
                    .enter_debug_mode(&mut self.ic, &mut self.cpu, pc, false);
            }

            window.update_with_buffer(&self.gpu.screen_buffer);
        }

        return Ok(());
    }
}
