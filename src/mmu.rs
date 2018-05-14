use std::convert::From;
use std;
use std::io::Write;

use constants::*;
use rom::{Rom, BootRom};

enum MemoryBankController {
    None,
    MBC1,
    MBC2,
    MBC3,
    MBC4,
    MBC5,
    HuC1,
    HuC3
}

impl From<u8> for MemoryBankController {
    fn from(code: u8) -> MemoryBankController {
        match code {
            0x00 | 0x08 | 0x09 => MemoryBankController::None,
            0x01 ... 0x03 => MemoryBankController::MBC1,
            0x05 | 0x06 => MemoryBankController::MBC2,
            0x0F ... 0x13 => MemoryBankController::MBC3,
            0x15 ... 0x17 => MemoryBankController::MBC4,
            0x19 ... 0x1E => MemoryBankController::MBC5,
            0xFE => MemoryBankController::HuC3,
            0xFF => MemoryBankController::HuC1,
            _ => MemoryBankController::None
        }
    }
}

pub struct Mmu {
    boot_rom: Box<[u8]>,
    cart_rom: Box<[u8]>,
    cart_ram: Box<[u8]>,
    video_ram: Box<[u8]>,
    work_ram_0: Box<[u8]>,
    work_ram_1: Box<[u8]>,
    unused_ram: Box<[u8]>,
    sprite_attribute_table: Box<[u8]>,
    io_port_ram: Box<[u8]>,
    high_ram: Box<[u8]>,
    interrupt_register: u8,
    mem_bank_controller: MemoryBankController,
    ram_bank_index: u16,
    rom_bank_index: u16,
    rom_ram_mode: u8,
    ram_enabled: bool,
    ram_size: usize,
    rom_size: usize,
    pub read_internal_rom: bool
}

impl Mmu {
    pub fn new(cart_rom_size: usize, cart_ram_size: usize, mbc: u8) -> Mmu {
        Mmu {
            boot_rom: vec![0; BOOT_ROM_SIZE].into_boxed_slice(),
            cart_rom: vec![0; cart_rom_size].into_boxed_slice(),
            cart_ram: vec![0; cart_ram_size].into_boxed_slice(),
            video_ram: vec![0; VRAM_SIZE].into_boxed_slice(),
            work_ram_0: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            work_ram_1: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            unused_ram: vec![0; UNUSED_RAM_SIZE].into_boxed_slice(),
            sprite_attribute_table: vec![0; SPRITE_ATTRIBUTE_TABLE_SIZE].into_boxed_slice(),
            io_port_ram: vec![0; IO_RAM_SIZE].into_boxed_slice(),
            high_ram: vec![0; HIGH_RAM_SIZE].into_boxed_slice(),
            interrupt_register: 0,
            mem_bank_controller: MemoryBankController::from(mbc),
            ram_bank_index: 0x00,
            rom_bank_index: 0x01,
            rom_ram_mode: 0x00,
            ram_enabled: false,
            ram_size: cart_ram_size,
            rom_size: cart_rom_size,
            read_internal_rom: true
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            CART_ROM_BANK_00_START ... CART_ROM_BANK_00_END => {
                if self.read_internal_rom {
                    //println!("Tried writing to internal boot rom");
                } else {
                    if addr <= 0x1FFF {
                        match self.mem_bank_controller {
                            MemoryBankController::None => println!("Tried writing to ROM bank 0"),
                            MemoryBankController::MBC1 => {
                                match value & 0x0F {
                                    0x0A => self.ram_enabled = true,
                                    _ => self.ram_enabled = false
                                };
                            },
                            MemoryBankController::MBC2 => {
                                if addr & 0x0100 == 0x0000 {
                                    self.ram_enabled = !self.ram_enabled;
                                }
                            },
                            MemoryBankController::MBC3 => {
                                // TODO RTC Timer
                                match value & 0x0F {
                                    0x0A => self.ram_enabled = true,
                                    _ => self.ram_enabled = false
                                };
                            }
                            _ => { }
                        };
                    } else if addr >= 0x2000 {
                        match self.mem_bank_controller {
                            MemoryBankController::None => println!("Tried writing to ROM bank 0"),
                            MemoryBankController::MBC1 | MemoryBankController::MBC2 => {
                                match value {
                                    0x00 | 0x20 | 0x40 | 0x60 => self.rom_bank_index = (value + 1) as u16,
                                    _ => self.rom_bank_index |= (0x1F & value) as u16
                                };
                            },
                            MemoryBankController::MBC3 => {
                                self.rom_bank_index = value as u16;
                            }
                            _ => { }
                        };
                    }
                }
            },

            CART_ROM_BANK_NN_START ... CART_ROM_BANK_NN_END => {
                if addr <= 0x5FFF {
                    if self.rom_ram_mode == ROM_BANKING_MODE {
                        self.rom_bank_index |= ((value << 5) & 0x60) as u16;
                    } else {
                        self.ram_bank_index = value as u16;
                    }
                    // TODO: RTC Register stuff
                } else {
                    match self.mem_bank_controller {
                        MemoryBankController::MBC1 => {
                            self.rom_ram_mode = value & 0x01;
                        },
                        MemoryBankController::MBC3 => {
                            // TODO: Latch Clock Data to RTC
                        },
                        _ => { }
                    };
                }
            }

            VRAM_START ... VRAM_END => self.video_ram[(addr - VRAM_START) as usize] = value,

            CART_EXTERNAL_RAM_START ... CART_EXTERNAL_RAM_END => {
                match self.mem_bank_controller {
                    MemoryBankController::None => {
                        if self.ram_size > 0 {
                            self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize] = value;
                        }
                    }
                    MemoryBankController::MBC1 => {
                        if self.ram_enabled {
                            if self.ram_size > 8 * 1024 {
                                self.cart_ram[((self.ram_bank_index * RAM_BANK_SIZE) +
                                    addr - CART_EXTERNAL_RAM_START) as usize] = value;
                            } else if self.ram_size > 0 {
                                self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize] = value;
                            }
                        }
                    },
                    MemoryBankController::MBC2 => {
                        self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize] |= value & 0x0F;
                    },
                    MemoryBankController::MBC3 => {
                        if self.ram_enabled {
                            if self.ram_size > 8 * 1024 {
                                self.cart_ram[((self.ram_bank_index * RAM_BANK_SIZE) + 
                                addr - CART_EXTERNAL_RAM_START) as usize] = value;
                            } else if self.ram_size > 0 {
                                self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize] = value;
                            }
                        }

                        // TODO: RTC stuff 
                    }
                    _ => { }
                }
            },

            WORK_RAM_BANK_0_START ... WORK_RAM_BANK_0_END =>
                self.work_ram_0[(addr - WORK_RAM_BANK_0_START) as usize] = value,

            WORK_RAM_BANK_1_START ... WORK_RAM_BANK_1_END =>
                self.work_ram_1[(addr - WORK_RAM_BANK_1_START) as usize] = value,

            WORK_RAM_ECHO_START ... WORK_RAM_ECHO_END =>
                self.work_ram_0[(addr - WORK_RAM_ECHO_START) as usize] = value,

            SPRITE_ATTRIBUTE_TABLE_START ... SPRITE_ATTRIBUTE_TABLE_END =>
                self.sprite_attribute_table[(addr - SPRITE_ATTRIBUTE_TABLE_START) as usize] = value,

            UNUSED_RAM_START ... UNUSED_RAM_END =>
                self.unused_ram[(addr - UNUSED_RAM_START) as usize] = value,
                //println!("Tried writing to unusable RAM"),

            IO_RAM_START ... IO_RAM_END => {
                // TODO: Check if any IO registers are cleared by writing
                self.io_port_ram[(addr - IO_RAM_START) as usize] = value;

                if addr == 0xFF01 {
                    println!("Write to serial port: {:X} (char: {})", value, value as char);
                }
            }

            INTERRUPT_ENABLE_REGISTER => self.interrupt_register = value,

            _ => { }
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            CART_ROM_BANK_00_START ... CART_ROM_BANK_00_END => {
                if addr <= 0xFF && self.read_internal_rom {
                    self.boot_rom[addr as usize]
                } else {
                    self.cart_rom[(addr - CART_ROM_BANK_00_START) as usize]
                }
            },
            CART_ROM_BANK_NN_START ... CART_ROM_BANK_NN_END => {
                match self.mem_bank_controller {
                    MemoryBankController::None => self.cart_rom[(addr - CART_ROM_BANK_NN_START) as usize],
                    _ => {
                        if self.rom_ram_mode == ROM_BANKING_MODE {
                            self.cart_rom[((self.rom_bank_index * ROM_BANK_SIZE) + 
                                addr - CART_ROM_BANK_NN_START) as usize]
                        } else {
                            self.cart_rom[(addr - CART_ROM_BANK_NN_START) as usize]
                        }
                    }
                }
            }

            VRAM_START ... VRAM_END => self.video_ram[(addr - VRAM_START) as usize],

            CART_EXTERNAL_RAM_START ... CART_EXTERNAL_RAM_END => {
                match self.mem_bank_controller {
                    MemoryBankController::None => self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize],
                    _ => {
                        if self.rom_ram_mode == RAM_BANKING_MODE {
                            self.cart_ram[((self.rom_bank_index * RAM_BANK_SIZE) + 
                                addr - CART_EXTERNAL_RAM_START) as usize]
                        } else {
                            self.cart_ram[(addr - CART_EXTERNAL_RAM_START) as usize]
                        }
                    }
                }
            },

            WORK_RAM_BANK_0_START ... WORK_RAM_BANK_0_END =>
                self.work_ram_0[(addr - WORK_RAM_BANK_0_START) as usize],

            WORK_RAM_BANK_1_START ... WORK_RAM_BANK_1_END =>
                self.work_ram_1[(addr - WORK_RAM_BANK_1_START) as usize],

            WORK_RAM_ECHO_START ... WORK_RAM_ECHO_END =>
                self.work_ram_0[(addr - WORK_RAM_ECHO_START) as usize],

            SPRITE_ATTRIBUTE_TABLE_START ... SPRITE_ATTRIBUTE_TABLE_END =>
                self.sprite_attribute_table[(addr - SPRITE_ATTRIBUTE_TABLE_START) as usize],

            UNUSED_RAM_START ... UNUSED_RAM_END => 
                self.unused_ram[(addr - UNUSED_RAM_START) as usize],

            IO_RAM_START ... IO_RAM_END => {
                self.io_port_ram[(addr - IO_RAM_START) as usize]
            }

            INTERRUPT_ENABLE_REGISTER => self.interrupt_register,

            _ => 0
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr + 1) as u16) << 8) | ( 0x00FF & self.read_byte(addr) as u16)
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr + 1, (value >> 8) as u8);
        self.write_byte(addr, (value & 0x00FF) as u8);
    }

    pub fn load_rom(&mut self, rom: Rom, bootrom: BootRom) {
        let mut index = 0usize;
        
        for byte in rom.ivt.iter() {
            self.cart_rom[index] = *byte;
            index += 1;
        }

        let headerbytes: [u8; 0x50] = rom.header.into();

        for byte in headerbytes.iter() {
            self.cart_rom[index] = *byte;
            index += 1;
        }

        for byte in rom.banks {
            self.cart_rom[index] = byte;
            index += 1;
        }

        index = 0;

        for byte in bootrom.0.iter() {
            self.boot_rom[index] = *byte;
            index += 1;
        }
    }
}