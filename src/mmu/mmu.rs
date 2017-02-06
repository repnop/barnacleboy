use mmu::memconsts::*;

pub struct Memory {
    cart_rom: Box<[u8]>,
    cart_ram: Box<[u8]>,
    video_ram: Box<[u8]>,
    work_ram: Box<[u8]>,
    sprite_attribute_table: Box<[u8]>,
    io_port_ram: Box<[u8]>,
    high_ram: Box<[u8]>,
    interrupt_register: u8,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            cart_rom: vec![0; CARTRIDGE_ROM_SIZE].into_boxed_slice(),
            cart_ram: vec![0; CART_EXTERNAL_RAM_SIZE].into_boxed_slice(),
            video_ram: vec![0; VRAM_SIZE].into_boxed_slice(),
            work_ram: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            sprite_attribute_table: vec![0; SPRITE_ATTRIBUTE_TABLE_SIZE].into_boxed_slice(),
            io_port_ram: vec![0; IO_RAM_SIZE].into_boxed_slice(),
            high_ram: vec![0; HIGH_RAM_SIZE].into_boxed_slice(),
            interrupt_register: 0,
        }
    }

    pub fn read_byte(&self, mem_loc: u16) -> u8 {
        match mem_loc {
            CART_ROM_BANK_00_START...CART_ROM_BANK_NN_END => self.cart_rom[mem_loc as usize],
            VRAM_START...VRAM_END => self.video_ram[(mem_loc - VRAM_START) as usize],
            CART_EXTERNAL_RAM_START...CART_EXTERNAL_RAM_END => {
                self.cart_ram[(mem_loc - CART_EXTERNAL_RAM_START) as usize]
            }
            WORK_RAM_BANK_0_START...WORK_RAM_BANK_1_END => {
                self.work_ram[(mem_loc - WORK_RAM_BANK_0_START) as usize]
            }
            WORK_RAM_ECHO_START...WORK_RAM_ECHO_END => {
                self.work_ram[(mem_loc - WORK_RAM_ECHO_START) as usize]
            }
            SPRITE_ATTRIBUTE_TABLE_START...SPRITE_ATTRIBUTE_TABLE_END => {
                self.sprite_attribute_table[(mem_loc - SPRITE_ATTRIBUTE_TABLE_START) as usize]
            }
            UNUSED_RAM_START...UNUSED_RAM_END => {
                panic!("Attempted read from unusable RAM: {:#x}", mem_loc)
            }
            IO_RAM_START...IO_RAM_END => self.io_port_ram[(mem_loc - IO_RAM_START) as usize],
            HIGH_RAM_START...HIGH_RAM_END => self.high_ram[(mem_loc - HIGH_RAM_START) as usize],
            INTERRUPT_ENABLE_REGISTER => self.interrupt_register,

            _ => unreachable!(),
        }
    }

    pub fn write_byte(&mut self, mem_loc: u16, value: u8) {
        match mem_loc {
            CART_ROM_BANK_00_START...CART_ROM_BANK_NN_END => {
                self.cart_rom[mem_loc as usize] = value;
            }
            VRAM_START...VRAM_END => {
                self.video_ram[(mem_loc - VRAM_START) as usize] = value;
            }
            CART_EXTERNAL_RAM_START...CART_EXTERNAL_RAM_END => {
                self.cart_ram[(mem_loc - CART_EXTERNAL_RAM_START) as usize] = value;
            }
            WORK_RAM_BANK_0_START...WORK_RAM_BANK_1_END => {
                self.work_ram[(mem_loc - WORK_RAM_BANK_0_START) as usize] = value;
            }
            WORK_RAM_ECHO_START...WORK_RAM_ECHO_END => {
                self.work_ram[(mem_loc - WORK_RAM_ECHO_START) as usize] = value;
            }
            SPRITE_ATTRIBUTE_TABLE_START...SPRITE_ATTRIBUTE_TABLE_END => {
                self.sprite_attribute_table[(mem_loc - SPRITE_ATTRIBUTE_TABLE_START) as usize] =
                    value;
            }
            UNUSED_RAM_START...UNUSED_RAM_END => {
                panic!("Attempted read from unusable RAM: {:#x}", mem_loc)
            }
            IO_RAM_START...IO_RAM_END => {
                self.io_port_ram[(mem_loc - IO_RAM_START) as usize] = value;
            }
            HIGH_RAM_START...HIGH_RAM_END => {
                self.high_ram[(mem_loc - HIGH_RAM_START) as usize] = value;
            }
            INTERRUPT_ENABLE_REGISTER => {
                self.interrupt_register = value;
            }

            _ => unreachable!(),
        }
    }

    pub fn read_word(&self, mem_loc: u16) -> u16 {
        ((self.read_byte(mem_loc) as u16) << 8) | (self.read_byte(mem_loc + 1) as u16)
    }

    pub fn write_word(&mut self, mem_loc: u16, value: u16) {
        self.write_byte(mem_loc, (value >> 8) as u8);
        self.write_byte(mem_loc + 1, (value & 0x00FF) as u8);
    }
}
