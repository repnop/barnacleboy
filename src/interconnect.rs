use mmu::Mmu;
use rom::{Rom, BootRom};
use constants::{ROM_BANK_SIZE, RAM_BANK_SIZE};

pub struct Interconnect {
    pub(self) mmu: Mmu
}

impl Interconnect {
    pub fn new(rom: Rom, bootrom: BootRom) -> Interconnect {
        let romsize: usize = if rom.header.rom_size <= 0x07 {
            (32 * 1024) << (rom.header.rom_size as usize)
        } else {
            match rom.header.rom_size {
                0x52 => (ROM_BANK_SIZE as usize) * 72,
                0x53 => (ROM_BANK_SIZE as usize) * 80,
                0x54 => (ROM_BANK_SIZE as usize) * 96,
                _ => (ROM_BANK_SIZE as usize) * 72
            }
        };

        let ramsize: usize = match rom.header.ram_size {
            0x00 => 0,
            0x01 => 2 * 1024,
            0x02 => 8 * 1024,
            0x03 => 4 * 8 * 1024,
            _ => 0
        };
        
        let mut ic = Interconnect {
            mmu: Mmu::new(romsize, ramsize, rom.header.cartridge_type)
        };

        ic.mmu.load_rom(rom, bootrom);

        ic
    }

    pub fn mem_read_byte(&self, addr: u16) -> u8 {
        self.mmu.read_byte(addr)
    }

    pub fn mem_write_byte(&mut self, addr: u16, value: u8) {
        self.mmu.write_byte(addr, value);
    }

    pub fn mem_read_word(&self, addr: u16) -> u16 {
        self.mmu.read_word(addr)
    }

    pub fn mem_write_word(&mut self, addr: u16, value: u16) {
        self.mmu.write_word(addr, value);
    }

    pub fn disable_internal_rom(&mut self) {
        self.mmu.read_internal_rom = false;
    }
}