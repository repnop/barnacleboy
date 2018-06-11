use cpu::LRError;

pub trait MemoryInterface {
    type Word;
    type Index;
    type Error;

    fn read(&self, address: Self::Index) -> Result<Self::Word, Self::Error>;
    fn write(&mut self, address: Self::Index, data: Self::Word) -> Result<(), Self::Error>;
}

/// ROM only. Contains 32KiB of ROM mapped to 0x0000 to 0x7FFF, with an optional
/// 8KiB of RAM mapped at 0xA000-0xBFFF.
pub struct RomOnly {
    rom: Box<[u8]>,
    video_ram: Box<[u8]>,
    ram: Option<Box<[u8]>>,
    work_ram_0: Box<[u8]>,
    work_ram_1: Box<[u8]>,
    oam: Box<[u8]>,
    io: Box<[u8]>,
    high_ram: Box<[u8]>,
}

impl RomOnly {
    pub fn blank() -> RomOnly {
        RomOnly {
            rom: vec![0; 32 * 1024].into_boxed_slice(),
            video_ram: vec![0; 8 * 1024].into_boxed_slice(),
            ram: None,
            work_ram_0: vec![0; 4 * 1024].into_boxed_slice(),
            work_ram_1: vec![0; 4 * 1024].into_boxed_slice(),
            oam: vec![0; 160].into_boxed_slice(),
            io: vec![0; 128].into_boxed_slice(),
            high_ram: vec![0; 128].into_boxed_slice(),
        }
    }
}

impl MemoryInterface for RomOnly {
    type Word = u8;
    type Index = u16;
    type Error = LRError;

    fn read(&self, address: Self::Index) -> Result<Self::Word, Self::Error> {
        match address {
            0x0000...0x7FFF => if let Some(&val) = self.rom.get(address as usize) {
                Ok(val)
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            0x8000...0x9FFF => if let Some(&val) = self.video_ram.get(address as usize - 0x8000) {
                Ok(val)
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            0xA000...0xBFFF => if let Some(ram) = &self.ram {
                if let Some(&val) = ram.get(address as usize - 0xA000) {
                    Ok(val)
                } else {
                    Err(LRError::InvalidMemoryRead(address))
                }
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            _ => Err(LRError::InvalidMemoryRead(address)),
        }
    }

    fn write(&mut self, address: Self::Index, data: Self::Word) -> Result<(), Self::Error> {
        match address {
            0x0000...0x7FFF => if let Some(loc) = self.rom.get_mut(address as usize) {
                *loc = data;
            } else {
                return Err(LRError::InvalidMemoryWrite(address));
            },
            0xA000...0xBFFF => if let Some(ram) = &mut self.ram {
                if let Some(val) = ram.get_mut(address as usize - 0xA000) {
                    *val = data;
                } else {
                    return Err(LRError::InvalidMemoryWrite(address));
                }
            } else {
                return Err(LRError::InvalidMemoryWrite(address));
            },
            _ => return Err(LRError::InvalidMemoryWrite(address)),
        }

        Ok(())
    }
}

pub struct Mbc1 {
    rom_banks: Vec<Box<[u8]>>,
    rom_bank_select: usize,
    video_ram: Box<[u8]>,
    ram_banks: Vec<Box<[u8]>>,
    ram_bank_select: usize,
    work_ram_0: Box<[u8]>,
    work_ram_1: Box<[u8]>,
    oam: Box<[u8]>,
    io: Box<[u8]>,
    high_ram: Box<[u8]>,
}

impl Mbc1 {
    pub fn blank(num_rom_banks: usize, ram_size: usize, num_ram_banks: usize) -> Mbc1 {
        Mbc1 {
            rom_banks: vec![vec![0; 16 * 1024].into_boxed_slice(); num_rom_banks],
            rom_bank_select: 0,
            video_ram: vec![0; 8 * 1024].into_boxed_slice(),
            ram_banks: vec![vec![0; ram_size * 1024].into_boxed_slice(); num_ram_banks],
            ram_bank_select: 0,
            work_ram_0: vec![0; 4 * 1024].into_boxed_slice(),
            work_ram_1: vec![0; 4 * 1024].into_boxed_slice(),
            oam: vec![0; 160].into_boxed_slice(),
            io: vec![0; 128].into_boxed_slice(),
            high_ram: vec![0; 128].into_boxed_slice(),
        }
    }
}

// TODO: Bank number in error?
impl MemoryInterface for Mbc1 {
    type Word = u8;
    type Index = u16;
    type Error = LRError;

    fn read(&self, address: Self::Index) -> Result<Self::Word, Self::Error> {
        match address {
            0x0000...0x3FFF => if let Some(&val) = self.rom_banks[0].get(address as usize) {
                Ok(val)
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            0x4000...0x7FFF => if let Some(Some(&val)) = &self.rom_banks
                .get(self.rom_bank_select)
                .map(|v| v.get(address as usize - 0x4000))
            {
                Ok(val)
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            0xA000...0xBFFF => if let Some(Some(&val)) = &self.ram_banks
                .get(self.ram_bank_select)
                .map(|v| v.get(address as usize - 0xA000))
            {
                Ok(val)
            } else {
                Err(LRError::InvalidMemoryRead(address))
            },
            _ => Err(LRError::InvalidMemoryRead(address)),
        }
    }

    fn write(&mut self, address: Self::Index, data: Self::Word) -> Result<(), Self::Error> {
        Ok(())
    }

    /*fn write(&mut self, address: Self::Index, data: Self::Word) -> Result<(), Self::Error> {
        match address {
            0x0000...0x7FFF => if let Some(loc) = self.rom.get_mut(address as usize) {
                *loc = data;
            } else {
                return Err(LRError::InvalidMemoryWrite(address));
            },
            0xA000...0xBFFF => if let Some(ram) = &mut self.ram {
                if let Some(val) = ram.get_mut(address as usize) {
                    *val = data;
                } else {
                    return Err(LRError::InvalidMemoryWrite(address));
                }
            } else {
                return Err(LRError::InvalidMemoryWrite(address));
            },
            _ => return Err(LRError::InvalidMemoryWrite(address)),
        }

        Ok(())
    }*/
}
