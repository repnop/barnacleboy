// All constants are inclusive for START ... END

pub const MEMORY_SIZE: u16 = 0xFFFF;

pub const CART_ROM_BANK_00_START: u16 = 0x0000;
pub const CART_ROM_BANK_00_END: u16 = 0x3FFF;
pub const CART_ROM_BANK_00_SIZE: usize = (CART_ROM_BANK_00_END - CART_ROM_BANK_00_START +
                                          1) as usize;

pub const CART_ROM_BANK_NN_START: u16 = 0x4000;
pub const CART_ROM_BANK_NN_END: u16 = 0x7FFF;
pub const CART_ROM_BANK_NN_SIZE: usize = (CART_ROM_BANK_NN_END - CART_ROM_BANK_NN_START +
                                          1) as usize;

pub const CARTRIDGE_ROM_SIZE: usize = CART_ROM_BANK_00_SIZE + CART_ROM_BANK_NN_SIZE;

pub const VRAM_START: u16 = 0x8000;
pub const VRAM_END: u16 = 0x9FFF;
pub const VRAM_SIZE: usize = (VRAM_END - VRAM_START + 1) as usize;

pub const CART_EXTERNAL_RAM_START: u16 = 0xA000;
pub const CART_EXTERNAL_RAM_END: u16 = 0xBFFF;
pub const CART_EXTERNAL_RAM_SIZE: usize = (CART_EXTERNAL_RAM_END - CART_EXTERNAL_RAM_START +
                                           1) as usize;

pub const WORK_RAM_BANK_0_START: u16 = 0xC000;
pub const WORK_RAM_BANK_0_END: u16 = 0xCFFF;
pub const WORK_RAM_BANK_0_SIZE: usize = (WORK_RAM_BANK_0_END - WORK_RAM_BANK_0_START + 1) as usize;

pub const WORK_RAM_BANK_1_START: u16 = 0xD000;
pub const WORK_RAM_BANK_1_END: u16 = 0xDFFF;
pub const WORK_RAM_BANK_1_SIZE: usize = (WORK_RAM_BANK_1_END - WORK_RAM_BANK_1_START + 1) as usize;

pub const WORK_RAM_SIZE: usize = WORK_RAM_BANK_0_SIZE + WORK_RAM_BANK_1_SIZE;

// Same memory as WORK_RAM_BANK_0_START to (WORK_RAM_BANK_1_END - 0x0200)
pub const WORK_RAM_ECHO_START: u16 = 0xE000;
pub const WORK_RAM_ECHO_END: u16 = 0xFDFF;
pub const WORK_RAM_ECHO_SIZE: usize = (WORK_RAM_ECHO_END - WORK_RAM_ECHO_START + 1) as usize;

pub const SPRITE_ATTRIBUTE_TABLE_START: u16 = 0xFE00;
pub const SPRITE_ATTRIBUTE_TABLE_END: u16 = 0xFE9F;
pub const SPRITE_ATTRIBUTE_TABLE_SIZE: usize =
    (SPRITE_ATTRIBUTE_TABLE_END - SPRITE_ATTRIBUTE_TABLE_START + 1) as usize;

pub const UNUSED_RAM_START: u16 = 0xFEA0;
pub const UNUSED_RAM_END: u16 = 0xFEFF;
pub const UNUSED_RAM_SIZE: usize = (UNUSED_RAM_END - UNUSED_RAM_START + 1) as usize;

pub const IO_RAM_START: u16 = 0xFF00;
pub const IO_RAM_END: u16 = 0xFF7F;
pub const IO_RAM_SIZE: usize = (IO_RAM_END - IO_RAM_START + 1) as usize;

pub const HIGH_RAM_START: u16 = 0xFF80;
pub const HIGH_RAM_END: u16 = 0xFFFE;
pub const HIGH_RAM_SIZE: usize = (HIGH_RAM_END - HIGH_RAM_START + 1) as usize;

pub const INTERRUPT_ENABLE_REGISTER: u16 = 0xFFFF;
