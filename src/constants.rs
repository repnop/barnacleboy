// MMU CONSTANTS

// All constants are inclusive for START ... END

pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

pub const MEMORY_SIZE: u16 = 0xFFFF;
pub const MAX_ROM_BANKS: usize = 32;

pub const ROM_BANKING_MODE: u8 = 0x00;
pub const RAM_BANKING_MODE: u8 = 0x01;

pub const RAM_BANK_SIZE: u16 = 8 * 1024;
pub const ROM_BANK_SIZE: u16 = 16 * 1024;

pub const BOOT_ROM_SIZE: usize = 0x100;

pub const CART_ROM_BANK_00_START: u16 = 0x0000;
pub const CART_ROM_BANK_00_END: u16 = 0x3FFF;
pub const CART_ROM_BANK_00_SIZE: usize = (CART_ROM_BANK_00_END - CART_ROM_BANK_00_START + 1) as
                                         usize;

pub const CART_ROM_BANK_NN_START: u16 = 0x4000;
pub const CART_ROM_BANK_NN_END: u16 = 0x7FFF;
pub const CART_ROM_BANK_NN_SIZE: usize = (CART_ROM_BANK_NN_END - CART_ROM_BANK_NN_START + 1) as
                                         usize;

pub const CARTRIDGE_ROM_SIZE: usize = CART_ROM_BANK_00_SIZE + CART_ROM_BANK_NN_SIZE;

pub const VRAM_START: u16 = 0x8000;
pub const VRAM_END: u16 = 0x9FFF;
pub const VRAM_SIZE: usize = (VRAM_END - VRAM_START + 1) as usize;

pub const CART_EXTERNAL_RAM_START: u16 = 0xA000;
pub const CART_EXTERNAL_RAM_END: u16 = 0xBFFF;
pub const CART_EXTERNAL_RAM_SIZE: usize = (CART_EXTERNAL_RAM_END - CART_EXTERNAL_RAM_START + 1) as
                                          usize;

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


// IO MAPPED MEMORY REGISTERS

pub const P1_REGISTER: u16 = 0xFF00;
pub const SB_REGISTER: u16 = 0xFF01;
pub const SC_REGISTER: u16 = 0xFF02;
pub const DIV_REGISTER: u16 = 0xFF04;
pub const TIMA_REGISTER: u16 = 0xFF05;
pub const TMA_REGISTER: u16 = 0xFF06;
pub const TAC_REGISTER: u16 = 0xFF07;
pub const IF_REGISTER: u16 = 0xFF0F;
pub const NR10_REGISTER: u16 = 0xFF10;
pub const NR11_REGISTER: u16 = 0xFF11;
pub const NR12_REGISTER: u16 = 0xFF12;
pub const NR13_REGISTER: u16 = 0xFF13;
pub const NR14_REGISTER: u16 = 0xFF14;
pub const NR21_REGISTER: u16 = 0xFF16;
pub const NR22_REGISTER: u16 = 0xFF17;
pub const NR23_REGISTER: u16 = 0xFF18;
pub const NR24_REGISTER: u16 = 0xFF19;
pub const NR30_REGISTER: u16 = 0xFF1A;
pub const NR31_REGISTER: u16 = 0xFF1B;
pub const NR32_REGISTER: u16 = 0xFF1C;
pub const NR33_REGISTER: u16 = 0xFF1D;
pub const NR34_REGISTER: u16 = 0xFF1E;
pub const NR41_REGISTER: u16 = 0xFF20;
pub const NR42_REGISTER: u16 = 0xFF21;
pub const NR43_REGISTER: u16 = 0xFF22;
pub const NR44_REGISTER: u16 = 0xFF23;
pub const NR50_REGISTER: u16 = 0xFF24;
pub const NR51_REGISTER: u16 = 0xFF25;
pub const NR52_REGISTER: u16 = 0xFF26;

pub const WAVE_PATTERN_RAM: u16 = 0xFF30;

    // LCD REGISTERS

pub const LCDC_REGISTER: u16 = 0xFF40;
pub const LCDC_STAT_REGISTER: u16 = 0xFF41;
pub const SCY_REGISTER: u16 = 0xFF42;
pub const SCX_REGISTER: u16 = 0xFF43;
pub const LCDC_Y_COORD: u16 = 0xFF44;
pub const LCDC_Y_CMP: u16 = 0xFF45;
pub const DMA_TRANSFER_START: u16 = 0xFF46;
pub const BG_WINDOW_PALETTE_REGISTER: u16 = 0xFF47;
pub const OBJ_PALETTE_0: u16 = 0xFF48;
pub const OBJ_PALETTE_1: u16 = 0xFF49;
pub const WINDOW_Y_POS: u16 = 0xFF4A;
pub const WINDOW_X_POS: u16 = 0xFF4B;

// CPU CONSTANTS

pub const FLAGS_ZERO_INDEX: u8 = 7;
pub const FLAGS_SUB_INDEX: u8 = 6;
pub const FLAGS_HALF_CARRY_INDEX: u8 = 5;
pub const FLAGS_CARRY_INDEX: u8 = 4;

pub const FLAG_ZERO: u8 = 1 << FLAGS_ZERO_INDEX;
pub const FLAG_SUB: u8 = 1 << FLAGS_SUB_INDEX;
pub const FLAG_CARRY: u8 = 1 << FLAGS_CARRY_INDEX;
pub const FLAG_HALF_CARRY: u8 = 1 << FLAGS_HALF_CARRY_INDEX;

pub const INTERRUPT_FLAG_VBLANK: u8 = 1;
pub const INTERRUPT_FLAG_LCD_STAT: u8 = 1 << 1;
pub const INTERRUPT_FLAG_TIMER: u8 = 1 << 2;
pub const INTERRUPT_FLAG_SERIAL: u8 = 1 << 3;
pub const INTERRUPT_FLAG_JOYPAD: u8 = 1 << 4;

pub const REG_A_INDEX: usize = 0b111;
pub const REG_B_INDEX: usize = 0b000;
pub const REG_C_INDEX: usize = 0b001;
pub const REG_D_INDEX: usize = 0b010;
pub const REG_E_INDEX: usize = 0b011;
pub const REG_H_INDEX: usize = 0b100;
pub const REG_L_INDEX: usize = 0b101;

pub const CYCLES_PER_SEC: usize = 4194304;

// TODO: 10s are and average of a 12/8 clock amount for JRs
//  Find a way to differentiate between them
//  Last 4 rows have a lot of averages also
pub static CYCLE_TABLE: [u8; 256] = [
     4,   12,    8,    8,    4,    4,    8,    4,    20,    8,    8,    8,    4,    4,    8,    4,
     4,   12,    8,    8,    4,    4,    8,    4,    12,    8,    8,    8,    4,    4,    8,    4,
    10,   12,    8,    8,    4,    4,    8,    4,    10,    8,    8,    8,    4,    4,    8,    4,
    10,   12,    8,    8,   12,   12,   12,    4,    10,    8,    8,    8,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     8,    8,    8,    8,    8,    8,    4,    8,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
     4,    4,    4,    4,    4,    4,    8,    4,     4,    4,    4,    4,    4,    4,    8,    4,
    14,   12,   14,   16,   18,   16,    8,   16,    14,   16,   14,    4,   18,   24,    8,   16,
    14,   12,   14,    4,   18,   16,    8,   16,    14,   16,   14,    4,   18,    4,    8,   16,
    12,   12,    8,    4,    4,   16,    8,   16,    16,    4,   16,    4,    4,    4,    8,   16,
    12,   12,    8,    4,    4,   16,    8,   16,    12,    8,   16,    4,    4,    4,    8,   16];

pub static R: [usize; 8] = [REG_B_INDEX,
                            REG_C_INDEX,
                            REG_D_INDEX,
                            REG_E_INDEX,
                            REG_H_INDEX,
                            REG_L_INDEX,
                            0b110, // HL
                            REG_A_INDEX];

pub static RP: [usize; 3] = [0, 1, 2];

pub static RP2: [usize; 4] = [0, 1, 2, 3];

pub static CC: [u8; 4] = [FLAG_ZERO, FLAG_ZERO, FLAG_CARRY, FLAG_CARRY];