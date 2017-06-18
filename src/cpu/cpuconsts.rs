use cpu::cpu::WordRegister;

pub const FLAGS_ZERO_INDEX: u8 = 7;
pub const FLAGS_SUB_INDEX: u8 = 6;
pub const FLAGS_HALF_CARRY_INDEX: u8 = 5;
pub const FLAGS_CARRY_INDEX: u8 = 4;

pub const FLAG_ZERO: u8 = 1 << FLAGS_ZERO_INDEX;
pub const FLAG_SUB: u8 = 1 << FLAGS_SUB_INDEX;
pub const FLAG_CARRY: u8 = 1 << FLAGS_CARRY_INDEX;
pub const FLAG_HALF_CARRY: u8 = 1 << FLAGS_HALF_CARRY_INDEX;

pub const REG_A_INDEX: usize = 0b111;
pub const REG_B_INDEX: usize = 0b000;
pub const REG_C_INDEX: usize = 0b001;
pub const REG_D_INDEX: usize = 0b010;
pub const REG_E_INDEX: usize = 0b011;
pub const REG_H_INDEX: usize = 0b100;
pub const REG_L_INDEX: usize = 0b101;

pub const CYCLES_PER_SEC: usize = 4194304;

pub static R: [usize; 8] = [REG_B_INDEX,
                        REG_C_INDEX,
                        REG_D_INDEX,
                        REG_E_INDEX,
                        REG_H_INDEX,
                        REG_L_INDEX,
                        0b110, // HL
                        REG_A_INDEX];

pub static RP: [WordRegister; 4] =
    [WordRegister::BC, WordRegister::DE, WordRegister::HL, WordRegister::SP];

pub static RP2: [WordRegister; 4] =
    [WordRegister::BC, WordRegister::DE, WordRegister::HL, WordRegister::AF];

pub static CC: [u8; 4] = [FLAG_ZERO, FLAG_ZERO, FLAG_CARRY, FLAG_CARRY];
