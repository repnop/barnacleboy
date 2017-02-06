// So the Gameboy's opcodes are very spread out in some cases...
// Decided to decode them into their types to decode them
// Find opcode table here: http://pastraiser.com/cpu/gameboy/gameboy_opcodes.html

pub struct Instruction(pub u8);

pub enum Operation {
    Adc,
    Add16,
    Add8,
    And,
    Bit,
    Call,
    Ccf,
    Cp,
    Cpl,
    Daa,
    Dec16,
    Dec8,
    Di,
    Ei,
    Halt,
    Inc16,
    Inc8,
    Jp,
    Jr,
    Ld16,
    Ld8,
    Ldh,
    Nop,
    Or,
    Pop,
    Push,
    Res,
    Ret,
    Reti,
    Rl,
    Rla,
    Rlc,
    Rlca,
    Rr,
    Rra,
    Rrc,
    Rrca,
    Rst,
    Sbc,
    Scf,
    Set,
    Sla,
    Sra,
    Srl,
    Stop,
    Sub,
    Swap,
    Xor,
}

impl Instruction {
    #[inline]
    pub fn is_prefixed(&self) -> bool {
        self.0 == 0xCB
    }

    pub fn get_opcode_type_nonprefix(&self) -> Operation {
        match self.0 {
            0x0 | 0x40 | 0x52 | 0x49 | 0x5B | 0x6D | 0x7F | 0x64 => Operation::Nop,

            0x10 => Operation::Stop,

            0x76 => Operation::Halt,

            0x07 => Operation::Rlca,
            0x17 => Operation::Rla,

            0x27 => Operation::Daa,
            0x37 => Operation::Scf,

            0x0F => Operation::Rrca,
            0x1F => Operation::Rra,

            0x2F => Operation::Cpl,
            0x3F => Operation::Ccf,

            0x20 | 0x30 | 0x18 | 0x28 | 0x38 => Operation::Jr,

            0xC2 | 0xD2 | 0xC3 | 0xE9 | 0xCA | 0xDA => Operation::Jp,

            0xC0 | 0xD0 | 0xC8 | 0xD8 | 0xC9 => Operation::Ret,

            0xD9 => Operation::Reti,

            0xC4 | 0xD4 | 0xCC | 0xDC | 0xCD => Operation::Call,

            //TODO: find a way to put on multiple lines
            0x41...0x4F | 0x50...0x5F | 0x60...0x6F | 0x70...0x7F | 0x06 | 0x16 | 0x26 | 0x36 |
            0x0A | 0x1A | 0x2A | 0x3A | 0x0E | 0x1E | 0x2E | 0x3E | 0xE2 | 0xF2 | 0xEA | 0xFA => {
                Operation::Ld8
            } //Match will short-circuit the values that are in this range that go to Nop

            0x01 | 0x11 | 0x21 | 0x31 | 0x08 | 0xF9 | 0xF8 => Operation::Ld16,

            0xE0 | 0xF0 => Operation::Ldh,

            0x03 | 0x13 | 0x23 | 0x33 => Operation::Inc16,

            0x04 | 0x14 | 0x24 | 0x34 | 0x0C | 0x1C | 0x2C | 0x3C => Operation::Inc8,

            0x09 | 0x19 | 0x29 | 0x39 | 0xE8 => Operation::Add16,

            0x05 | 0x15 | 0x25 | 0x35 | 0x0D | 0x1D | 0x2D | 0x3D => Operation::Dec8,

            0x0B | 0x1B | 0x2B | 0x3B => Operation::Dec16,

            0x80...0x87 | 0xC6 => Operation::Add8,
            0x88...0x8F | 0xCE => Operation::Adc,
            0x90...0x97 | 0xD6 => Operation::Sub,
            0x98...0x9F | 0xDE => Operation::Sbc,
            0xA0...0xA7 | 0xE6 => Operation::And,
            0xA8...0xAF | 0xEE => Operation::Xor,
            0xB0...0xB7 | 0xF6 => Operation::Or,
            0xB8...0xBF | 0xFE => Operation::Cp,

            0xC1 | 0xD1 | 0xE1 | 0xF1 => Operation::Pop,
            0xC5 | 0xD5 | 0xE5 | 0xF5 => Operation::Push,

            0xC7 | 0xD7 | 0xE7 | 0xF7 | 0xCF | 0xDF | 0xEF | 0xFF => Operation::Rst,

            0xF3 => Operation::Di,
            0xFB => Operation::Ei,

            _ => panic!("Unrecognized instruction: {:#x}", self.0),
        }
    }

    pub fn get_opcode_type_prefixed(&self) -> Operation {
        match self.0 {

            0x00...0x07 => Operation::Rlc,
            0x08...0x0F => Operation::Rrc,

            0x10...0x17 => Operation::Rl,
            0x18...0x1F => Operation::Rr,

            0x20...0x27 => Operation::Sla,
            0x28...0x2F => Operation::Sra,

            0x30...0x37 => Operation::Swap,
            0x38...0x3F => Operation::Srl,

            0x40...0x7F => Operation::Bit,

            0x80...0xBF => Operation::Res,

            0xC0...0xFF => Operation::Set,

            _ => unreachable!(),
        }
    }
}
