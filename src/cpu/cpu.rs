use cpu::instruction;
use super::super::interconnect::interconnect::Interconnect;

const FLAGS_ZERO_INDEX: u8 = 7;
const FLAGS_SUB_INDEX: u8 = 6;
const FLAGS_CARRY_BORROW_3: u8 = 5;
const FLAGS_CARRY_BORROW_7: u8 = 4;

pub struct Cpu<'a> {
    registers: [u8; 7],
    flags: u8,
    stack_pointer: u16,
    program_counter: u16,
    interconnect: &'a Interconnect
}

impl<'a> Cpu<'a> {
    pub fn new(interconnect: &Interconnect) -> Cpu {
        Cpu {
            registers: [0u8; 7],
            flags: 0u8,
            stack_pointer: 0u16,
            program_counter: 0u16,
            interconnect: interconnect
        }
    }

    pub fn execute_instruction(&mut self) {

    }

    #[inline]
    pub fn zero_flag(&self) -> bool {
        self.flags & (1u8 << FLAGS_ZERO_INDEX) == 0b1000_0000
    }

    #[inline]
    pub fn subtraction_flag(&self) -> bool {
        self.flags & (1u8 << FLAGS_SUB_INDEX) == 0b0100_0000
    }

    #[inline]
    pub fn half_carry(&self) -> bool {
        self.flags & (1u8 << FLAGS_CARRY_BORROW_3) == 0b0010_0000
    }

    #[inline]
    pub fn full_carry(&self) -> bool {
        self.flags & (1u8 << FLAGS_CARRY_BORROW_7) == 0b0001_0000
    }
}
