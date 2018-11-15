#[cfg(test)]
mod cpu_tests;
mod instructions;

use memory::MemoryInterface;
use std::{cell::RefCell, rc::Rc};

/// Zero flag.
pub const F_ZERO: u8 = 0b1000_0000;
/// Subtraction flag.
pub const F_SUBTRACT: u8 = 0b0100_0000;
/// Half-carry flag.
pub const F_HALFCARRY: u8 = 0b0010_0000;
/// Carry flag.
pub const F_CARRY: u8 = 0b0001_0000;

type MemoryController<'a> = &'a mut dyn MemoryInterface<Word = u8, Index = u16, Error = LRError>;
type SharpResult = Result<(), LRError>;
/// The original Sharp LR35902 processor, a 8080/Z80 derivative with some
/// interesting changes. Most notably, removal of the shadow register set along
/// with various opcode changes.
pub struct SharpLR35902<'a> {
    /// CPU registers.
    registers: SharpLR35902Registers,
    /// Interrupt pending flag. (Do we need this?)
    interrupt_pending: bool,
    /// Whether the CPU is halted or not.
    halted: bool,
    /// Current instruction opcode.
    current_opcode: u8,
    /// `Rc` to the memory controller object.
    memory_controller: MemoryController<'a>,
}

/// The Sharp LR35902 register set. Contains 7 8-bit general purpose registers
/// (`a`, `b`, `c`, `d`, `e`, `h`, `l`), a flag register, program counter, and
/// stack pointer. Each of the 7 general purpose registers plus the flags
/// register can be combined into 4 different 16-bit register pairs: `af`, `bc`,
/// `de`, and `hl`. The first register name denotes the top 8 bits, with the
/// second denoting the bottom 8 bits.
#[cfg(target_endian = "little")]
#[derive(Debug, Default, Clone, Copy)]
#[repr(C, align(2))]
pub struct SharpLR35902Registers {
    /// Flags register.
    pub f: u8,
    /// General purpose `A` register.
    pub a: u8,
    /// General purpose `C` register.
    pub c: u8,
    /// General purpose `B` register.
    pub b: u8,
    /// General purpose `E` register.
    pub e: u8,
    /// General purpose `D` register.
    pub d: u8,
    /// General purpose `L` register.
    pub l: u8,
    /// General purpose `H` register.
    pub h: u8,
    /// Program counter.
    pub pc: u16,
    /// Stack pointer.
    pub sp: u16,
}

#[cfg(target_endian = "big")]
#[derive(Debug, Default, Clone, Copy)]
#[repr(C, align(2))]
pub struct SharpLR35902Registers {
    /// General purpose `A` register.
    pub a: u8,
    /// Flags register.
    pub f: u8,
    /// General purpose `B` register.
    pub b: u8,
    /// General purpose `C` register.
    pub c: u8,
    /// General purpose `D` register.
    pub d: u8,
    /// General purpose `D` register.
    pub e: u8,
    /// General purpose `H` register.
    pub h: u8,
    /// General purpose `L` register.
    pub l: u8,
    /// Program counter.
    pub pc: u16,
    /// Stack pointer.
    pub sp: u16,
}

impl SharpLR35902Registers {
    /// Temporarily transmutes `SharpLR35902Registers` into `DWordRegisters`
    /// which contain the 16-bit register pairs for convenience.
    fn as_dwords(&mut self) -> &mut DWordRegisters {
        unsafe { &mut *(self as *mut SharpLR35902Registers as *mut DWordRegisters) }
    }

    /// Sets the zero flag.
    fn set_z(&mut self) {
        self.f |= F_ZERO;
    }

    /// Sets the subtraction flag.
    fn set_s(&mut self) {
        self.f |= F_SUBTRACT;
    }

    /// Sets the half-carry flag.
    fn set_h(&mut self) {
        self.f |= F_HALFCARRY;
    }

    /// Sets the carry flag.
    fn set_c(&mut self) {
        self.f |= F_CARRY;
    }

    /// Clears the zero flag.
    fn clear_z(&mut self) {
        self.f &= !F_ZERO;
    }

    /// Clears the subtraction flag.
    fn clear_s(&mut self) {
        self.f &= !F_SUBTRACT;
    }

    /// Clears the half-carry flag.
    fn clear_h(&mut self) {
        self.f &= !F_HALFCARRY;
    }

    /// Clears the carry flag.
    fn clear_c(&mut self) {
        self.f &= !F_CARRY;
    }

    /// Returns whether or not the zero flag is set.
    fn z(&self) -> bool {
        self.f & F_ZERO == F_ZERO
    }

    /// Returns whether or not the subtraction flag is set.
    fn s(&self) -> bool {
        self.f & F_SUBTRACT == F_SUBTRACT
    }

    /// Returns whether or not the half-carry flag is set.
    fn h(&self) -> bool {
        self.f & F_HALFCARRY == F_HALFCARRY
    }

    /// Returns whether or not the carry flag is set.
    fn c(&self) -> bool {
        self.f & F_CARRY == F_CARRY
    }
}

/// Representation of the 16-bit register pairs.
#[derive(Clone, Copy, Default, Debug)]
#[repr(C, align(2))]
struct DWordRegisters {
    /// Register pair `AF`.
    pub af: u16,
    /// Register pair `BC`.
    pub bc: u16,
    /// Register pair `DE`.
    pub de: u16,
    /// Register pair `HL`.
    pub hl: u16,
    /// Program counter.
    pub pc: u16,
    /// Stack pointer.
    pub sp: u16,
}

impl ::std::ops::Index<u8> for SharpLR35902Registers {
    type Output = u8;

    fn index(&self, index: u8) -> &u8 {
        match index {
            0 => &self.b,
            1 => &self.c,
            2 => &self.d,
            3 => &self.e,
            4 => &self.h,
            5 => &self.l,
            6 => &self.f,
            7 => &self.a,
            _ => panic!("Register index out of bounds."),
        }
    }
}

impl ::std::ops::IndexMut<u8> for SharpLR35902Registers {
    fn index_mut(&mut self, index: u8) -> &mut u8 {
        match index {
            0 => &mut self.b,
            1 => &mut self.c,
            2 => &mut self.d,
            3 => &mut self.e,
            4 => &mut self.h,
            5 => &mut self.l,
            6 => &mut self.f,
            7 => &mut self.a,
            _ => panic!("Register index out of bounds."),
        }
    }
}

impl ::std::ops::Index<u8> for DWordRegisters {
    type Output = u16;

    fn index(&self, index: u8) -> &u16 {
        match index {
            0 => &self.bc,
            1 => &self.de,
            2 => &self.hl,
            3 => &self.af,
            _ => panic!("DWord register index out of bounds."),
        }
    }
}

impl ::std::ops::IndexMut<u8> for DWordRegisters {
    fn index_mut(&mut self, index: u8) -> &mut u16 {
        match index {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 => &mut self.af,
            _ => panic!("DWord register index out of bounds."),
        }
    }
}

/// CPU errors.
#[derive(Debug)]
pub enum LRError {
    InvalidMemoryRead(u16),
    InvalidMemoryWrite(u16),
    RamDisabled(u16),
    InvalidBankRead(u16, usize),
    InvalidBankWrite(u16, usize),
}

impl ::std::fmt::Display for LRError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LRError::InvalidMemoryRead(addr) => format!("Invalid memory read at {:#X}", addr),
                LRError::InvalidMemoryWrite(addr) => format!("Invalid memory write at {:#X}", addr),
                LRError::RamDisabled(addr) => {
                    format!("Attempted read/write at {:#X} while RAM disabled", addr)
                }
                LRError::InvalidBankRead(addr, bankno) => {
                    format!("Invalid memory read at {:#X} bank no. {}", addr, bankno)
                }
                LRError::InvalidBankWrite(addr, bankno) => {
                    format!("Invalid memory write at {:#X} bank no. {}", addr, bankno)
                }
            }
        )
    }
}

impl ::std::error::Error for LRError {
    fn description(&self) -> &'static str {
        match self {
            LRError::InvalidMemoryRead(_) => "Invalid memory read",
            LRError::InvalidMemoryWrite(_) => "Invalid memory write",
            LRError::RamDisabled(_) => "Attempted read/write while RAM is disabled",
            LRError::InvalidBankRead(_, _) => "Invalid bank memory read",
            LRError::InvalidBankWrite(_, _) => "Invalid bank memory write",
        }
    }
}

impl<'a> SharpLR35902<'a> {
    /// Creates a new `SharpLR35902` from an `Rc<RefCell<MemoryInterface>>`.
    pub fn new(mi: MemoryController<'a>) -> SharpLR35902<'a> {
        Self {
            registers: Default::default(),
            interrupt_pending: false,
            halted: false,
            current_opcode: 0x76,
            memory_controller: mi,
        }
    }

    /// Reads a word at the program counter and increments.
    fn read_instruction_word(&mut self) -> Result<u8, LRError> {
        let pc = self.registers.pc;
        self.registers.pc += 1;

        Ok(self.memory_controller.read(pc)?)
    }

    /// Reads a dword at the program counter and increments twice.
    fn read_instruction_dword(&mut self) -> Result<u16, LRError> {
        let first_byte = self.read_instruction_word()?;
        let second_byte = self.read_instruction_word()?;

        Ok(((second_byte as u16) << 8) | first_byte as u16)
    }

    /// Reads a byte at the address `addr`.
    fn read(&mut self, addr: u16) -> Result<u8, LRError> {
        Ok(self.memory_controller.read(addr)?)
    }

    /// Reads a byte at the address pointed to by `HL`.
    fn read_hl(&mut self) -> Result<u8, LRError> {
        let hl = self.registers.as_dwords().hl;

        Ok(self.memory_controller.read(hl)?)
    }

    /// Writes a byte at the given address.
    fn write(&mut self, addr: u16, data: u8) -> Result<(), LRError> {
        self.memory_controller.write(addr, data)?;

        Ok(())
    }

    /// Writes a byte to the address pointed to by `HL`.
    fn write_hl(&mut self, data: u8) -> Result<(), LRError> {
        let hl = self.registers.as_dwords().hl;

        self.memory_controller.write(hl, data)?;
        Ok(())
    }
}
