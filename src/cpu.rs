/// Zero flag.
const FLAG_Z: u8 = 0b1000_0000;
/// Subtraction flag.
const FLAG_S: u8 = 0b0100_0000;
/// Half-carry flag.
const FLAG_H: u8 = 0b0010_0000;
/// Carry flag.
const FLAG_C: u8 = 0b0001_0000;

/// Trait defining the interface to the CPU.
pub trait Cpu {
    type Error;

    /// Execute opcodes until stopped.
    fn execute(&mut self) -> Result<(), Self::Error>;
    /// Execute opcodes until greater than or equal to `cycle_bound`.
    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error>;
}

/// The original Sharp LR35902 processor, a 8080/Z80 derivative with some
/// interesting changes. Most notably, removal of the shadow register set along
/// with various opcode changes.
#[derive(Default)]
pub struct SharpLR35902 {
    registers: SharpLR35902Registers,
    interrupt_pending: bool,
    halted: bool,
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
    pub f: u8,
    pub a: u8,
    pub c: u8,
    pub b: u8,
    pub e: u8,
    pub d: u8,
    pub l: u8,
    pub h: u8,
    pub pc: u16,
    pub sp: u16,
}

#[cfg(target_endian = "big")]
#[derive(Debug, Default, Clone, Copy)]
#[repr(C, align(2))]
pub struct SharpLR35902Registers {
    pub a: u8,
    pub f: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub pc: u16,
    pub sp: u16,
}

impl SharpLR35902Registers {
    /// Temporarily transmutes `SharpLR35902Registers` into `DWordRegisters`
    /// which contain the 16-bit register pairs for convenience.
    fn as_dwords(&mut self) -> &mut DWordRegisters {
        unsafe { ::std::mem::transmute(self) }
    }

    /// Sets the zero flag.
    fn set_z(&mut self) {
        self.f |= FLAG_Z;
    }

    /// Sets the subtraction flag.
    fn set_s(&mut self) {
        self.f |= FLAG_S;
    }

    /// Sets the half-carry flag.
    fn set_h(&mut self) {
        self.f |= FLAG_H;
    }

    /// Sets the carry flag.
    fn set_c(&mut self) {
        self.f |= FLAG_C;
    }

    /// Clears the zero flag.
    fn clear_z(&mut self) {
        self.f &= !FLAG_Z;
    }

    /// Clears the subtraction flag.
    fn clear_s(&mut self) {
        self.f &= !FLAG_S;
    }

    /// Clears the half-carry flag.
    fn clear_h(&mut self) {
        self.f &= !FLAG_H;
    }

    /// Clears the carry flag.
    fn clear_c(&mut self) {
        self.f &= !FLAG_C;
    }

    /// Returns whether or not the zero flag is set.
    fn z(&self) -> bool {
        self.f & FLAG_Z == FLAG_Z
    }

    /// Returns whether or not the subtraction flag is set.
    fn s(&self) -> bool {
        self.f & FLAG_S == FLAG_S
    }

    /// Returns whether or not the half-carry flag is set.
    fn h(&self) -> bool {
        self.f & FLAG_H == FLAG_H
    }

    /// Returns whether or not the carry flag is set.
    fn c(&self) -> bool {
        self.f & FLAG_C == FLAG_C
    }
}

/// Representation of the 16-bit register pairs.
#[derive(Clone, Copy, Default, Debug)]
#[repr(C, align(2))]
struct DWordRegisters {
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub pc: u16,
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
pub enum LRError {
    InvalidMemoryRead(u16),
    InvalidMemoryWrite(u16),
}

impl Cpu for SharpLR35902 {
    type Error = LRError;

    fn execute(&mut self) -> Result<(), LRError> {
        Ok(())
    }

    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flags() {
        let mut cpu = SharpLR35902::default();

        cpu.registers.set_c();
        assert!(cpu.registers.c());

        cpu.registers.set_s();
        assert!(cpu.registers.s());

        cpu.registers.set_z();
        assert!(cpu.registers.z());

        cpu.registers.set_h();
        assert!(cpu.registers.h());

        cpu.registers.clear_c();
        assert!(!cpu.registers.c());

        cpu.registers.clear_s();
        assert!(!cpu.registers.s());

        cpu.registers.clear_z();
        assert!(!cpu.registers.z());

        cpu.registers.clear_h();
        assert!(!cpu.registers.h());
    }

    #[test]
    fn registers() {
        let mut cpu = SharpLR35902::default();

        cpu.registers.a = 0x11;
        cpu.registers.f = 0x22;
        cpu.registers.b = 0x33;
        cpu.registers.c = 0x44;
        cpu.registers.d = 0x55;
        cpu.registers.e = 0x66;
        cpu.registers.h = 0x77;
        cpu.registers.l = 0x88;

        assert_eq!(cpu.registers.as_dwords().af, 0x1122);
        assert_eq!(cpu.registers.as_dwords().bc, 0x3344);
        assert_eq!(cpu.registers.as_dwords().de, 0x5566);
        assert_eq!(cpu.registers.as_dwords().hl, 0x7788);
    }
}
