#[cfg(test)]
mod cpu_tests;

use memory::MemoryInterface;
use std::{cell::RefCell, rc::Rc};

/// Zero flag.
const F_ZERO: u8 = 0b1000_0000;
/// Subtraction flag.
const F_SUBTRACT: u8 = 0b0100_0000;
/// Half-carry flag.
const F_HALFCARRY: u8 = 0b0010_0000;
/// Carry flag.
const F_CARRY: u8 = 0b0001_0000;

/// Trait defining the interface to the CPU.
pub trait Cpu {
    type Error;

    /// Execute opcodes until stopped.
    fn execute(&mut self) -> Result<(), Self::Error>;
    /// Execute opcodes until greater than or equal to `cycle_bound`.
    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error>;
    /// Executes a single instruction, returning the number of cycles executed
    /// by the instruction.
    fn step(&mut self) -> Result<usize, Self::Error>;
}

type MemoryController = Rc<RefCell<MemoryInterface<Word = u8, Index = u16, Error = LRError>>>;
type SharpResult = Result<(), LRError>;
/// The original Sharp LR35902 processor, a 8080/Z80 derivative with some
/// interesting changes. Most notably, removal of the shadow register set along
/// with various opcode changes.
pub struct SharpLR35902 {
    /// CPU registers.
    registers: SharpLR35902Registers,
    /// Interrupt pending flag. (Do we need this?)
    interrupt_pending: bool,
    /// Whether the CPU is halted or not.
    halted: bool,
    /// `Rc` to the memory controller object.
    memory_controller: MemoryController,
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
}

impl ::std::fmt::Display for LRError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LRError::InvalidMemoryRead(addr) => format!("Invalid memory read at {:X}", addr),
                LRError::InvalidMemoryWrite(addr) => format!("Invalid memory write at {:X}", addr),
            }
        )
    }
}

impl ::std::error::Error for LRError {
    fn description(&self) -> &'static str {
        match self {
            LRError::InvalidMemoryRead(_) => "Invalid memory read",
            LRError::InvalidMemoryWrite(_) => "Invalid memory write",
        }
    }
}

impl Cpu for SharpLR35902 {
    type Error = LRError;
    fn execute(&mut self) -> Result<(), LRError> {
        unimplemented!()
    }

    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error> {
        unimplemented!()
    }

    fn step(&mut self) -> Result<usize, Self::Error> {
        unimplemented!()
    }
}

impl SharpLR35902 {
    /// Creates a new `SharpLR35902` from an `Rc<RefCell<MemoryInterface>>`.
    pub fn new(mi: MemoryController) -> SharpLR35902 {
        Self {
            registers: Default::default(),
            interrupt_pending: false,
            halted: false,
            memory_controller: mi,
        }
    }

    /// Reads a word at the program counter and increments.
    fn read_instruction_word(&mut self) -> Result<u8, LRError> {
        let pc = self.registers.pc;
        self.registers.pc += 1;

        Ok(self.memory_controller.borrow().read(pc)?)
    }

    /// Reads a dword at the program counter and increments twice.
    fn read_instruction_dword(&mut self) -> Result<u16, LRError> {
        let first_byte = self.read_instruction_word()?;
        let second_byte = self.read_instruction_word()?;

        Ok(((second_byte as u16) << 8) | first_byte as u16)
    }

    /// Reads a byte at the address `addr`.
    fn read(&mut self, addr: u16) -> Result<u8, LRError> {
        Ok(self.memory_controller.borrow().read(addr)?)
    }

    /// Reads a byte at the address pointed to by `HL`.
    fn read_hl(&mut self) -> Result<u8, LRError> {
        let hl = self.registers.as_dwords().hl;

        Ok(self.memory_controller.borrow().read(hl)?)
    }

    /// Writes a byte at the given address.
    fn write(&mut self, addr: u16, data: u8) -> Result<(), LRError> {
        self.memory_controller.borrow_mut().write(addr, data)?;

        Ok(())
    }

    /// Writes a byte to the address pointed to by `HL`.
    fn write_hl(&mut self, data: u8) -> Result<(), LRError> {
        let hl = self.registers.as_dwords().hl;

        self.memory_controller.borrow_mut().write(hl, data)?;
        Ok(())
    }
}

/// Groups of bits in an opcode.
///
/// Broken up into the following groups:
///```
/// Bits 6-7: x
///
/// Bits 3-5: y
///
///     Bits 4-5: p
///
///     Bit 3: q
///
/// Bits 0-2: z
/// ```
struct OpcodeBits {
    x: u8,
    y: u8,
    z: u8,
    p: u8,
    q: u8,
}

impl From<u8> for OpcodeBits {
    fn from(op: u8) -> OpcodeBits {
        let x = (op & 0b1100_0000) >> 6;
        let y = (op & 0b0011_1000) >> 3;
        let z = op & 0b0000_0111;
        let p = (y & 0b110) >> 1;
        let q = y & 0b001;

        OpcodeBits { x, y, z, p, q }
    }
}

/// Array of instructions for executing individual opcodes.
const INSTRUCTIONS: [fn(&mut SharpLR35902, u8) -> SharpResult; 2] = [nop, ld_r_r];

/// NOP instruction. Does nothing.
///
/// Flags affected: none
fn nop(_: &mut SharpLR35902, _: u8) -> SharpResult {
    Ok(())
}

/// Stops the processor.
///
/// Flags affected: none
fn stop(_: &mut SharpLR35902, _: u8) -> SharpResult {
    // No-Op for now
    Ok(())
}

// BEGIN 8-BIT TRANSFER INSTRUCTIONS

/// Loads the value of register `A` into the memory address pointed to by
/// register `BC`.
///
/// Flags affected: none
fn ld_at_bc_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().bc;
    let a = cpu.registers.a;

    cpu.write(addr, a)?;
    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `DE`.
///
/// Flags affected: none
fn ld_at_de_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().de;
    let a = cpu.registers.a;

    cpu.write(addr, a)?;
    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `HL` then increments `HL`.
///
/// Flags affected: none
fn ld_at_hli_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let a = cpu.registers.a;

    cpu.write_hl(a)?;
    cpu.registers.as_dwords().hl += 1;

    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `HL` then decrements `HL`.
///
/// Flags affected: none
fn ld_at_hld_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let a = cpu.registers.a;

    cpu.write_hl(a)?;
    cpu.registers.as_dwords().hl -= 1;

    Ok(())
}

/// Loads the value of the memory address pointed to by register `HL` into
/// register `A` into then increments `HL`.
///
/// Flags affected: none
fn ld_a_at_hli(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    cpu.registers.a = cpu.read_hl()?;
    cpu.registers.as_dwords().hl += 1;

    Ok(())
}

/// Loads the value of the memory address pointed to by register `HL` into
/// register `A` into then decrements `HL`.
///
/// Flags affected: none
fn ld_a_at_hld(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    cpu.registers.a = cpu.read_hl()?;
    cpu.registers.as_dwords().hl -= 1;

    Ok(())
}

/// Reads an immediate 8-bit value into the specified register.
///
/// Flags affected: none
fn ld_r_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);

    if bits.y != 6 {
        cpu.registers[bits.y] = cpu.read_instruction_word()?;
    } else {
        let data = cpu.read_instruction_word()?;
        cpu.write_hl(data)?;
    }

    Ok(())
}

/// Reads an 8-bit value pointed to by register `HL` into the specified
/// register.
///
/// Flags affected: none
fn ld_r_at_hl(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    cpu.registers[bits.y] = cpu.read_hl()?;

    Ok(())
}

/// Writes an 8-bit value in the specified register to the memory location
/// pointed to by register `HL`.
///
/// Flags affected: none
fn ld_at_hl_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    let val = cpu.registers[bits.z];

    cpu.write_hl(val)?;

    Ok(())
}

/// Writes an 8-bit value in the specified register to the memory location
/// pointed to by register `HL`.
///
/// Flags affected: none
fn ld_at_hl_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let val = cpu.read_instruction_word()?;
    cpu.write_hl(val)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `BC` into the `A`
/// register.
///
/// Flags affected: none
fn ld_a_at_bc(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().bc;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `DE` into the `A`
/// register.
///
/// Flags affected: none
fn ld_a_at_de(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().de;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `C` + 0xFF00 into the `A`
/// register.
///
/// Flags affected: none
fn ld_a_at_c(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.c as u16 + 0xFF00;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by register `C` + 0xFF00.
///
/// Flags affected: none
fn ld_at_c_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.c as u16 + 0xFF00;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by the immediate 8-bit operand `d8` + 0xFF00
/// into the `A` register.
///
/// Flags affected: none
fn ld_a_at_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_word()? as u16 + 0xFF00;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by the immediate 8-bit operand `d8` + 0xFF00.
///
/// Flags affected: none
fn ld_at_d8_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_word()? as u16 + 0xFF00;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by the immediate 816bit operand `d16`
/// into the `A` register.
///
/// Flags affected: none
fn ld_a_at_d16(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_dword()?;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by the immediate 16-bit operand `d16`
///
/// Flags affected: none
fn ld_at_d16_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_dword()?;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads the contents of one register into another.
///
/// Flags affected: none
fn ld_r_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    cpu.registers[bits.y] = cpu.registers[bits.z];

    Ok(())
}

// BEGIN 16-BIT TRANSFER INSTRUCTIONS

/// Loads a 16-bit immediate value into the register pair `rr`.
///
/// Flags affected: none
fn ld_rr_nn(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    let data = cpu.read_instruction_dword()?;

    if bits.p != 0x03 {
        cpu.registers.as_dwords()[bits.p] = data;
    } else {
        cpu.registers.sp = data;
    }

    Ok(())
}

/// Loads the contents of register `HL` into the stack pointer (`SP`).
///
/// Flags affected: none
fn ld_sp_hl(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let hl = cpu.registers.as_dwords().hl;

    cpu.registers.sp = hl;

    Ok(())
}

/// Pushes the value of a register pair `rr` onto the stack and subtracts the
/// stack pointer (`SP`) by 2.
///
/// Flags affected: none
fn push_rr(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);

    let d16 = cpu.registers.as_dwords()[bits.p];
    let sp = cpu.registers.sp;

    cpu.write(sp - 1, (d16 >> 8) as u8)?;
    cpu.write(sp - 2, (d16 & 0x00FF) as u8)?;
    cpu.registers.sp -= 2;

    Ok(())
}

/// Pops a 16-bit value off of the stack into a register pair `rr` and adds 2 to
/// the stack pointer (`SP`).
///
/// Flags affected: none
fn pop_rr(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    let sp = cpu.registers.sp;

    let d16 = cpu.read(sp)? as u16 | (cpu.read(sp + 1)? as u16) << 8;
    cpu.registers.sp += 2;

    cpu.registers.as_dwords()[bits.p] = d16;

    Ok(())
}

/// Adds an immediate 8-bit signed integer to the stack pointer (`SP`) and
/// places the result into register pair `HL`.
///
/// Flags affected: C - *, H - *, S - 0, Z - 0
fn ldhl_sp_e(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let e = cpu.read_instruction_word()? as i8 as i16;
    let sp = cpu.registers.sp as i16;

    let result = i16_add(sp, e);

    cpu.registers.as_dwords().hl = result.result as u16;

    if result.carry {
        cpu.registers.set_c();
    }

    if result.half_carry {
        cpu.registers.set_h();
    }

    cpu.registers.clear_s();
    cpu.registers.clear_z();

    Ok(())
}

/// Stores the value of the stack pointer `SP` into the immediate 16-bit
/// address.
///
/// Flags affected: none
fn ld_d16_sp(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let d16 = cpu.read_instruction_word()? as u16 | (cpu.read_instruction_word()? as u16) << 8;
    let sp = cpu.registers.sp;

    cpu.write(d16, (sp & 0x00FF) as u8)?;
    cpu.write(d16 + 1, (sp >> 8) as u8)?;

    Ok(())
}

// BEGIN 8-BIT ARITHMETIC AND LOGIC OPERATIONS

/// Adds the value in the register `r` to register `A` and stores the result in
/// register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
fn add_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);

    let (result, flags) = if bits.z != 0b110 {
        u8_add(cpu.registers.a, cpu.registers[bits.z]).into_parts()
    } else {
        let second = cpu.read_hl()?;
        u8_add(cpu.registers.a, second).into_parts()
    };

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Adds the value of the immediate 8-bit operand `d8` to register `A` and
/// stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
fn add_a_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let d8 = cpu.read_instruction_word()?;

    let (result, flags) = u8_add(cpu.registers.a, d8).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Adds the value in the register `r` to register `A` along with the value of
/// the carry flag then stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
fn adc_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    add_a_r(cpu, opcode)?;

    let (result, flags) = u8_add(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Adds the value of the immediate 8-bit operand to register `A` along with the
/// value of the carry flag then stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
fn adc_a_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    add_a_d8(cpu, opcode)?;

    let (result, flags) = u8_add(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Subtracts the value in the register `r` from register `A` and stores the
/// result in register `A`.
///
/// Flags affected: C - *, H - *, S - 1, Z - *
fn sub_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);

    let (result, flags) = if bits.z != 0b110 {
        u8_sub(cpu.registers.a, cpu.registers[bits.z]).into_parts()
    } else {
        let second = cpu.read_hl()?;
        u8_sub(cpu.registers.a, second).into_parts()
    };

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Subtracts the value of the immediate 8-bit operand `d8` from register `A`
/// and stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 1, Z - *
fn sub_a_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let d8 = cpu.read_instruction_word()?;

    let (result, flags) = u8_sub(cpu.registers.a, d8).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Subtracts the value in the register `r` from register `A` along with the
/// value of the carry flag then stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 1, Z - *
fn sbc_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    sub_a_r(cpu, opcode)?;

    let (result, flags) = u8_sub(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Subtracts the value of the immediate 8-bit operand `d8` from register `A`
/// along with the value of the carry flag then stores the result in register
/// `A`.
///
/// Flags affected: C - *, H - *, S - 1, Z - *
fn sbc_a_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    sub_a_d8(cpu, opcode)?;

    let (result, flags) = u8_sub(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

// BEGIN HELPER FUNCTIONS
/// Contains the result and flag changes of an ALU operation.
#[derive(Debug, Default, PartialEq)]
struct AluResult<T: Default> {
    /// Result of the operation.
    result: T,
    /// Whether the zero flag was set or not.
    zero: bool,
    /// Whether the carry flag was set or not.
    carry: bool,
    /// Whether the half-carry flag was set or not.
    half_carry: bool,
    /// Whether the subtract flag was set or not.
    subtract: bool,
}

impl<T: Default> AluResult<T> {
    /// Returns the result of the operation and the flags in `u8` form.
    fn into_parts(self) -> (T, u8) {
        let res = self.result;
        let flags = {
            let mut f = 0u8;

            if self.zero {
                f |= F_ZERO;
            }

            if self.half_carry {
                f |= F_HALFCARRY;
            }

            if self.carry {
                f |= F_CARRY;
            }

            if self.subtract {
                f | F_SUBTRACT;
            }

            f
        };

        (res, flags)
    }
}

/// Helper function.
///
/// Returns an `AluResult` of the addition of two `u8`s.
fn u8_add(first: u8, second: u8) -> AluResult<u8> {
    let mut chc = AluResult::default();

    let (res, cry) = first.overflowing_add(second);
    chc.result = res;
    chc.carry = cry;

    if chc.result == 0 {
        chc.zero = true;
    }

    if (first & 0x0F) + (second & 0x0F) > 0x0F {
        chc.half_carry = true;
    }

    chc
}

/// Helper function.
///
/// Returns an `AluResult` of the addition of two `u16`s.
fn u16_add(first: u16, second: u16) -> AluResult<u16> {
    let mut chc = AluResult::default();

    let (res, cry) = first.overflowing_add(second);
    chc.result = res;
    chc.carry = cry;

    if chc.result == 0 {
        chc.zero = true;
    }

    if (first & 0xFFF) + (second & 0xFFF) > 0xFFF {
        chc.half_carry = true;
    }

    chc
}

/// Helper function.
///
/// Returns an `AluResult` of the subtraction of two `u8`s.
fn u8_sub(first: u8, second: u8) -> AluResult<u8> {
    let mut chc = AluResult::default();

    let (res, cry) = first.overflowing_sub(second);
    chc.result = res;
    chc.carry = cry;

    if chc.result == 0 {
        chc.zero = true;
    }

    if (first & 0x0F) < (second & 0x0F) {
        chc.half_carry = true;
    }

    chc.subtract = true;

    chc
}

/// Helper function.
///
/// Returns an `AluResult` of the subtraction of two `u16`s.
fn u16_sub(first: u16, second: u16) -> AluResult<u16> {
    let mut chc = AluResult::default();

    let (res, cry) = first.overflowing_sub(second);
    chc.result = res;
    chc.carry = cry;

    if chc.result == 0 {
        chc.zero = true;
    }

    if (first & 0xFFF) < (second & 0xFFF) {
        chc.half_carry = true;
    }

    chc.subtract = true;

    chc
}

/// Helper function.
///
/// Returns an `AluResult` of the addition of two `i16`s.
fn i16_add(first: i16, second: i16) -> AluResult<i16> {
    let mut chc = AluResult::default();

    let (res, cry) = first.overflowing_add(second);
    chc.result = res;
    chc.carry = cry;

    if chc.result == 0 {
        chc.zero = true;
    }

    if (first & 0xFFF) + (second & 0xFFF) > 0xFFF {
        chc.half_carry = true;
    }

    chc.subtract = false;

    chc
}
