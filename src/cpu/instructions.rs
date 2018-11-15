use crate::cpu::{LRError, SharpLR35902, SharpResult, F_CARRY, F_HALFCARRY, F_SUBTRACT, F_ZERO};

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
pub struct OpcodeBits {
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
pub const INSTRUCTIONS: [fn(&mut SharpLR35902, u8) -> SharpResult; 2] = [nop, ld_r_r];

/// NOP instruction. Does nothing.
///
/// Flags affected: none
pub fn nop(_: &mut SharpLR35902, _: u8) -> SharpResult {
    Ok(())
}

/// Stops the processor.
///
/// Flags affected: none
pub fn stop(_: &mut SharpLR35902, _: u8) -> SharpResult {
    // No-Op for now
    Ok(())
}

// BEGIN 8-BIT TRANSFER INSTRUCTIONS

/// Loads the value of register `A` into the memory address pointed to by
/// register `BC`.
///
/// Flags affected: none
pub fn ld_at_bc_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().bc;
    let a = cpu.registers.a;

    cpu.write(addr, a)?;
    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `DE`.
///
/// Flags affected: none
pub fn ld_at_de_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().de;
    let a = cpu.registers.a;

    cpu.write(addr, a)?;
    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `HL` then increments `HL`.
///
/// Flags affected: none
pub fn ld_at_hli_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let a = cpu.registers.a;

    cpu.write_hl(a)?;
    cpu.registers.as_dwords().hl += 1;

    Ok(())
}

/// Loads the value of register `A` into the memory address pointed to by
/// register `HL` then decrements `HL`.
///
/// Flags affected: none
pub fn ld_at_hld_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let a = cpu.registers.a;

    cpu.write_hl(a)?;
    cpu.registers.as_dwords().hl -= 1;

    Ok(())
}

/// Loads the value of the memory address pointed to by register `HL` into
/// register `A` into then increments `HL`.
///
/// Flags affected: none
pub fn ld_a_at_hli(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    cpu.registers.a = cpu.read_hl()?;
    cpu.registers.as_dwords().hl += 1;

    Ok(())
}

/// Loads the value of the memory address pointed to by register `HL` into
/// register `A` into then decrements `HL`.
///
/// Flags affected: none
pub fn ld_a_at_hld(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    cpu.registers.a = cpu.read_hl()?;
    cpu.registers.as_dwords().hl -= 1;

    Ok(())
}

/// Reads an immediate 8-bit value into the specified register.
///
/// Flags affected: none
pub fn ld_r_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn ld_r_at_hl(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    cpu.registers[bits.y] = cpu.read_hl()?;

    Ok(())
}

/// Writes an 8-bit value in the specified register to the memory location
/// pointed to by register `HL`.
///
/// Flags affected: none
pub fn ld_at_hl_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    let val = cpu.registers[bits.z];

    cpu.write_hl(val)?;

    Ok(())
}

/// Writes an 8-bit value in the specified register to the memory location
/// pointed to by register `HL`.
///
/// Flags affected: none
pub fn ld_at_hl_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let val = cpu.read_instruction_word()?;
    cpu.write_hl(val)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `BC` into the `A`
/// register.
///
/// Flags affected: none
pub fn ld_a_at_bc(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().bc;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `DE` into the `A`
/// register.
///
/// Flags affected: none
pub fn ld_a_at_de(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.as_dwords().de;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by register `C` + 0xFF00 into the `A`
/// register.
///
/// Flags affected: none
pub fn ld_a_at_c(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.c as u16 + 0xFF00;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by register `C` + 0xFF00.
///
/// Flags affected: none
pub fn ld_at_c_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.registers.c as u16 + 0xFF00;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by the immediate 8-bit operand `d8` + 0xFF00
/// into the `A` register.
///
/// Flags affected: none
pub fn ld_a_at_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_word()? as u16 + 0xFF00;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by the immediate 8-bit operand `d8` + 0xFF00.
///
/// Flags affected: none
pub fn ld_at_d8_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_word()? as u16 + 0xFF00;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads an 8-bit value pointed to by the immediate 816bit operand `d16`
/// into the `A` register.
///
/// Flags affected: none
pub fn ld_a_at_d16(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_dword()?;
    cpu.registers.a = cpu.read(addr)?;

    Ok(())
}

/// Writes the 8-bit contents of the `A` register to the memory location pointed
/// to by the immediate 16-bit operand `d16`
///
/// Flags affected: none
pub fn ld_at_d16_a(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let addr = cpu.read_instruction_dword()?;
    let a = cpu.registers.a;
    cpu.write(addr, a)?;

    Ok(())
}

/// Reads the contents of one register into another.
///
/// Flags affected: none
pub fn ld_r_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
    let bits = OpcodeBits::from(opcode);
    cpu.registers[bits.y] = cpu.registers[bits.z];

    Ok(())
}

// BEGIN 16-BIT TRANSFER INSTRUCTIONS

/// Loads a 16-bit immediate value into the register pair `rr`.
///
/// Flags affected: none
pub fn ld_rr_nn(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn ld_sp_hl(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
    let hl = cpu.registers.as_dwords().hl;

    cpu.registers.sp = hl;

    Ok(())
}

/// Pushes the value of a register pair `rr` onto the stack and subtracts the
/// stack pointer (`SP`) by 2.
///
/// Flags affected: none
pub fn push_rr(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn pop_rr(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn ldhl_sp_e(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
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
pub fn ld_d16_sp(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
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
pub fn add_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn add_a_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
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
pub fn adc_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn adc_a_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn sub_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn sub_a_d8(cpu: &mut SharpLR35902, _: u8) -> SharpResult {
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
pub fn sbc_a_r(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub fn sbc_a_d8(cpu: &mut SharpLR35902, opcode: u8) -> SharpResult {
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
pub struct AluResult<T: Default> {
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
    pub fn into_parts(self) -> (T, u8) {
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
pub fn u8_add(first: u8, second: u8) -> AluResult<u8> {
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
pub fn u16_add(first: u16, second: u16) -> AluResult<u16> {
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
pub fn u8_sub(first: u8, second: u8) -> AluResult<u8> {
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
pub fn u16_sub(first: u16, second: u16) -> AluResult<u16> {
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
pub fn i16_add(first: i16, second: i16) -> AluResult<i16> {
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
