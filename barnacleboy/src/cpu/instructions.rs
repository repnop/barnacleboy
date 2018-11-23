use crate::cpu::{SharpLR35902, SharpResult, F_CARRY, F_HALFCARRY, F_SUBTRACT, F_ZERO};
use barnacleboy_macros::ins_test;

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
    pub x: u8,
    pub y: u8,
    pub z: u8,
    pub p: u8,
    pub q: u8,
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
pub const INSTRUCTIONS: [fn(&mut SharpLR35902) -> SharpResult; 2] = [nop, ld];

/// NOP instruction. Does nothing.
///
/// Flags affected: none
pub fn nop(_: &mut SharpLR35902) -> SharpResult {
    Ok(())
}

/// Stops the processor.
///
/// Flags affected: none
pub fn stop(_: &mut SharpLR35902) -> SharpResult {
    // No-Op for now
    Ok(())
}

// BEGIN 8-BIT TRANSFER INSTRUCTIONS

pub fn ld(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

    match (bits.x, bits.y, bits.z) {
        // LD r, r'
        (0b01, r @ 0b000..=0b101, rp @ 0b000..=0b101) => {
            let rp = cpu.registers[rp];
            cpu.registers[r] = rp;
        }
        // LD r, d8
        (0b00, r @ 0b000..=0b101, 0b110) => {
            let d8 = cpu.read_instruction_word()?;
            cpu.registers[r] = d8;
        }
        // LD r, (HL)
        (0b01, r @ 0b000..=0b101, 0b110) => {
            let val = cpu.read_hl()?;
            cpu.registers[r] = val;
        }
        // LD (HL), r
        (0b01, 0b110, r) => {
            let reg = cpu.registers[r];
            cpu.write_hl(reg)?;
        }
        // LD (HL), d8
        (0b00, 0b110, 0b110) => {
            let d8 = cpu.read_instruction_word()?;
            cpu.write_hl(d8)?;
        }
        // LD A, (BC)
        (0b00, 0b001, 0b010) => {
            let bc = cpu.registers.as_dwords().bc;
            let val = cpu.read(bc)?;
            cpu.a = val;
        }
        // LD A, (DE)
        (0b00, 0b011, 0b010) => {
            let de = cpu.registers.as_dwords().de;
            let val = cpu.read(de)?;
            cpu.a = val;
        }
        // LD A, (C+0xFF00)
        (0b11, 0b110, 0b010) => {
            let c = cpu.c as u16 + 0xFF00;
            let val = cpu.read(c)?;
            cpu.a = val;
        }
        // LD (C+0xFF00), A
        (0b11, 0b100, 0b010) => {
            let c = cpu.c as u16 + 0xFF00;
            let a = cpu.a;
            cpu.write(c, a)?;
        }
        // LD A, (d8+0xFF00)
        (0b11, 0b110, 0b000) => {
            let d8 = cpu.read_instruction_word()? as u16 + 0xFF00;
            cpu.a = cpu.read(d8)?;
        }
        // LD (d8+0xFF00), A
        (0b11, 0b100, 0b000) => {
            let d8 = cpu.read_instruction_word()? as u16 + 0xFF00;
            let a = cpu.a;
            cpu.write(d8, a)?;
        }
        // LD A, (d16)
        (0b11, 0b111, 0b010) => {
            let d8 = cpu.read_instruction_dword()?;
            cpu.a = cpu.read(d8)?;
        }
        // LD (d16), A
        (0b11, 0b101, 0b010) => {
            let d8 = cpu.read_instruction_dword()?;
            let a = cpu.a;
            cpu.write(d8, a)?;
        }
        // LD A, (HLI)
        (0b00, 0b101, 0b010) => {
            cpu.a = cpu.read_hl()?;
            cpu.as_dwords().hl += 1;
        }
        // LD A, (HLD)
        (0b00, 0b111, 0b010) => {
            cpu.a = cpu.read_hl()?;
            cpu.as_dwords().hl -= 1;
        }
        // LD (BC), A
        (0b00, 0b000, 0b010) => {
            let bc = cpu.as_dwords().bc;
            let a = cpu.a;
            cpu.write(bc, a)?;
        }
        // LD (DE), A
        (0b00, 0b010, 0b010) => {
            let de = cpu.as_dwords().de;
            let a = cpu.a;
            cpu.write(de, a)?;
        }
        // LD (HLI), A
        (0b00, 0b100, 0b010) => {
            let a = cpu.a;
            cpu.write_hl(a)?;
            cpu.as_dwords().hl += 1;
        }
        // LD (HLD), A
        (0b00, 0b110, 0b010) => {
            let a = cpu.a;
            cpu.write_hl(a)?;
            cpu.as_dwords().hl -= 1;
        }
        // LD rr, d16
        (0b00, r @ 0b000, 0b001)
        | (0b00, r @ 0b010, 0b001)
        | (0b00, r @ 0b100, 0b001)
        | (0b00, r @ 0b110, 0b001) => {
            let d16 = cpu.read_instruction_dword()?;
            cpu.as_dwords()[r] = d16;
        }
        // LD SP, HL
        (0b11, 0b111, 0b001) => {
            cpu.sp = cpu.as_dwords().hl;
        }
        // LD (d16), SP
        (0b00, 0b001, 0b000) => {
            let sp = cpu.sp;
            let d16 = cpu.read_instruction_dword()?;
            cpu.write(d16, (sp & 0x00FF) as u8)?;
            cpu.write(d16 + 1, ((sp & 0xFF00) as u8) >> 8)?;
        }
        _ => unreachable!(),
    }

    Ok(())
}

// BEGIN 16-BIT TRANSFER INSTRUCTIONS

/// Loads a 16-bit immediate value into the register pair `rr`.
///
/// Flags affected: none
pub fn ld_rr_nn(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);
    let data = cpu.read_instruction_dword()?;

    if bits.p != 0x03 {
        cpu.registers.as_dwords()[bits.p] = data;
    } else {
        cpu.registers.sp = data;
    }

    Ok(())
}

/// Pushes the value of a register pair `rr` onto the stack and subtracts the
/// stack pointer (`SP`) by 2.
///
/// Flags affected: none
pub fn push_rr(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

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
pub fn pop_rr(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);
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
pub fn ldhl_sp_e(cpu: &mut SharpLR35902) -> SharpResult {
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

// BEGIN 8-BIT ARITHMETIC AND LOGIC OPERATIONS

/// Adds the value in the register `r` to register `A` and stores the result in
/// register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
pub fn add_a_r(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

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
pub fn add_a_d8(cpu: &mut SharpLR35902) -> SharpResult {
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
pub fn adc_a_r(cpu: &mut SharpLR35902) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    add_a_r(cpu)?;

    let (result, flags) = u8_add(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Adds the value of the immediate 8-bit operand to register `A` along with the
/// value of the carry flag then stores the result in register `A`.
///
/// Flags affected: C - *, H - *, S - 0, Z - *
pub fn adc_a_d8(cpu: &mut SharpLR35902) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    add_a_d8(cpu)?;

    let (result, flags) = u8_add(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Subtracts the value in the register `r` from register `A` and stores the
/// result in register `A`.
///
/// Flags affected: C - *, H - *, S - 1, Z - *
pub fn sub_a_r(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

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
pub fn sub_a_d8(cpu: &mut SharpLR35902) -> SharpResult {
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
pub fn sbc_a_r(cpu: &mut SharpLR35902) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    sub_a_r(cpu)?;

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
pub fn sbc_a_d8(cpu: &mut SharpLR35902) -> SharpResult {
    let c = if cpu.registers.f & F_CARRY == F_CARRY {
        1
    } else {
        0
    };

    sub_a_d8(cpu)?;

    let (result, flags) = u8_sub(cpu.registers.a, c).into_parts();

    cpu.registers.a = result;
    cpu.registers.f = flags;

    Ok(())
}

/// Performs a logical AND operation of the immediate 8-bit operand `d8` from
/// register `A`, with a register `r`, or the value of the byte at (HL).
///
/// Flags affected: C - 0, H - 1, S - 0, Z - *
#[ins_test("and_d8", |cpu| {
    cpu.memory_controller.write(0x00, 0xFF).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b11_100_110;
}, |cpu| {
    cpu.a == 0x0F
})]
#[ins_test("and_r", |cpu| {
    cpu.b = 0xFF;
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_100_000;
}, |cpu| {
    cpu.a == 0x0F
})]
#[ins_test("and_hl", |cpu| {
    cpu.write_hl(0xFF).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_100_110;
}, |cpu| {
    cpu.a == 0x0F
})]
pub fn and(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

    match (bits.x & 1 == 1, bits.z == 0b110) {
        // AND d8
        (true, true) => {
            let d8 = cpu.read_instruction_word()?;
            cpu.registers.a &= d8;
        }
        // AND r
        (false, false) => {
            let reg = cpu.registers[bits.z];
            cpu.registers.a &= reg;
        }
        // AND (HL)
        (false, true) => {
            let val = cpu.read_hl()?;
            cpu.registers.a &= val;
        }
        _ => unreachable!(),
    }

    cpu.clear_c();
    cpu.set_h();
    cpu.clear_s();

    if cpu.registers.a == 0 {
        cpu.set_z();
    } else {
        cpu.clear_z();
    }

    Ok(())
}

/// Performs a logical OR operation of the immediate 8-bit operand `d8` from
/// register `A`, with a register `r`, or the value of the byte at (HL).
///
/// Flags affected: C - 0, H - 0, S - 0, Z - *
#[ins_test("or_d8", |cpu| {
    cpu.memory_controller.write(0x00, 0xF0).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b11_110_110;
}, |cpu| {
    cpu.a == 0xFF
})]
#[ins_test("or_r", |cpu| {
    cpu.b = 0xF0;
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_110_000;
}, |cpu| {
    cpu.a == 0xFF
})]
#[ins_test("or_hl", |cpu| {
    cpu.write_hl(0xF0).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_110_110;
}, |cpu| {
    cpu.a == 0xFF
})]
pub fn or(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

    match (bits.x & 1 == 1, bits.z == 0b110) {
        // OR d8
        (true, true) => {
            let d8 = cpu.read_instruction_word()?;
            cpu.registers.a |= d8;
        }
        // OR r
        (false, false) => {
            let reg = cpu.registers[bits.z];
            cpu.registers.a |= reg;
        }
        // OR (HL)
        (false, true) => {
            let val = cpu.read_hl()?;
            cpu.registers.a |= val;
        }
        _ => unreachable!(),
    }

    cpu.clear_c();
    cpu.clear_h();
    cpu.clear_s();

    if cpu.registers.a == 0 {
        cpu.set_z();
    } else {
        cpu.clear_z();
    }

    Ok(())
}

/// Performs a logical XOR operation of the immediate 8-bit operand `d8` from
/// register `A`, with a register `r`, or the value of the byte at (HL).
///
/// Flags affected: C - 0, H - 0, S - 0, Z - *
#[ins_test("xor_d8", |cpu| {
    cpu.memory_controller.write(0x00, 0x0F).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b11_101_110;
}, |cpu| {
    cpu.a == 0x00
})]
#[ins_test("xor_r", |cpu| {
    cpu.b = 0x0F;
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_101_000;
}, |cpu| {
    cpu.a == 0x00
})]
#[ins_test("xor_hl", |cpu| {
    cpu.write_hl(0x0F).unwrap();
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_101_110;
}, |cpu| {
    cpu.a == 0x00
})]
pub fn xor(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

    match (bits.x & 1 == 1, bits.z == 0b110) {
        // OR d8
        (true, true) => {
            let d8 = cpu.read_instruction_word()?;
            cpu.registers.a ^= d8;
        }
        // OR r
        (false, false) => {
            let reg = cpu.registers[bits.z];
            cpu.registers.a ^= reg;
        }
        // OR (HL)
        (false, true) => {
            let val = cpu.read_hl()?;
            cpu.registers.a ^= val;
        }
        _ => unreachable!(),
    }

    cpu.clear_c();
    cpu.clear_h();
    cpu.clear_s();

    if cpu.registers.a == 0 {
        cpu.set_z();
    } else {
        cpu.clear_z();
    }

    Ok(())
}

/// Performs a comparison operation of the immediate 8-bit operand `d8` from
/// register `A`, with a register `r`, or the value of the byte at (HL).
///
/// Flags affected: C - *, H - *, S - 1, Z - *
#[ins_test("cp_d8", |cpu| {
    cpu.memory_controller.write(0x00, 0x0F).unwrap();
    cpu.a = 0x00;
    cpu.current_opcode = 0b11_111_110;
}, |cpu| {
    cpu.h() && cpu.c() && !cpu.z() && cpu.s()
})]
#[ins_test("cp_r", |cpu| {
    cpu.b = 0x0F;
    cpu.a = 0x0F;
    cpu.current_opcode = 0b10_111_000;
}, |cpu| {
    !cpu.h() && !cpu.c() && cpu.z() && cpu.s()
})]
#[ins_test("cp_hl", |cpu| {
    cpu.write_hl(0x0F).unwrap();
    cpu.a = 0xFE;
    cpu.current_opcode = 0b10_111_110;
}, |cpu| {
   cpu.h() && !cpu.c() && !cpu.z() && cpu.s()
})]
pub fn cp(cpu: &mut SharpLR35902) -> SharpResult {
    let bits = OpcodeBits::from(cpu.current_opcode);

    let (_, flags) = match (bits.x & 1 == 1, bits.z == 0b110) {
        // CP d8
        (true, true) => {
            let d8 = cpu.read_instruction_word()?;
            u8_sub(cpu.registers.a, d8).into_parts()
        }
        // CP r
        (false, false) => {
            let reg = cpu.registers[bits.z];
            u8_sub(cpu.registers.a, reg).into_parts()
        }
        // CP (HL)
        (false, true) => {
            let val = cpu.read_hl()?;
            u8_sub(cpu.registers.a, val).into_parts()
        }
        _ => unreachable!(),
    };

    cpu.f = flags;

    Ok(())
}

// BEGIN HELPER FUNCTIONS
/// Contains the result and flag changes of an ALU operation.
#[derive(Debug, Default, PartialEq)]
pub struct AluResult<T: Default> {
    /// Result of the operation.
    pub result: T,
    /// Whether the zero flag was set or not.
    pub zero: bool,
    /// Whether the carry flag was set or not.
    pub carry: bool,
    /// Whether the half-carry flag was set or not.
    pub half_carry: bool,
    /// Whether the subtract flag was set or not.
    pub subtract: bool,
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
                f |= F_SUBTRACT;
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
