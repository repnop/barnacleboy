use crate::cpu::{
    instructions::{self as ins, u16_add, u16_sub, u8_add, u8_sub, AluResult, OpcodeBits},
    SharpLR35902,
};
use crate::memory::{MemoryError, MemoryInterface};

#[derive(Debug, Default)]
pub(crate) struct DummyMemoryInterface {
    mem: [u8; 32],
}

impl MemoryInterface for DummyMemoryInterface {
    fn read(&self, address: u16) -> Result<u8, MemoryError> {
        Ok(self.mem[address as usize])
    }

    fn write(&mut self, address: u16, data: u8) -> Result<(), MemoryError> {
        self.mem[address as usize] = data;
        Ok(())
    }
}

#[test]
fn flags() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

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
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

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

#[test]
fn opcode_bits() {
    let opcode = 0b1010_1010;
    let bits = OpcodeBits::from(opcode);

    assert_eq!(bits.x, 0b10);
    assert_eq!(bits.y, 0b101);
    assert_eq!(bits.z, 0b010);
    assert_eq!(bits.p, 0b10);
    assert_eq!(bits.q, 0b1);
}

#[test]
fn ld_at_bc_a() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);
    cpu.registers.a = 0xFF;
    cpu.registers.as_dwords().bc = 0x01;
    cpu.current_opcode = 0x02;
    assert!(ins::ld_at_bc_a(&mut cpu).is_ok());
    assert_eq!(cpu.read(0x01).unwrap(), 0xFF);
}

#[test]
fn ld_at_de_a() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);
    cpu.registers.a = 0xFF;
    cpu.registers.as_dwords().de = 0x01;
    cpu.current_opcode = 0x12;
    assert!(ins::ld_at_de_a(&mut cpu).is_ok());
    assert_eq!(cpu.read(0x01).unwrap(), 0xFF);
}

#[test]
fn ld_at_hli_a() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);
    cpu.registers.a = 0xFF;
    cpu.registers.as_dwords().hl = 0x01;
    cpu.current_opcode = 0x22;
    assert!(ins::ld_at_hli_a(&mut cpu).is_ok());
    assert_eq!(cpu.read(0x01).unwrap(), 0xFF);
    assert_eq!(cpu.registers.as_dwords().hl, 0x02);
}

#[test]
fn ld_at_hld_a() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);
    cpu.registers.a = 0xFF;
    cpu.registers.as_dwords().hl = 0x01;
    cpu.current_opcode = 0x22;
    assert!(ins::ld_at_hld_a(&mut cpu).is_ok());
    assert_eq!(cpu.read(0x01).unwrap(), 0xFF);
    assert_eq!(cpu.registers.as_dwords().hl, 0x00);
}

#[test]
fn ld_r_d8() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

    for i in 0u8..8u8 {
        cpu.write(i as u16, i + 1).unwrap();
    }

    cpu.current_opcode = 0x06;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.b, 0x01);

    cpu.current_opcode = 0x0E;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.c, 0x02);

    cpu.current_opcode = 0x16;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.d, 0x03);

    cpu.current_opcode = 0x1E;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.e, 0x04);

    cpu.current_opcode = 0x26;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.h, 0x05);

    cpu.current_opcode = 0x2E;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.l, 0x06);

    cpu.current_opcode = 0x36;
    cpu.registers.as_dwords().hl = 0x0000;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x07);

    cpu.current_opcode = 0x3E;
    assert!(ins::ld_r_d8(&mut cpu).is_ok());
    assert_eq!(cpu.registers.a, 0x08);
}

#[test]
fn ld_r_at_hl() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

    cpu.write(0x0000, 0xFF).unwrap();

    cpu.current_opcode = 0x46;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.b, 0xFF);

    cpu.current_opcode = 0x4E;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.c, 0xFF);

    cpu.current_opcode = 0x56;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.d, 0xFF);

    cpu.current_opcode = 0x5E;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.e, 0xFF);

    cpu.current_opcode = 0x66;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.h, 0xFF);

    cpu.current_opcode = 0x6E;
    cpu.registers.h = 0x00;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.l, 0xFF);

    cpu.current_opcode = 0x7E;
    cpu.registers.l = 0x00;
    assert!(ins::ld_r_at_hl(&mut cpu).is_ok());
    assert_eq!(cpu.registers.a, 0xFF);
}

#[test]
fn ld_at_hl_r() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

    cpu.registers.as_dwords().hl = 0x0011;
    cpu.registers.b = 0x01;
    cpu.registers.c = 0x02;
    cpu.registers.d = 0x03;
    cpu.registers.e = 0x04;
    cpu.registers.a = 0x05;

    cpu.current_opcode = 0x70;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x01);

    cpu.current_opcode = 0x71;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x02);

    cpu.current_opcode = 0x72;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x03);

    cpu.current_opcode = 0x73;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x04);

    cpu.current_opcode = 0x74;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x00);

    cpu.current_opcode = 0x75;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x11);

    // 0x76 is the instruction `HALT`
    cpu.current_opcode = 0x77;
    assert!(ins::ld_at_hl_r(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0x05);
}

#[test]
fn ld_at_hl_d8() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

    cpu.registers.as_dwords().hl = 0x0011;
    cpu.write(0x00, 0xFF).unwrap();

    cpu.current_opcode = 0x36;
    assert!(ins::ld_at_hl_d8(&mut cpu).is_ok());
    assert_eq!(cpu.read_hl().unwrap(), 0xFF);
}

#[test]
fn ld_a_at_bc() {
    let dmi = &mut DummyMemoryInterface::default();
    let mut cpu = SharpLR35902::new(dmi);

    cpu.write(0x0A, 0xFF).unwrap();
    cpu.registers.as_dwords().bc = 0x0A;

    cpu.current_opcode = 0x0A;
    assert!(ins::ld_a_at_bc(&mut cpu).is_ok());
    assert_eq!(cpu.registers.a, 0xFF);
}

#[test]
fn helper_tests() {
    let result = AluResult {
        result: 0xFFu8,
        zero: false,
        carry: false,
        half_carry: false,
        subtract: false,
    };
    assert_eq!(u8_add(0xF0, 0x0F), result);

    let result = AluResult {
        result: 0x10u8,
        zero: false,
        carry: false,
        half_carry: true,
        subtract: false,
    };
    assert_eq!(u8_add(0x0F, 0x01), result);

    let result = AluResult {
        result: 0x00u8,
        zero: true,
        carry: true,
        half_carry: true,
        subtract: false,
    };
    assert_eq!(u8_add(0xFF, 0x01), result);

    let result = AluResult {
        result: 0xEFu8,
        zero: false,
        carry: false,
        half_carry: true,
        subtract: true,
    };
    assert_eq!(u8_sub(0xF0, 0x01), result);

    let result = AluResult {
        result: 0xFFu8,
        zero: false,
        carry: true,
        half_carry: true,
        subtract: true,
    };
    assert_eq!(u8_sub(0x00, 0x01), result);

    let result = AluResult {
        result: 0xFFFFu16,
        zero: false,
        carry: false,
        half_carry: false,
        subtract: false,
    };
    assert_eq!(u16_add(0xFF00, 0x00FF), result);

    let result = AluResult {
        result: 0x1000u16,
        zero: false,
        carry: false,
        half_carry: true,
        subtract: false,
    };
    assert_eq!(u16_add(0x0FFF, 0x0001), result);

    let result = AluResult {
        result: 0x00u16,
        zero: true,
        carry: true,
        half_carry: true,
        subtract: false,
    };
    assert_eq!(u16_add(0xFFFF, 0x0001), result);

    let result = AluResult {
        result: 0xEF00u16,
        zero: false,
        carry: false,
        half_carry: true,
        subtract: true,
    };
    assert_eq!(u16_sub(0xFEFF, 0x0FFF), result);

    let result = AluResult {
        result: 0xFFFFu16,
        zero: false,
        carry: true,
        half_carry: true,
        subtract: true,
    };
    assert_eq!(u16_sub(0x0000, 0x0001), result);
}
