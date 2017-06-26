use std::ops::{Index, IndexMut};
use std::convert::Into;

struct Registers {
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8
}

impl Registers {
    fn new() -> Registers {
        Registers {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0
        }
    }

    fn as_pairs(&self) -> RegisterPairs {
        RegisterPairs {
            af: ((self.a as u16) << 8) | self.f as u16,
            bc: ((self.b as u16) << 8) | self.c as u16,
            de: ((self.d as u16) << 8) | self.e as u16,
            hl: ((self.h as u16) << 8) | self.l as u16
        }
    }
}

impl Index<usize> for Registers {
    type Output = u8;

    fn index(&self, index: usize) -> &u8 {
        match index {
            0 => &self.b,
            1 => &self.c,
            2 => &self.d,
            3 => &self.e,
            4 => &self.h,
            5 => &self.l,
            6 => &self.f,
            7 | _ => &self.a
        }
    }
}

impl IndexMut<usize> for Registers {
    fn index_mut(&mut self, index: usize) -> &mut u8 {
        match index {
            0 => &mut self.b,
            1 => &mut self.c,
            2 => &mut self.d,
            3 => &mut self.e,
            4 => &mut self.h,
            5 => &mut self.l,
            6 => &mut self.f,
            7 | _ => &mut self.a
        }
    }
}

struct RegisterPairs {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16
}

impl Into<Registers> for RegisterPairs {
    fn into(self) -> Registers {
        Registers {
            a: (self.af >> 8) as u8,
            f: (self.af & 0x00FF) as u8,
            b: (self.bc >> 8) as u8,
            c: (self.bc & 0x00FF) as u8,
            d: (self.de >> 8) as u8,
            e: (self.de & 0x00FF) as u8,
            h: (self.hl >> 8) as u8,
            l: (self.hl & 0x00FF) as u8,
        }
    }
}

impl Index<usize> for RegisterPairs {
    type Output = u16;

    fn index(&self, index: usize) -> &u16 {
        match index {
            0 => &self.bc,
            1 => &self.de,
            2 => &self.hl,
            3 | _ => &self.af,
        }
    }
}

impl IndexMut<usize> for RegisterPairs {
    fn index_mut(&mut self, index: usize) -> &mut u16 {
        match index {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 | _ => &mut self.af,
        }
    }
}

pub struct Cpu {
    regs: Registers,
    pc: u16,
    sp: u16,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            regs: Registers::new(),
            pc: 0,
            sp: 0
        }
    }
}

#[test]
fn test_register_conversion() {
    let mut cpu = Cpu::new();

    cpu.regs.a = 0x00;
    cpu.regs.f = 0x11;
    cpu.regs.b = 0x22;
    cpu.regs.c = 0x33;
    cpu.regs.d = 0x44;
    cpu.regs.e = 0x55;
    cpu.regs.h = 0x66;
    cpu.regs.l = 0x77;

    let pairs = cpu.regs.as_pairs();

    assert!(pairs.af == 0x0011);
    assert!(pairs.bc == 0x2233);
    assert!(pairs.de == 0x4455);
    assert!(pairs.hl == 0x6677);

    cpu.regs = pairs.into();

    assert!(cpu.regs.a == 0x00);
    assert!(cpu.regs.f == 0x11);
    assert!(cpu.regs.b == 0x22);
    assert!(cpu.regs.c == 0x33);
    assert!(cpu.regs.d == 0x44);
    assert!(cpu.regs.e == 0x55);
    assert!(cpu.regs.h == 0x66);
    assert!(cpu.regs.l == 0x77);

}