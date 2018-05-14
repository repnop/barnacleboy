use std;
use std::ops::{Index, IndexMut};
use std::convert::Into;

use constants::*;
use interconnect::Interconnect;

pub struct Registers {
    pub a: u8,
    pub f: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8
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

    pub fn as_pairs(&self) -> RegisterPairs {
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

pub struct RegisterPairs {
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16
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

macro_rules! read_byte {
    ($cpu:ident, $ic:ident) => ({
        let retn = $ic.mem_read_byte($cpu.pc);
        $cpu.pc += 1;
        retn
    })
}

macro_rules! read_word {
    ($cpu:ident, $ic:ident) => ({
        let retn = $ic.mem_read_word($cpu.pc);
        $cpu.pc += 2;
        retn
    })
}

pub struct Cpu {
    pub regs: Registers,
    pub pc: u16,
    pub sp: u16,
    pub interrupts_enabled: bool,
    pub halted: bool,
    pub stopped: bool,
    pub finished_bootrom: bool
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            regs: Registers::new(),
            pc: 0,
            sp: 0,
            interrupts_enabled: false,
            halted: false,
            stopped: false,
            finished_bootrom: false
        }
    }

    pub fn set_flags(&mut self, flags: u8) {
        self.regs.f |= flags;
    }

    pub fn clear_flags(&mut self, flags: u8) {
        self.regs.f &= !flags;
    }

    pub fn step(&mut self, ic: &mut Interconnect, debug: bool) -> (bool, u8) {

        if !self.finished_bootrom && self.pc >= 0x100 {
            self.finished_bootrom = true;
            ic.disable_internal_rom();
            //println!("Out of bootrom");
        }
        
        let opcode = read_byte!(self, ic);
        
        let val = self.execute(ic, opcode);

        (val, CYCLE_TABLE[opcode as usize])
    }

    pub fn check_and_handle_interrupts(&mut self, ic: &mut Interconnect, debug: bool) {
        if self.interrupts_enabled {
            let intenabled = ic.mem_read_byte(0xFFFF);
            let mut intrequest = ic.mem_read_byte(0xFF0F);
            let pc = self.pc;
        
            if intenabled & INTERRUPT_FLAG_VBLANK == intrequest & INTERRUPT_FLAG_VBLANK {
                self.interrupts_enabled = false;
                intrequest &= !INTERRUPT_FLAG_VBLANK;
                self.push16(ic, pc);
                self.pc = 0x40;
            } else if intenabled & INTERRUPT_FLAG_LCD_STAT == intrequest & INTERRUPT_FLAG_LCD_STAT {
                self.interrupts_enabled = false;
                intrequest &= !INTERRUPT_FLAG_LCD_STAT;
                self.push16(ic, pc);
                self.pc = 0x48;
            } else if intenabled & INTERRUPT_FLAG_TIMER == intrequest & INTERRUPT_FLAG_TIMER {
                self.interrupts_enabled = false;
                intrequest &= !INTERRUPT_FLAG_TIMER;
                self.push16(ic, pc);
                self.pc = 0x50;
            } else if intenabled & INTERRUPT_FLAG_SERIAL == intrequest & INTERRUPT_FLAG_SERIAL {
                self.interrupts_enabled = false;
                intrequest &= !INTERRUPT_FLAG_SERIAL;
                self.push16(ic, pc);
                self.pc = 0x58;
            } else if intenabled & INTERRUPT_FLAG_JOYPAD == intrequest & INTERRUPT_FLAG_JOYPAD {
                self.interrupts_enabled = false;
                intrequest &= !INTERRUPT_FLAG_JOYPAD;
                self.push16(ic, pc);
                self.pc = 0x60;
            }
        }
    }

    // Returns a bool that indicates whether a 0xDB (debug breakpoint) was triggered
    fn execute(&mut self, ic: &mut Interconnect, opcode: u8) -> bool {
        let mut debug_triggered = false;

        let (x, y, z, p, q) = extract_x_y_z_p_q(opcode);

        match (x, y, z, p, q) {
            // x = 0
            // match on y when z = 0
            (0, 0, 0, _, _) => { },//nop,

            (0, 1, 0, _, _) => self.ld_nn_sp(ic, opcode),

            (0, 2, 0, _, _) => self.stop(ic, opcode),

            (0, 3, 0, _, _) => self.jr_d(ic, opcode),

            (0, 4...7, 0, _, _) => self.jr_cc_d(ic, opcode),

            // match on q when z = 1
            (0, _, 1, _, 0) => self.ld_rp_nn(ic, opcode),

            (0, _, 1, _, 1) => self.add_hl_rp(ic, opcode),

            // match on p when z = 2, q = 0
            (0, _, 2, 0, 0) => self.ld_bc_a(ic, opcode),

            (0, _, 2, 1, 0) => self.ld_de_a(ic, opcode),

            (0, _, 2, 2, 0) => self.ld_hlp_a(ic, opcode),

            (0, _, 2, 3, 0) => self.ld_hlm_a(ic, opcode),

            // match on p when z = 2, q = 1
            (0, _, 2, 0, 1) => self.ld_a_bc(ic, opcode),

            (0, _, 2, 1, 1) => self.ld_a_de(ic, opcode),

            (0, _, 2, 2, 1) => self.ld_a_hlp(ic, opcode),

            (0, _, 2, 3, 1) => self.ld_a_hlm(ic, opcode),

            // match on q when z = 3
            (0, _, 3, _, 0) => self.inc_rp(ic, opcode),
            (0, _, 3, _, 1) => self.dec_rp(ic, opcode),

            // match on z = 4 ... 6
            (0, _, 4, _, _) => self.inc_r(ic, opcode),

            (0, _, 5, _, _) => self.dec_r(ic, opcode),

            (0, _, 6, _, _) => self.ld_r(ic, opcode),

            // match on y when z = 7
            (0, 0, 7, _, _) => self.rlca(ic, opcode),

            (0, 1, 7, _, _) => self.rrca(ic, opcode),

            (0, 2, 7, _, _) => self.rla(ic, opcode),

            (0, 3, 7, _, _) => self.rra(ic, opcode),

            (0, 4, 7, _, _) => self.daa(ic, opcode),

            (0, 5, 7, _, _) => self.cpl(ic, opcode),

            (0, 6, 7, _, _) => self.scf(ic, opcode),

            (0, 7, 7, _, _) => self.ccf(ic, opcode),

            // x = 1
            // match on z = 0 ... 6
            (1, 6, 6, _, _) => self.halt(ic, opcode),
            (1, _, 0...7, _, _) => self.ld_r_r(ic, opcode),
            //(1, _, 7, _, _) => self.ld_r_r(ic, opcode),

            // x = 2
            (2, _, _, _, _) => self.alu_r(ic, opcode),

            // x = 3
            // match on y when z = 0
            (3, 0...3, 0, _, _) => self.ret_cc(ic, opcode),

            (3, 4, 0, _, _) => self.ld_ff00_n_a(ic, opcode),

            (3, 5, 0, _, _) => self.add_sp_d(ic, opcode),

            (3, 6, 0, _, _) => self.ld_a_ff00_n(ic, opcode),

            (3, 7, 0, _, _) => self.ld_hl_sp_d(ic, opcode),

            // match on q = 0, z = 1
            (3, _, 1, _, 0) => self.pop_rp2(ic, opcode),

            // match on p when q = 1, z = 1
            (3, _, 1, 0, 1) => self.ret(ic, opcode),
            (3, _, 1, 1, 1) => self.reti(ic, opcode),
            (3, _, 1, 2, 1) => self.jp_hl(ic, opcode),
            (3, _, 1, 3, 1) => self.ld_sp_hl(ic, opcode),

            // match on y when z = 2
            (3, 0...3, 2, _, _) => self.jp_cc_nn(ic, opcode),

            (3, 4, 2, _, _) => self.ld_ff00_c_a(ic, opcode),

            (3, 5, 2, _, _) => self.ld_nn_a(ic, opcode),
            (3, 6, 2, _, _) => self.ld_a_ff00_c(ic, opcode),
            (3, 7, 2, _, _) => self.ld_a_nn(ic, opcode),

            // match on y when z = 3
            (3, 0, 3, _, _) => self.jp_nn(ic, opcode),

            (3, 1, 3, _, _) => self.prefixed(ic, opcode),
            (3, 2, 3, _, _) => { },
            (3, 3, 3, _, _) => debug_triggered = true,
            (3, 4...5, 3, _, _) => { },

            (3, 6, 3, _, _) => self.di(ic, opcode),

            (3, 7, 3, _, _) => self.ei(ic, opcode),

            // match on y when z = 4
            (3, 0...3, 4, _, _) => self.call_cc_nn(ic, opcode),

            // match on q when z = 5
            (3, _, 5, _, 0) => self.push_rp2(ic, opcode),
            (3, _, 5, 0, 1) => self.call_nn(ic, opcode),

            // z = 6, 7
            (3, _, 6, _, _) => self.alu_n(ic, opcode),

            (3, _, 7, _, _) => self.rst_y8(ic, opcode),

            // shouldn't get here, and if we do, something has gone very wrong
            _ => { },
        };

        debug_triggered
    }

    fn ld_nn_sp(&mut self, ic: &mut Interconnect, op: u8) {
        let addr = read_word!(self, ic);

        ic.mem_write_word(addr, self.sp);
        
    }

    fn stop(&mut self, ic: &mut Interconnect, op: u8) {
        self.stopped = true;
    }

    fn jr_d(&mut self, ic: &mut Interconnect, op: u8) {
        let offset = read_byte!(self, ic) as i8;

        let mut pc = self.pc as i16;
        pc += offset as i16;

        self.pc = pc as u16;
    }

    fn jr_cc_d(&mut self, ic: &mut Interconnect, op: u8) {
        let y = (extract_x_y_z_p_q(op).1 - 4) as usize;
        let val = read_byte!(self, ic);
        let mut flags = self.regs.f;

        if y % 2 == 0 {
            flags = !flags;
        }

        if flags & CC[y] == CC[y] {

            self.pc = (self.pc as i16 + (val as i8) as i16) as u16;
        }
    }

    fn ld_rp_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let data = read_word!(self, ic);

        let p = extract_x_y_z_p_q(op).3 as usize;

        if p == 3 {
            self.sp = data;
        } else {
            let mut pairs = self.regs.as_pairs();
            pairs[RP[p]] = data;
            self.regs = pairs.into();
        }
    }

    fn add_hl_rp(&mut self, ic: &mut Interconnect, op: u8) {
        let p = extract_x_y_z_p_q(op).3 as usize;

        let mut pairs = self.regs.as_pairs();

        let (hl, xx) = (pairs.hl,
                        if p == 3 {
                            self.sp
                        } else {
                            pairs[RP[p]]
                        });

        let (fc, hc) = get_chc(false, true, hl, xx);
        self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_SUB);
        self.set_flags(fc | hc);

        pairs.hl = hl + xx;
        self.regs = pairs.into();
    }

    fn ld_bc_a(&mut self, ic: &mut Interconnect, op: u8) {
        let pairs = self.regs.as_pairs();

        ic.mem_write_byte(pairs.bc, self.regs.a);
    }

    fn ld_de_a(&mut self, ic: &mut Interconnect, op: u8) {
        let pairs = self.regs.as_pairs();

        ic.mem_write_byte(pairs.de, self.regs.a);
    }

    fn ld_hlp_a(&mut self, ic: &mut Interconnect, op: u8) {
        let mut pairs = self.regs.as_pairs();

        ic.mem_write_byte(pairs.hl, self.regs.a);

        pairs.hl += 1;

        self.regs = pairs.into();
    }

    fn ld_hlm_a(&mut self, ic: &mut Interconnect, op: u8) {
        let mut pairs = self.regs.as_pairs();

        ic.mem_write_byte(pairs.hl, self.regs.a);

        pairs.hl -= 1;

        self.regs = pairs.into();
    }

    fn ld_a_bc(&mut self, ic: &mut Interconnect, op: u8) {
        let pairs = self.regs.as_pairs();

        self.regs.a = ic.mem_read_byte(pairs.bc);
    }

    fn ld_a_de(&mut self, ic: &mut Interconnect, op: u8) {
        let pairs = self.regs.as_pairs();

        self.regs.a = ic.mem_read_byte(pairs.de);
    }

    fn ld_a_hlp(&mut self, ic: &mut Interconnect, op: u8) {
        let mut pairs = self.regs.as_pairs();

        self.regs.a = ic.mem_read_byte(pairs.hl);

        pairs.hl += 1;

        self.regs = pairs.into();
    }

    fn ld_a_hlm(&mut self, ic: &mut Interconnect, op: u8) {
        let mut pairs = self.regs.as_pairs();

        self.regs.a = ic.mem_read_byte(pairs.hl);

        pairs.hl -= 1;

        self.regs = pairs.into();
    }

    fn inc_rp(&mut self, ic: &mut Interconnect, op: u8) {
        let p = extract_x_y_z_p_q(op).3 as usize;

        if p == 3 {
            self.sp += 1;
        } else {
            let mut pairs = self.regs.as_pairs();
            pairs[RP[p]] += 1;
            self.regs = pairs.into();
        }
    }

    fn dec_rp(&mut self, ic: &mut Interconnect, op: u8) {
        let p = extract_x_y_z_p_q(op).3 as usize;

        if p == 3 {
            self.sp += 1;
        } else {
            let mut pairs = self.regs.as_pairs();
            pairs[RP[p]] -= 1;
            self.regs = pairs.into();
        }
    }

    fn inc_r(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;

        let (_, hc) = get_chc(true, true, self.regs[R[y]] as u16, 1);

        self.clear_flags(FLAG_SUB | FLAG_ZERO);
        self.set_flags(hc);

        self.regs[R[y]] += 1;

        if self.regs[R[y]] == 0 {
            self.set_flags(FLAG_ZERO);
        }
    }

    fn dec_r(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;

        let (_, hc) = get_chc(true, false, self.regs[R[y]] as u16, 1);

        self.clear_flags(FLAG_SUB | FLAG_ZERO);
        self.set_flags(hc);

        self.regs[R[y]] -= 1;

        if self.regs[R[y]] == 0 {
            self.set_flags(FLAG_ZERO);
        }
    }

    fn ld_r(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;
        let val = read_byte!(self, ic);

        if y != 0b110 {
            self.regs[R[y]] = val;
        } else {
            ic.mem_write_byte(self.regs.as_pairs().hl, val);
        }
    }

    fn rlca(&mut self, ic: &mut Interconnect, op: u8) {
        if self.regs.a & 0x80 == 0x80 {
            self.set_flags(FLAG_CARRY);
        }

        self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

        self.regs.a = self.regs.a.rotate_left(1);
    }

    fn rrca(&mut self, ic: &mut Interconnect, op: u8) {
        if self.regs.a & 0x01 == 0x01 {
            self.set_flags(FLAG_CARRY);
        }

        self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

        self.regs.a = self.regs.a.rotate_right(1);
    }

    fn rla(&mut self, ic: &mut Interconnect, op: u8) {
        let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;

        if self.regs.a & 0x80 == 0x80 {
            self.set_flags(FLAG_CARRY);
        }

        self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

        self.regs.a = self.regs.a << 1 | if carry { 1 } else { 0 };
    }

    fn rra(&mut self, ic: &mut Interconnect, op: u8) {
        let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;

        if self.regs.a & 0x01 == 0x01 {
            self.set_flags(FLAG_CARRY);
        }

        self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

        self.regs.a = self.regs.a >> 1 | if carry { 1 << 7 } else { 0 };
    }

    fn daa(&mut self, ic: &mut Interconnect, op: u8) {
        let is_sub = (self.regs.f & FLAG_SUB) >> FLAGS_SUB_INDEX == 1;
        let cf = self.regs.f & FLAG_CARRY;
        let hcf = self.regs.f & FLAG_HALF_CARRY;
        let topnib = (self.regs.a & 0xF0) >> 4;
        let lownib = self.regs.a & 0x0F;

        if is_sub {
            match (cf, topnib, hcf, lownib) {
                (0, 0x0...0x9, 0, 0x0...0x9) => {
                    self.clear_flags(FLAG_CARRY);
                }
                (0, 0x0...0x8, 1, 0x6...0xF) => {
                    self.regs.a += 0xFA;
                    self.clear_flags(FLAG_CARRY);
                }
                (1, 0x7...0xF, 0, 0x0...0x9) => {
                    self.regs.a += 0xA0;
                    self.set_flags(FLAG_CARRY);
                }
                (1, 0x6...0xF, 1, 0x6...0xF) => {
                    self.regs.a += 0x9A;
                    self.set_flags(FLAG_CARRY);
                }
                _ => {}
            }
        } else {
            match (cf, topnib, hcf, lownib) {
                (0, 0x0...0x9, 0, 0x0...0x9) => {
                    self.clear_flags(FLAG_CARRY);
                }
                (0, 0x0...0x8, 0, 0xA...0xF) => {
                    self.regs.a += 6;
                    self.clear_flags(FLAG_CARRY);
                }
                (0, 0x0...0x9, 1, 0x0...0x3) => {
                    self.regs.a += 6;
                    self.clear_flags(FLAG_CARRY);
                }
                (0, 0xA...0xF, 0, 0x0...0x9) => {
                    self.regs.a += 0x60;
                    self.set_flags(FLAG_CARRY);
                }
                (0, 0x9...0xF, 0, 0xA...0xF) => {
                    self.regs.a += 0x66;
                    self.set_flags(FLAG_CARRY);
                }
                (0, 0xA...0xF, 1, 0x0...0x3) => {
                    self.regs.a += 0x66;
                    self.set_flags(FLAG_CARRY);
                }
                (1, 0x0...0x2, 0, 0x0...0x9) => {
                    self.regs.a += 0x60;
                    self.set_flags(FLAG_CARRY);
                }
                (1, 0x0...0x2, 0, 0xA...0xF) => {
                    self.regs.a += 0x66;
                    self.set_flags(FLAG_CARRY);
                }
                (1, 0x0...0x3, 1, 0x0...0x3) => {
                    self.regs.a += 0x66;
                    self.set_flags(FLAG_CARRY);
                }
                _ => {}
            }
        }

        if self.regs.a == 0 {
            self.set_flags(FLAG_ZERO);
        }

        self.clear_flags(FLAG_SUB);
    }

    fn cpl(&mut self, ic: &mut Interconnect, op: u8) {
        self.regs.a = !self.regs.a;
    }

    fn scf(&mut self, ic: &mut Interconnect, op: u8) {
        self.set_flags(FLAG_CARRY);
        self.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);
    }

    fn ccf(&mut self, ic: &mut Interconnect, op: u8) {
        if self.regs.f & FLAG_CARRY == FLAG_CARRY {
            self.clear_flags(FLAG_CARRY);
        } else {
            self.set_flags(FLAG_CARRY);
        }

        self.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);
    }

    fn ld_r_r(&mut self, ic: &mut Interconnect, op: u8) {
        let (_, y, z, _, _) = extract_x_y_z_p_q(op);

        if y == 0b110 {
            ic.mem_write_byte(self.regs.as_pairs().hl, self.regs[R[z as usize]]);
        } else if z == 0b110 {
            self.regs[R[y as usize]] = ic.mem_read_byte(self.regs.as_pairs().hl);
        } else {
            self.regs[R[y as usize]] = self.regs[R[z as usize]];
        }
    }

    fn halt(&mut self, ic: &mut Interconnect, op: u8) {
        self.halted = true;
    }

    fn alu_r(&mut self, ic: &mut Interconnect, op: u8) {
        let (_, y, z, _, _) = extract_x_y_z_p_q(op);
        let mut reg_val = self.regs[R[z as usize]];
        if R[z as usize] == 0b110 {
            reg_val = ic.mem_read_byte(self.regs.as_pairs().hl);
        }

        match y {
            // ADD A, R[z]
            0 => {
                let (fc, hc) = get_chc(true, true, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
                self.set_flags(fc | hc);

                self.regs.a += reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }

            }

            // ADC A, R[z]
            1 => {
                let (fc, hc) = get_chc(true, true, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
                self.set_flags(fc | hc);

                let (val, of) = self.regs.a.overflowing_add(reg_val);
                self.regs.a = val;

                if of {
                    self.regs.a += 1;
                }

                if self.regs.a == 0 {
                    self.regs.f |= 1 << FLAGS_ZERO_INDEX;
                }
            }

            // SUB R[z]
            2 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                self.regs.a -= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // SBC A, R[z]
            3 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                let (val, of) = self.regs.a.overflowing_sub(reg_val);
                self.regs.a = val;

                if of {
                    self.regs.a -= 1;
                }

                if self.regs.a == 0 {
                    self.regs.f |= 1 << FLAGS_ZERO_INDEX;
                }
            }

            // AND R[z]
            4 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY);
                self.set_flags(FLAG_HALF_CARRY);

                self.regs.a &= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // XOR R[z]
            5 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

                self.regs.a ^= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // OR R[z]
            6 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

                self.regs.a |= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // CP R[z]
            7 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                if self.regs.a - reg_val == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }
            _ => unreachable!(),
        };
    }

    fn ret_cc(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;

        let mut flags = self.regs.f;

        if y % 2 == 0 {
            flags = !flags;
        }

        if flags & CC[y] == CC[y] {
            let val = self.pop16(ic);
            self.pc = val;
        }
    }

    fn ld_ff00_n_a(&mut self, ic: &mut Interconnect, op: u8) {
        let nn = read_byte!(self, ic);

        ic.mem_write_byte(0xFF00 + nn as u16, self.regs.a);
    }

    fn add_sp_d(&mut self, ic: &mut Interconnect, op: u8) {
        let d = read_byte!(self, ic);

        self.clear_flags(FLAG_ZERO | FLAG_SUB);

        let (fc, hc) = (if (self.sp as i16).overflowing_add(d as i16).1 { FLAG_CARRY } else { 0 },
                        if (((self.sp as i16) & 0x0FFF) + ((d as i16) & 0x0FFF)) & 0x1000 == 0x1000 { FLAG_HALF_CARRY } else { 0 });

        self.set_flags(fc | hc);

        self.sp = (self.sp as i16 + d as i16) as u16;

        
    }

    fn ld_a_ff00_n(&mut self, ic: &mut Interconnect, op: u8) {
        let nn = read_byte!(self, ic);
        
        self.regs.a = ic.mem_read_byte(0xFF00 + nn as u16);
    }

    fn ld_hl_sp_d(&mut self, ic: &mut Interconnect, op: u8) {
        let d = read_byte!(self, ic);
        let sp = self.sp;
        self.clear_flags(FLAG_ZERO | FLAG_SUB);

        let (fc, hc) = (if (self.sp as i16).overflowing_add(d as i16).1 { FLAG_CARRY } else { 0 },
                        if (((self.sp as i16) & 0x0FFF) + ((d as i16) & 0x0FFF)) & 0x1000 == 0x1000 { FLAG_HALF_CARRY } else { 0 });

        self.set_flags(fc | hc);

        let mut pairs = self.regs.as_pairs();

        pairs.hl = (sp as i16 + d as i16) as u16;

        self.regs = pairs.into();
    }

    fn pop_rp2(&mut self, ic: &mut Interconnect, op: u8) {
        let p = extract_x_y_z_p_q(op).3 as usize;
        let val = self.pop16(ic);
        let mut pairs = self.regs.as_pairs();

        pairs[RP2[p]] = val;

        self.regs = pairs.into();
    }

    fn ret(&mut self, ic: &mut Interconnect, op: u8) {
        self.pc = self.pop16(ic);
    }

    fn reti(&mut self, ic: &mut Interconnect, op: u8) {
        self.interrupts_enabled = true;
        self.pc = self.pop16(ic);
    }

    fn jp_hl(&mut self, ic: &mut Interconnect, op: u8) {
        self.pc = self.regs.as_pairs().hl;
    }

    fn ld_sp_hl(&mut self, ic: &mut Interconnect, op: u8) {
        self.sp = self.regs.as_pairs().hl;
    }

    fn jp_cc_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;
        let val = read_word!(self, ic);
        let mut flags = self.regs.f;

        if y % 2 == 0 {
            flags = !flags;
        }

        if flags & CC[y] == CC[y] {
            self.pc = val;
        }
    }

    fn ld_ff00_c_a(&mut self, ic: &mut Interconnect, op: u8) {
        ic.mem_write_byte(0xFF00 + self.regs.c as u16, self.regs.a);
    }

    fn ld_nn_a(&mut self, ic: &mut Interconnect, op: u8) {
        let nn = read_word!(self, ic);
        
        ic.mem_write_byte(nn, self.regs.a);
    }

    fn ld_a_ff00_c(&mut self, ic: &mut Interconnect, op: u8) {
        self.regs.a = ic.mem_read_byte(0xFF00 + self.regs.c as u16);
    }

    fn ld_a_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let nn = read_word!(self, ic);

        self.regs.a = ic.mem_read_byte(nn);
    }

    fn jp_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let val = read_word!(self, ic);
        self.pc = val;
    }

    fn di(&mut self, ic: &mut Interconnect, op: u8) {
        self.interrupts_enabled = false;
    }

    fn ei(&mut self, ic: &mut Interconnect, op: u8) {
        self.interrupts_enabled = true;
    }

    fn call_cc_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1 as usize;
        let val = read_word!(self, ic);
        let mut flags = self.regs.f;
        let pc = self.pc;

        if y % 2 == 0 {
            flags = !flags;
        }

        if flags & CC[y] == CC[y] {
            self.push16(ic, pc);
            self.pc = val;
        }
    }

    fn push_rp2(&mut self, ic: &mut Interconnect, op: u8) {
        let p = extract_x_y_z_p_q(op).3 as usize;
        let val = self.regs.as_pairs()[RP2[p]];

        self.push16(ic, val);
    }

    fn call_nn(&mut self, ic: &mut Interconnect, op: u8) {
        let val = read_word!(self, ic);
        let pc = self.pc;

        self.push16(ic, pc);
        self.pc = val;
    }

    fn alu_n(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1;

        let reg_val = read_byte!(self, ic);
        match y {
            // ADD A, d8
            0 => {
                let (fc, hc) = get_chc(true, true, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
                self.set_flags(fc | hc);

                self.regs.a += reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }

            }

            // ADC A, d8
            1 => {
                let (fc, hc) = get_chc(true, true, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
                self.set_flags(fc | hc);

                let (val, of) = self.regs.a.overflowing_add(reg_val);
                self.regs.a = val;

                if of {
                    self.regs.a += 1;
                }

                if self.regs.a == 0 {
                    self.regs.f |= 1 << FLAGS_ZERO_INDEX;
                }
            }

            // SUB d8
            2 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                self.regs.a -= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // SBC A, d8
            3 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                let (val, of) = self.regs.a.overflowing_sub(reg_val);
                self.regs.a = val;

                if of {
                    self.regs.a -= 1;
                }

                if self.regs.a == 0 {
                    self.regs.f |= 1 << FLAGS_ZERO_INDEX;
                }
            }

            // AND d8
            4 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY);
                self.set_flags(FLAG_HALF_CARRY);

                self.regs.a &= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // XOR d8
            5 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

                self.regs.a ^= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // OR d8
            6 => {
                self.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

                self.regs.a |= reg_val;

                if self.regs.a == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }

            // CP d8
            7 => {
                let (fc, hc) = get_chc(true, false, self.regs.a as u16, reg_val as u16);

                self.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
                self.set_flags(fc | hc | FLAG_SUB);

                if self.regs.a - reg_val == 0 {
                    self.set_flags(FLAG_ZERO);
                }
            }
            _ => unreachable!(),
        };
    }

    fn rst_y8(&mut self, ic: &mut Interconnect, op: u8) {
        let y = extract_x_y_z_p_q(op).1;
        let pc = self.pc;

        self.push16(ic, pc);
        self.pc = (y * 8) as u16;
    }

    fn prefixed(&mut self, ic: &mut Interconnect, op: u8) {
        let op = read_byte!(self, ic);

        let (x, y, z, _, _) = extract_x_y_z_p_q(op);
        match x {
            0 => {
                match y {
                    0 => {
                        if z != 0b110 {
                            if self.regs[R[z as usize]] & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            self.regs[R[z as usize]] = self.regs[R[z as usize]].rotate_left(1);
                        } else {
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            if hl & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            if hl.rotate_left(1) == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, hl.rotate_left(1));
                        }
                    }
                    1 => {
                        if z != 0b110 {
                            if self.regs[R[z as usize]] & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            self.regs[R[z as usize]] = self.regs[R[z as usize]].rotate_right(1);
                        } else {
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            if hl & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            if hl.rotate_right(1) == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, hl.rotate_right(1));
                        }
                    }
                    2 => {
                        if z != 0b110 {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;

                            if self.regs[R[z as usize]] & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = self.regs[R[z as usize]] << 1 | if carry { 1 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            self.regs[R[z as usize]] = val;
                        } else {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            if hl & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = hl << 1 | if carry { 1 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, val);
                        }
                    }
                    3 => {
                        if z != 0b110 {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;

                            if self.regs[R[z as usize]] & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = self.regs[R[z as usize]] >> 1 |
                                    if carry { 1 << 7 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            self.regs[R[z as usize]] = val;
                        } else {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            if hl & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = hl >> 1 | if carry { 1 << 7 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, val);
                        }
                    }
                    4 => {
                        if z != 0b110 {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;

                            if self.regs[R[z as usize]] & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = self.regs[R[z as usize]] << 1;

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            self.regs[R[z as usize]] = val;
                        } else {
                            let carry = self.regs.f & FLAG_CARRY == FLAG_CARRY;
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            if hl & 0x80 == 0x80 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = hl << 1;

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, val);
                        }
                    }
                    5 => {
                        if z != 0b110 {
                            let carry = self.regs[R[z as usize]] & 0x80 == 0x80;

                            if self.regs[R[z as usize]] & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = self.regs[R[z as usize]] >> 1 |
                                    if carry { 0x80 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            self.regs[R[z as usize]] = val;
                        } else {
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            let carry = hl & 0x80 == 0x80;
                            if hl & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = hl >> 1 | if carry { 0x80 } else { 0 };

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, val);
                        }
                    }
                    6 => {
                        if z != 0b110 {
                            let top = self.regs[R[z as usize]] & 0xF0;
                            let bot = self.regs[R[z as usize]] & 0x0F;

                            self.regs[R[z as usize]] = (bot << 4) | (top >> 4);

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY | FLAG_CARRY);

                            if self.regs[R[z as usize]] == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                        } else {
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);
                            let top = hl & 0xF0;
                            let bot = hl & 0x0F;

                            ic.mem_write_byte(self.regs.as_pairs().hl, (bot << 4) | (top >> 4));

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY | FLAG_CARRY);

                            if (bot << 4) | (top >> 4) == 0 {
                                self.set_flags(FLAG_ZERO);
                            }
                        }
                    }
                    7 => {
                        if z != 0b110 {
                            if self.regs[R[z as usize]] & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = self.regs[R[z as usize]] >> 1;

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            self.regs[R[z as usize]] = val;
                        } else {
                            let hl = ic.mem_read_byte(self.regs.as_pairs().hl);

                            if hl & 0x01 == 0x01 {
                                self.set_flags(FLAG_CARRY);
                            }

                            self.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                            let val = hl >> 1;

                            if val == 0 {
                                self.set_flags(FLAG_ZERO);
                            }

                            ic.mem_write_byte(self.regs.as_pairs().hl, val);
                        }
                    }
                    _ => unreachable!(),
                }
            }
            1 => {
                if z != 0b110 {
                    let result = (1 << y) & self.regs[R[z as usize]];

                    self.clear_flags(FLAG_SUB | FLAG_ZERO);
                    self.set_flags(FLAG_HALF_CARRY);

                    if result == 0 {
                        self.set_flags(FLAG_ZERO);
                    }
                } else {
                    let result = (1 << y) & ic.mem_read_byte(self.regs.as_pairs().hl);

                    self.clear_flags(FLAG_SUB | FLAG_ZERO);
                    self.set_flags(FLAG_HALF_CARRY);

                    if result == 0 {
                        self.set_flags(FLAG_ZERO);
                    }
                }
            }
            2 => {
                if z != 0b110 {
                    self.regs[R[z as usize]] &= !(1 << y);
                } else {
                    let hl = ic.mem_read_byte(self.regs.as_pairs().hl);

                    ic.mem_write_byte(self.regs.as_pairs().hl, hl & !(1 << y));
                }
            }
            3 => {
                if z != 0b110 {
                    self.regs[R[z as usize]] |= (1 << y);
                } else {
                    let hl = ic.mem_read_byte(self.regs.as_pairs().hl);

                    ic.mem_write_byte(self.regs.as_pairs().hl, hl | (1 << y));
                }
            }
            _ => unreachable!(),
        };
    }

    pub fn push16(&mut self, interconnect: &mut Interconnect, val: u16) {
        interconnect.mem_write_byte(self.sp - 2, ((0xFF00 & val) >> 8) as u8);
        interconnect.mem_write_byte(self.sp - 1, (0x00FF & val) as u8);

        self.sp -= 2;
    }

    pub fn pop16(&mut self, interconnect: &mut Interconnect) -> u16 {
        let high = interconnect.mem_read_byte(self.sp);
        let low = interconnect.mem_read_byte(self.sp + 1);

        self.sp += 2;

        ((high as u16) << 8) & 0xFF00 | (0x00FF & low as u16)
    }
}

fn extract_x_y_z_p_q(opcode: u8) -> (u8, u8, u8, u8, u8) {
        let (x, y, z) = ((opcode & 0b11000000) >> 6, (opcode & 0b00111000) >> 3, opcode & 0b00000111);

        let (p, q) = ((y & 0b00000110) >> 1, y & 0b00000001);

        (x, y, z, p, q)
}

fn get_chc(byte: bool, adding: bool, first: u16, second: u16) -> (u8, u8) {
    let mut result = (0u8, 0u8);

    if byte {
        if adding {
            result.0 = if (first as u8) > (0xFF - second as u8) { FLAG_CARRY } else { 0 };
            result.1 = if (first & 0x0F) + (second & 0x0F) > 0x0F { FLAG_HALF_CARRY } else { 0 };
        } else {
            result.0 = if (first as u8).overflowing_sub(second as u8).1 { 1 << FLAGS_CARRY_INDEX } else { 0 };
            result.1 = (((((first as u8) & 0x0F) - ((second as u8) & 0x0F)) & 0x80)) >> 3;
        }
    } else {
        if adding {
            result.0 = if (first).overflowing_add(second).1 { 1 << FLAGS_CARRY_INDEX } else { 0 };
            result.1 = (((((first) & 0xFFF) + ((second) & 0xFFF)) & 0x1000) >> 12) as u8;
        } else {
            result.0 = if (first).overflowing_sub(second).1 { 1 << FLAGS_CARRY_INDEX } else { 0 };
            result.1 = (((((first) & 0xFFF) + ((second) & 0xFFF)) & 0x1000) >> 12) as u8;
        }
    }

    result
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