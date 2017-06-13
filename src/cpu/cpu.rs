//use cpu::instruction::{Instruction, Operation};
use interconnect::Interconnect;
use cpu::cpuconsts::*;

#[derive(Debug)]
pub struct Cpu {
    pub registers: [u8; 8],
    pub flags: u8,
    pub stack_pointer: u16,
    pub program_counter: u16,
    pub current_opcode: u8,
    pub interrupts_enabled: bool,
}

pub enum CpuError {
    GPError,
    _MemRWError,
    Stop,
    Halt,
    _UnknownOpcode,
}

#[derive(Clone, Copy)]
enum WordRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
}

static R: [usize; 8] = [REG_B_INDEX,
                        REG_C_INDEX,
                        REG_D_INDEX,
                        REG_E_INDEX,
                        REG_H_INDEX,
                        REG_L_INDEX,
                        0b110, // HL
                        REG_A_INDEX];

static RP: [WordRegister; 4] =
    [WordRegister::BC, WordRegister::DE, WordRegister::HL, WordRegister::SP];

static RP2: [WordRegister; 4] =
    [WordRegister::BC, WordRegister::DE, WordRegister::HL, WordRegister::AF];

static CC: [u8; 4] = [FLAG_ZERO, FLAG_ZERO, FLAG_CARRY, FLAG_CARRY];

macro_rules! read_byte_at_pc {
    ($cpu:ident, $interconnect:ident) => ({
        let ret = $interconnect.mem_read_byte($cpu.program_counter);
        $cpu.program_counter += 1;
        ret
    })
}

macro_rules! read_word_at_pc {
    ($cpu:ident, $interconnect:ident) => ({
        let ret = $interconnect.mem_read_word($cpu.program_counter);
        $cpu.program_counter += 2;
        ret
    })
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            registers: [0; 8],
            flags: 0,
            stack_pointer: 0,
            program_counter: 0,
            current_opcode: 0,
            interrupts_enabled: true,
        }
    }

    pub fn run_next_instruction(&mut self, interconnect: &mut Interconnect) -> Result<(), CpuError> {
        self.current_opcode = interconnect.mem_read_byte(self.program_counter);
        self.program_counter += 1;
        Cpu::execute_instruction(self, interconnect)
    }

    pub fn execute_instruction(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

        let opcode = cpu.current_opcode;

        let (x, y, z, p, q) = extract_x_y_z_p_q(opcode);

        match (x, y, z, p, q) {
            // x = 0
            // match on y when z = 0
            (0, 0, 0, _, _) => {}
            (0, 1, 0, _, _) => ld_nn_sp(cpu, interconnect)?,
            (0, 2, 0, _, _) => stop(cpu, interconnect)?,
            (0, 3, 0, _, _) => jr_d(cpu, interconnect)?,
            (0, 4...7, 0, _, _) => jr_cc_d(cpu, interconnect)?,

            // match on q when z = 1
            (0, _, 1, _, 0) => ld_rp_nn(cpu, interconnect)?,
            (0, _, 1, _, 1) => add_hl_rp(cpu, interconnect)?,

            // match on p when z = 2, q = 0
            (0, _, 2, 0, 0) => ld_bc_a(cpu, interconnect)?,
            (0, _, 2, 1, 0) => ld_de_a(cpu, interconnect)?,
            (0, _, 2, 2, 0) => ld_hlp_a(cpu, interconnect)?,
            (0, _, 2, 3, 0) => ld_hlm_a(cpu, interconnect)?,

            // match on p when z = 2, q = 1
            (0, _, 2, 0, 1) => ld_a_bc(cpu, interconnect)?,
            (0, _, 2, 1, 1) => ld_a_de(cpu, interconnect)?,
            (0, _, 2, 2, 1) => ld_a_hlp(cpu, interconnect)?,
            (0, _, 2, 3, 1) => ld_a_hlm(cpu, interconnect)?,

            // match on q when z = 3
            (0, _, 3, _, 0) => inc_rp(cpu, interconnect)?,
            (0, _, 3, _, 1) => dec_rp(cpu, interconnect)?,

            // match on z = 4 ... 6
            (0, _, 4, _, _) => inc_r(cpu, interconnect)?,
            (0, _, 5, _, _) => dec_r(cpu, interconnect)?,
            (0, _, 6, _, _) => ld_r(cpu, interconnect)?,

            // match on y when z = 7
            (0, 0, 7, _, _) => rlca(cpu, interconnect)?,
            (0, 1, 7, _, _) => rrca(cpu, interconnect)?,
            (0, 2, 7, _, _) => rla(cpu, interconnect)?,
            (0, 3, 7, _, _) => rra(cpu, interconnect)?,
            (0, 4, 7, _, _) => daa(cpu, interconnect)?,
            (0, 5, 7, _, _) => cpl(cpu, interconnect)?,
            (0, 6, 7, _, _) => scf(cpu, interconnect)?,
            (0, 7, 7, _, _) => ccf(cpu, interconnect)?,

            // x = 1
            // match on z = 0 ... 6
            (1, _, 0...5, _, _) => ld_r_r(cpu, interconnect)?,
            (1, _, 6, _, _) => halt(cpu, interconnect)?,

            // x = 2
            (2, _, _, _, _) => alu_r(cpu, interconnect)?,

            // x = 3
            // match on y when z = 0
            (3, 0...3, 0, _, _) => ret_cc(cpu, interconnect)?,
            (3, 4, 0, _, _) => ld_ff00_nn_a(cpu, interconnect)?,
            (3, 5, 0, _, _) => add_sp_d(cpu, interconnect)?,
            (3, 6, 0, _, _) => ld_a_ff00_nn(cpu, interconnect)?,
            (3, 7, 0, _, _) => ld_hl_sp_d(cpu, interconnect)?,

            // match on q = 0, z = 1
            (3, _, 1, _, 0) => pop_rp2(cpu, interconnect)?,

            // match on p when q = 1, z = 1
            (3, _, 1, 0, 1) => ret(cpu, interconnect)?,
            (3, _, 1, 1, 1) => reti(cpu, interconnect)?,
            (3, _, 1, 2, 1) => jp_hl(cpu, interconnect)?,
            (3, _, 1, 3, 1) => ld_sp_hl(cpu, interconnect)?,

            // match on y when z = 2
            (3, 0...3, 2, _, _) => jp_cc_nn(cpu, interconnect)?,
            (3, 4, 2, _, _) => ld_ff00_c_a(cpu, interconnect)?,
            (3, 5, 2, _, _) => ld_nn_a(cpu, interconnect)?,
            (3, 6, 2, _, _) => ld_a_ff00_c(cpu, interconnect)?,
            (3, 7, 2, _, _) => ld_a_nn(cpu, interconnect)?,

            // match on y when z = 3
            (3, 0, 3, _, _) => jp_nn(cpu, interconnect)?,
            (3, 1, 3, _, _) => return Err(CpuError::GPError),
            (3, 2...5, 3, _, _) => {}
            (3, 6, 3, _, _) => di(cpu, interconnect)?,
            (3, 7, 3, _, _) => ei(cpu, interconnect)?,

            // match on y when z = 4
            (3, 0...3, 4, _, _) => call_cc_nn(cpu, interconnect)?,

            // match on q when z = 5
            (3, _, 5, _, 0) => push_rp2(cpu, interconnect)?,
            (3, _, 5, 0, 1) => call_nn(cpu, interconnect)?,

            // z = 6, 7
            (3, _, 6, _, _) => alu_n(cpu, interconnect)?,
            (3, _, 7, _, _) => rst_y8(cpu, interconnect)?,

            // shouldn't get here, and if we do, something has gone very wrong
            _ => unreachable!(),
        }

        Ok(())
    }

    fn set_16bit_reg(&mut self, register: WordRegister, value: u16) {
        match register {
            WordRegister::AF => {
                self.registers[REG_A_INDEX] = (value >> 8) as u8;
                self.flags = (0x00FF & value) as u8;
            }
            WordRegister::BC => {
                self.registers[REG_B_INDEX] = (value >> 8) as u8;
                self.registers[REG_C_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::DE => {
                self.registers[REG_D_INDEX] = (value >> 8) as u8;
                self.registers[REG_E_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::HL => {
                self.registers[REG_H_INDEX] = (value >> 8) as u8;
                self.registers[REG_L_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::SP => {
                self.stack_pointer = value;
            }
        }
    }

    fn get_16bit_reg(&mut self, register: WordRegister) -> u16 {
        match register {
            WordRegister::AF => ((self.registers[REG_A_INDEX] as u16) << 8) | (self.flags as u16),
            WordRegister::BC => {
                ((self.registers[REG_B_INDEX] as u16) << 8) | (self.registers[REG_C_INDEX] as u16)
            }
            WordRegister::DE => {
                ((self.registers[REG_D_INDEX] as u16) << 8) | (self.registers[REG_E_INDEX] as u16)
            }
            WordRegister::HL => {
                ((self.registers[REG_H_INDEX] as u16) << 8) | (self.registers[REG_L_INDEX] as u16)
            }
            WordRegister::SP => self.stack_pointer,
        }
    }

    fn set_flags(&mut self, flags: u8) {
        self.flags |= flags;
    }

    fn clear_flags(&mut self, flags: u8) {
        self.flags &= !flags;
    }
}

fn ld_nn_sp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let addr = read_word_at_pc!(cpu, interconnect);

    interconnect.mem_write_word(addr, cpu.stack_pointer);
    Ok(())
}

fn stop(_cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Stop)
}

fn jr_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let offset = read_byte_at_pc!(cpu, interconnect) as i8;

    let mut pc = cpu.program_counter as i16;
    pc += offset as i16;

    cpu.program_counter = pc as u16;

    Ok(())
}

fn jr_cc_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = (extract_x_y_z_p_q(cpu.current_opcode).1 - 4) as usize;
    let val = read_byte_at_pc!(cpu, interconnect);
    let mut flags = cpu.flags;

    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {

        cpu.program_counter = (cpu.program_counter as i16 + (val as i8) as i16) as u16;
    }

    Ok(())
}

fn ld_rp_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let data = read_word_at_pc!(cpu, interconnect);

    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    cpu.set_16bit_reg(RP[p], data);

    Ok(())
}

fn add_hl_rp(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let (hl, xx) = (cpu.get_16bit_reg(WordRegister::HL), cpu.get_16bit_reg(RP[p]));

    let (fc, hc) = word_detect_chc(hl, xx);
    cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_SUB);
    cpu.set_flags(fc | hc);

    cpu.set_16bit_reg(WordRegister::HL, hl + xx);
    Ok(())
}

fn ld_bc_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let bc = cpu.get_16bit_reg(WordRegister::BC);

    interconnect.mem_write_byte(bc, cpu.registers[REG_A_INDEX]);

    Ok(())
}

fn ld_de_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let de = cpu.get_16bit_reg(WordRegister::DE);

    interconnect.mem_write_byte(de, cpu.registers[REG_A_INDEX]);

    Ok(())
}

fn ld_hlp_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    interconnect.mem_write_byte(hl, cpu.registers[REG_A_INDEX]);

    cpu.set_16bit_reg(WordRegister::HL, hl + 1);

    Ok(())
}

fn ld_hlm_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    interconnect.mem_write_byte(hl, cpu.registers[REG_A_INDEX]);

    cpu.set_16bit_reg(WordRegister::HL, hl - 1);

    Ok(())
}

fn ld_a_bc(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let bc = cpu.get_16bit_reg(WordRegister::BC);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(bc);

    Ok(())
}

fn ld_a_de(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let de = cpu.get_16bit_reg(WordRegister::DE);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(de);

    Ok(())
}

fn ld_a_hlp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(hl);

    cpu.set_16bit_reg(WordRegister::HL, hl + 1);

    Ok(())
}

fn ld_a_hlm(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(hl);

    cpu.set_16bit_reg(WordRegister::HL, hl - 1);

    Ok(())
}

fn inc_rp(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let val = cpu.get_16bit_reg(RP[p]);

    cpu.set_16bit_reg(RP[p], val + 1);

    Ok(())
}

fn dec_rp(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let val = cpu.get_16bit_reg(RP[p]);

    cpu.set_16bit_reg(RP[p], val - 1);

    Ok(())
}

fn inc_r(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    println!("\n\n{}\n\n", y);

    let (_, hc) = byte_detect_chc(cpu.registers[R[y]], 1);

    cpu.clear_flags(FLAG_SUB);
    cpu.set_flags(hc);

    cpu.registers[R[y]] += 1;

    if cpu.registers[R[y]] == 0 {
        cpu.set_flags(FLAG_ZERO);
    }

    Ok(())
}

fn dec_r(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    let (_, hc) = byte_sub_detect_chc(cpu.registers[R[y]], 1);

    cpu.set_flags(hc | FLAG_SUB);

    cpu.registers[R[y]] -= 1;

    if cpu.registers[R[y]] == 0 {
        cpu.set_flags(FLAG_ZERO);
    }

    Ok(())
}

fn ld_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    let val = read_byte_at_pc!(cpu, interconnect);

    cpu.registers[R[y]] = val;

    Ok(())
}

fn rlca(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    if cpu.registers[REG_A_INDEX] & 0x80 == 0x80 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX].rotate_left(1);

    Ok(())
}

fn rrca(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    if cpu.registers[REG_A_INDEX] & 0x01 == 0x01 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX].rotate_right(1);

    Ok(())
}

fn rla(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

    if cpu.registers[REG_A_INDEX] & 0x80 == 0x80 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX] << 1 | if carry { 1 } else { 0 };

    Ok(())
}

fn rra(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

    if cpu.registers[REG_A_INDEX] & 0x01 == 0x01 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX] >> 1 | if carry { 1 << 7 } else { 0 };

    Ok(())
}

fn daa(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let is_sub = (cpu.flags & FLAG_SUB) >> FLAGS_SUB_INDEX == 1;
    let _cf = cpu.flags & FLAG_CARRY == FLAG_CARRY;
    let _hcf = cpu.flags & FLAG_HALF_CARRY == FLAG_HALF_CARRY;

    if is_sub {

    } else {

    }

    unimplemented!()
}

fn cpl(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.registers[REG_A_INDEX] = !cpu.registers[REG_A_INDEX];

    Ok(())
}

fn scf(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.set_flags(FLAG_CARRY);
    cpu.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);
    Ok(())
}

fn ccf(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    if cpu.flags & FLAG_CARRY == FLAG_CARRY {
        cpu.clear_flags(FLAG_CARRY);
    } else {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);

    Ok(())
}

fn ld_r_r(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(cpu.current_opcode);

    cpu.registers[R[y as usize]] = cpu.registers[R[z as usize]];

    Ok(())
}

fn halt(_cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Halt)
}

fn alu_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(cpu.current_opcode);

    let mut reg_val = cpu.registers[R[z as usize]];
    if R[z as usize] == 0b110 {
        reg_val = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
    }

    match y {
        // ADD A, R[z]
        0 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
            cpu.set_flags(fc | hc);

            cpu.registers[REG_A_INDEX] += reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }

        }

        // ADC A, R[z]
        1 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
            cpu.set_flags(fc | hc);

            let (val, of) = cpu.registers[REG_A_INDEX].overflowing_add(reg_val);
            cpu.registers[REG_A_INDEX] = val;

            if of {
                cpu.registers[REG_A_INDEX] += 1;
            }

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        }

        // SUB R[z]
        2 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            cpu.registers[REG_A_INDEX] -= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // SBC A, R[z]
        3 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            let (val, of) = cpu.registers[REG_A_INDEX].overflowing_sub(reg_val);
            cpu.registers[REG_A_INDEX] = val;

            if of {
                cpu.registers[REG_A_INDEX] -= 1;
            }

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        }

        // AND R[z]
        4 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY);
            cpu.set_flags(FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] &= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // XOR R[z]
        5 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] ^= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // OR R[z]
        6 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] |= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // CP R[z]
        7 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            if cpu.registers[REG_A_INDEX] - reg_val == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }
        _ => unreachable!(),
    };

    Ok(())
}

fn ret_cc(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    let mut flags = cpu.flags;

    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {
        let val = pop16(cpu, interconnect);
        cpu.program_counter = val;
    }

    Ok(())
}

fn ld_ff00_nn_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = read_word_at_pc!(cpu, interconnect);

    interconnect.mem_write_byte(0xFF00 + nn, cpu.registers[REG_A_INDEX]);

    Ok(())
}

fn add_sp_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let d = read_byte_at_pc!(cpu, interconnect);

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB);

    let (fc, hc) = word_addsigned_detect_chc(cpu.stack_pointer as i16, d as i16);

    cpu.set_flags(fc | hc);

    cpu.stack_pointer = (cpu.stack_pointer as i16 + d as i16) as u16;

    Ok(())
}

fn ld_a_ff00_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = read_word_at_pc!(cpu, interconnect);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(0xFF00 + nn);

    Ok(())
}

fn ld_hl_sp_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let d = read_byte_at_pc!(cpu, interconnect);
    let sp = cpu.stack_pointer;
    cpu.clear_flags(FLAG_ZERO | FLAG_SUB);

    let (fc, hc) = word_addsigned_detect_chc(cpu.stack_pointer as i16, d as i16);

    cpu.set_flags(fc | hc);

    cpu.set_16bit_reg(WordRegister::HL,
                      (sp as i16 + d as i16) as u16);

    Ok(())
}

fn pop_rp2(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let val = pop16(cpu, interconnect);

    cpu.set_16bit_reg(RP2[p], val);

    Ok(())
}

fn ret(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = pop16(cpu, interconnect);

    Ok(())
}

fn reti(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = pop16(cpu, interconnect);
    // TODO: Interrupt stuff
    Ok(())
}

fn jp_hl(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = cpu.get_16bit_reg(WordRegister::HL);

    Ok(())
}

fn ld_sp_hl(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.stack_pointer = cpu.get_16bit_reg(WordRegister::HL);

    Ok(())
}

fn jp_cc_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;
    let val = read_word_at_pc!(cpu, interconnect);
    let mut flags = cpu.flags;

    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {
        cpu.program_counter = val;
    }

    Ok(())
}

fn ld_ff00_c_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    interconnect.mem_write_byte(0xFF00 + cpu.registers[REG_C_INDEX] as u16,
                                cpu.registers[REG_A_INDEX]);

    Ok(())
}

fn ld_nn_a(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = read_word_at_pc!(cpu, interconnect);

    interconnect.mem_write_byte(nn, cpu.registers[REG_A_INDEX]);

    Ok(())
}

fn ld_a_ff00_c(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.registers[REG_A_INDEX] =
        interconnect.mem_read_byte(0xFF00 + cpu.registers[REG_C_INDEX] as u16);

    Ok(())
}

fn ld_a_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = read_word_at_pc!(cpu, interconnect);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(nn);

    Ok(())
}

fn jp_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let val = read_word_at_pc!(cpu, interconnect);

    cpu.program_counter = val;

    Ok(())
}

fn di(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.interrupts_enabled = false;

    Ok(())
}

fn ei(cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.interrupts_enabled = true;

    Ok(())
}

fn call_cc_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;
    let val = read_word_at_pc!(cpu, interconnect);
    let mut flags = cpu.flags;
    let pc = cpu.program_counter;
    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {
        push16(cpu, interconnect, pc);
        cpu.program_counter = val;
    }

    Ok(())
}

fn push_rp2(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;
    let val = cpu.get_16bit_reg(RP2[p]);

    push16(cpu, interconnect, val);

    Ok(())
}

fn call_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let val = read_word_at_pc!(cpu, interconnect);
    let pc = cpu.program_counter;

    push16(cpu, interconnect, pc);
    cpu.program_counter = val;

    Ok(())
}

fn alu_n(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(cpu.current_opcode).1;

    let reg_val = read_byte_at_pc!(cpu, interconnect);

    match y {
        // ADD A, d8
        0 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
            cpu.set_flags(fc | hc);

            cpu.registers[REG_A_INDEX] += reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }

        }

        // ADC A, d8
        1 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO | FLAG_SUB);
            cpu.set_flags(fc | hc);

            let (val, of) = cpu.registers[REG_A_INDEX].overflowing_add(reg_val);
            cpu.registers[REG_A_INDEX] = val;

            if of {
                cpu.registers[REG_A_INDEX] += 1;
            }

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        }

        // SUB d8
        2 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            cpu.registers[REG_A_INDEX] -= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // SBC A, d8
        3 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            let (val, of) = cpu.registers[REG_A_INDEX].overflowing_sub(reg_val);
            cpu.registers[REG_A_INDEX] = val;

            if of {
                cpu.registers[REG_A_INDEX] -= 1;
            }

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        }

        // AND d8
        4 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY);
            cpu.set_flags(FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] &= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // XOR d8
        5 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] ^= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // OR d8
        6 => {
            cpu.clear_flags(FLAG_SUB | FLAG_CARRY | FLAG_HALF_CARRY);

            cpu.registers[REG_A_INDEX] |= reg_val;

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }

        // CP d8
        7 => {
            let (fc, hc) = byte_sub_detect_chc(cpu.registers[REG_A_INDEX], reg_val);

            cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_ZERO);
            cpu.set_flags(fc | hc | FLAG_SUB);

            if cpu.registers[REG_A_INDEX] - reg_val == 0 {
                cpu.set_flags(FLAG_ZERO);
            }
        }
        _ => unreachable!(),
    };

    Ok(())
}

fn rst_y8(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(cpu.current_opcode).1;
    let pc = cpu.program_counter;

    push16(cpu, interconnect, pc);
    cpu.program_counter = (y * 8) as u16;

    Ok(())
}

fn push16(cpu: &mut Cpu, interconnect: &mut Interconnect, val: u16) {
    push8(cpu, interconnect, (val & 0xFF00 >> 8) as u8);
    push8(cpu, interconnect, (val & 0x00FF) as u8);
}

fn pop16(cpu: &mut Cpu, interconnect: &mut Interconnect) -> u16 {
    (pop8(cpu, interconnect) as u16) << 8 | (pop8(cpu, interconnect) as u16)
}

fn pop8(cpu: &mut Cpu, interconnect: &mut Interconnect) -> u8 {
    let ret = interconnect.mem_read_byte(cpu.stack_pointer);
    cpu.stack_pointer += 1;
    ret
}

fn push8(cpu: &mut Cpu, interconnect: &mut Interconnect, val: u8) {
    interconnect.mem_write_byte(cpu.stack_pointer, val);
    cpu.stack_pointer -= 1;
}

fn word_detect_chc(first: u16, second: u16) -> (u8, u8) {
    let carry_in = (((first & 0x07FF) + (second & 0x07FF)) & 0x0800) >> 11;
    let carry_out = ((first + second) & 0x1000) >> 12;

    let mut result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = FLAG_CARRY;
    }

    let (_, full_carry) = first.overflowing_add(second);

    if full_carry {
        result.0 = FLAG_HALF_CARRY;
    }

    result
}

fn byte_detect_chc(first: u8, second: u8) -> (u8, u8) {
    let carry_in = (((first & 0x07) + (second & 0x07)) & 0x08) >> 3;
    let carry_out = ((first + second) & 0x10) >> 4;

    let mut result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = FLAG_CARRY;
    }

    let (_, full_carry) = first.overflowing_add(second);

    if full_carry {
        result.0 = FLAG_HALF_CARRY;
    }

    result
}

fn word_sub_detect_chc(first: u16, second: u16) -> (u8, u8) {
    let carry_in = (((first & 0x07FF) - (second & 0x07FF)) & 0x0800) >> 11;
    let carry_out = ((first - second) & 0x1000) >> 12;

    let mut result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = FLAG_CARRY;
    }

    let (_, full_carry) = first.overflowing_sub(second);

    if full_carry {
        result.0 = FLAG_HALF_CARRY;
    }

    result
}

fn byte_sub_detect_chc(first: u8, second: u8) -> (u8, u8) {
    let carry_in = (((first & 0x07) - (second & 0x07)) & 0x08) >> 3;
    let carry_out = ((first - second) & 0x10) >> 4;

    let mut result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = FLAG_CARRY;
    }

    let (_, full_carry) = first.overflowing_sub(second);

    if full_carry {
        result.0 = FLAG_HALF_CARRY;
    }

    result
}

fn word_addsigned_detect_chc(first: i16, second: i16) -> (u8, u8) {
    let carry_in = (((first & 0x07FF) + (second & 0x07FF)) & 0x0800) >> 11;
    let carry_out = ((first + second) & 0x1000) >> 12;

    let mut result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = FLAG_CARRY;
    }

    let (_, full_carry) = first.overflowing_add(second);

    if full_carry {
        result.0 = FLAG_HALF_CARRY;
    }

    result
}

pub fn extract_x_y_z_p_q(opcode: u8) -> (u8, u8, u8, u8, u8) {
    let (x, y, z) = ((opcode & 0b11000000) >> 6, (opcode & 0b00111000) >> 3, opcode & 0b00000111);

    let (p, q) = ((y & 0b00000110) >> 1, y & 0b00000001);

    (x, y, z, p, q)
}
