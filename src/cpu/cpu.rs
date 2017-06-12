//use cpu::instruction::{Instruction, Operation};
use interconnect::Interconnect;
use cpu::cpuconsts::*;

pub struct Cpu {
    registers: [u8; 8],
    flags: u8,
    stack_pointer: u16,
    program_counter: u16,
    current_opcode: u8
}

pub enum CpuError {
    GPError,
    MemRWError,
    Stop,
    Halt,
    UnknownOpcode
}

enum WordRegister {
    AF,
    BC,
    DE,
    HL,
    SP
}

static r: [usize; 8] = [ REG_B_INDEX,
                      REG_C_INDEX,
                      REG_D_INDEX,
                      REG_E_INDEX,
                      REG_H_INDEX,
                      REG_L_INDEX,
                      0b110, // HL
                      REG_A_INDEX ];

static rp: [WordRegister; 4] = [ WordRegister::BC, 
                                 WordRegister::DE,
                                 WordRegister::HL,
                                 WordRegister::SP ];

static rp2: [WordRegister; 4] = [ WordRegister::BC,
                                  WordRegister::DE,
                                  WordRegister::HL,
                                  WordRegister::AF ];

impl Cpu {

    fn execute_instruction(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

        let opcode = cpu.current_opcode;

        let (x, y, z, p, q) = extract_x_y_z_p_q(opcode);

        if x == 0 {
            match z {
                0 => match y {
                        0 => { },
                        1 => ld_nn_sp(cpu, interconnect)?,
                        2 => stop(cpu, interconnect)?,
                        3 => jr_d(cpu, interconnect)?,
                        4 ... 7 => jr_cc_d(cpu, interconnect)?,
                        _ => unreachable!()
                },
                
                1 => match q {
                    0 => ld_rp_nn(cpu, interconnect)?,
                    1 => add_hl_rp(cpu, interconnect)?,
                    _ => unreachable!()
                },

                2 => match q {
                    0 => match p {
                        0 => ld_bc_a(cpu, interconnect)?,
                        1 => ld_de_a(cpu, interconnect)?,
                        2 => ld_hlp_a(cpu, interconnect)?,
                        3 => ld_hlm_a(cpu, interconnect)?,
                        _ => unreachable!()
                    },
                    1 => match p {
                        0 => ld_a_bc(cpu, interconnect)?,
                        1 => ld_a_de(cpu, interconnect)?,
                        2 => ld_a_hlp(cpu, interconnect)?,
                        3 => ld_a_hlm(cpu, interconnect)?,
                        _ => unreachable!()
                    },
                    _ => unreachable!()
                },

                3 => match q {
                    0 => inc_rp(cpu, interconnect)?,
                    1 => dec_rp(cpu, interconnect)?,
                    _ => unreachable!()
                },

                4 => inc_r(cpu, interconnect)?,
                5 => dec_r(cpu, interconnect)?,
                6 => ld_r(cpu, interconnect)?,

                7 => match y {
                    0 => rlca(cpu, interconnect)?,
                    1 => rrca(cpu, interconnect)?,
                    2 => rla(cpu, interconnect)?,
                    3 => rra(cpu, interconnect)?,
                    4 => daa(cpu, interconnect)?,
                    5 => cpl(cpu, interconnect)?,
                    6 => scf(cpu, interconnect)?,
                    7 => ccf(cpu, interconnect)?,
                    _ => unreachable!()
                },

                _ => unreachable!()
            };
        } else if x == 1 {
            match z {
                0 ... 5 => ld_r_r(cpu, interconnect)?,
                6 => halt(cpu, interconnect)?,
                _ => unreachable!()
            };
        } else if x == 2 {
            alu_r(cpu, interconnect)?;
        } else if x == 3 {
            match z {
                0 => match y {
                    0 ... 3 => ret_cc(cpu, interconnect)?,
                    4 => ld_ff00_nn_a(cpu, interconnect)?,
                    5 => add_sp_d(cpu, interconnect)?,
                    6 => ld_a_ff00_nn(cpu, interconnect)?,
                    7 => ld_hl_sp_d(cpu, interconnect)?,
                    _ => unreachable!()
                },
                1 => match q {
                    0 => pop_rp2(cpu, interconnect)?,
                    1 => match p {
                        0 => ret(cpu, interconnect)?,
                        1 => reti(cpu, interconnect)?,
                        2 => jp_hl(cpu, interconnect)?,
                        3 => ld_sp_hl(cpu, interconnect)?,
                        _ => unreachable!()
                    }
                },
                2 => match y {
                    0 ... 3 => jp_cc_nn(cpu, interconnect)?,
                    4 => ld_ff00_c_a(cpu, interconnect)?,
                    5 => ld_nn_a(cpu, interconnect)?,
                    6 => ld_a_ff00_c(cpu, interconnect)?,
                    7 => ld_a_nn(cpu, interconnect)?,
                    _ => unreachable!()
                },
                3 => match y {
                    0 => jp_nn(cpu, interconnect)?,
                    1 => return Err(CpuError::GPError),
                    2 ... 5 => { },
                    6 => di(cpu, interconnect)?,
                    7 => ei(cpu, interconnect)?,
                    _ => unreachable!()
                },
                4 => match y {
                    0 ... 3 => call_cc_nn(cpu, interconnect)?,
                    _ => unreachable!()
                },
                5 => match q {
                    0 => push_rp2(cpu, interconnect)?,
                    1 => match p {
                        0 => call_nn(cpu, interconnect)?,
                        _ => unreachable!()
                    }
                },
                6 => alu_n(cpu, interconnect)?,
                7 => rst_y8(cpu, interconnect)?,
            }
        }

        Ok(())
    }

    fn set_16bit_reg(&mut self, register: WordRegister, value: u16) {
        match register {
            WordRegister::AF => { 
                self.registers[REG_A_INDEX] = (value >> 8) as u8; 
                self.flags = (0x00FF & value) as u8;
            },
            WordRegister::BC => {
                self.registers[REG_B_INDEX] = (value >> 8) as u8;
                self.registers[REG_C_INDEX] = (0x00FF & value) as u8;
            },
            WordRegister::DE => {
                self.registers[REG_D_INDEX] = (value >> 8) as u8;
                self.registers[REG_E_INDEX] = (0x00FF & value) as u8;
            },
            WordRegister::HL => {
                self.registers[REG_H_INDEX] = (value >> 8) as u8;
                self.registers[REG_L_INDEX] = (0x00FF & value) as u8;
            },
            WordRegister::SP => {
                self.stack_pointer = value;
            }
        }
    }

    fn get_16bit_reg(&mut self, register: WordRegister) -> u16 {
        match register {
            WordRegister::AF => { 
                ((self.registers[REG_A_INDEX] as u16) << 8) |
                (self.flags as u16)
            },
            WordRegister::BC => {
                ((self.registers[REG_B_INDEX] as u16) << 8) |
                (self.registers[REG_C_INDEX] as u16)
            },
            WordRegister::DE => {
                ((self.registers[REG_D_INDEX] as u16) << 8) |
                (self.registers[REG_E_INDEX] as u16)
            },
            WordRegister::HL => {
                ((self.registers[REG_H_INDEX] as u16) << 8) |
                (self.registers[REG_L_INDEX] as u16)
            },
            WordRegister::SP => {
                self.stack_pointer
            }
        }
    }
}

fn ld_nn_sp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let addr = interconnect.mem_read_word(cpu.program_counter);
    cpu.program_counter += 2;

    interconnect.mem_write_word(addr, cpu.stack_pointer);
    Ok(())
}

fn stop(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Stop)
}

fn jr_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let offset = interconnect.mem_read_byte(cpu.program_counter) as i8 - 2;
    cpu.program_counter += 1;
    
    let pc = cpu.program_counter as i16;
    pc += offset as i16;

    cpu.program_counter = pc as u16;

    Ok(())
}

fn jr_cc_d(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn ld_rp_nn(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let data = interconnect.mem_read_word(cpu.program_counter);
    cpu.program_counter += 2;
    
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    cpu.set_16bit_reg(rp[p], data);

    Ok(())
}

fn add_hl_rp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let (hl, xx) = (cpu.get_16bit_reg(WordRegister::HL),
                    cpu.get_16bit_reg(rp[p]));

    let (fc, hc) = word_detect_chc(hl, xx);
    cpu.flags &= !(1 << FLAGS_HALF_CARRY + 1 << FLAGS_CARRY);
    cpu.flags |= fc | hc;
    cpu.flags &= !(1 << FLAGS_SUB_INDEX);
    
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

fn inc_rp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let val = cpu.get_16bit_reg(rp[p]);

    cpu.set_16bit_reg(rp[p], val + 1);

    Ok(())
}

fn dec_rp(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(cpu.current_opcode).3 as usize;

    let val = cpu.get_16bit_reg(rp[p]);

    cpu.set_16bit_reg(rp[p], val - 1);

    Ok(())
}

fn inc_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    cpu.registers[r[y]] += 1;

    Ok(())
}

fn dec_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    cpu.registers[r[y]] -= 1;

    Ok(())
}

fn ld_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(cpu.current_opcode).1 as usize;

    let val = interconnect.mem_read_byte(cpu.program_counter);
    cpu.program_counter += 1;

    cpu.registers[r[y]] = val;

    Ok(())
}

fn rlca(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn rrca(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn rla(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn rra(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn daa(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn cpl(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn scf(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn ccf(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    unimplemented!();

    Ok(())
}

fn ldr_r_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(cpu.current_opcode);

    cpu.registers[r[y as usize]] = cpu.registers[r[z as usize]];

    Ok(())
}

fn halt(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Halt)
}

fn alu_r(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(cpu.current_opcode);

    match y {
        0 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], cpu.registers[r[z as usize]]);
            cpu.flags &= !(1 << FLAGS_HALF_CARRY + 1 << FLAGS_CARRY + 1 << FLAGS_ZERO_INDEX);
            cpu.flags |= fc | hc;
            cpu.flags &= !(1 << FLAGS_SUB_INDEX);
            cpu.registers[REG_A_INDEX] += cpu.registers[r[z as usize]];

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        },
        1 => {
            let (fc, hc) = byte_detect_chc(cpu.registers[REG_A_INDEX], cpu.registers[r[z as usize]]);
            cpu.flags &= !(1 << FLAGS_HALF_CARRY + 1 << FLAGS_CARRY + 1 << FLAGS_ZERO_INDEX);
            cpu.flags |= fc | hc;
            cpu.flags &= !(1 << FLAGS_SUB_INDEX);
            let (val, of) = cpu.registers[REG_A_INDEX].overflowing_add(cpu.registers[r[z as usize]]);
            cpu.registers[REG_A_INDEX] = val;
            
            if of {
                cpu.registers[REG_A_INDEX] += 1;
            }

            if cpu.registers[REG_A_INDEX] == 0 {
                cpu.flags |= 1 << FLAGS_ZERO_INDEX;
            }
        },
        2 => {

        },
        _ => unreachable!()
    };

    Ok(())
}

fn word_detect_chc(first: u16, second: u16) -> (u8, u8) {
    let carry_in = (((first & 0x07FF) + (second & 0x07FF)) &
                                0x0800) >> 11;
    let carry_out = ((first + second) & 0x1000) >> 12;

    let result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = 1 << FLAGS_CARRY;
    }

    let (_, full_carry) = first.overflowing_add(second);

    if full_carry {
        result.0 = 1 << FLAGS_HALF_CARRY;
    }

    result
}

fn byte_detect_chc(first: u8, second: u8) -> (u8, u8) {
    let carry_in = (((first & 0x07) + (second & 0x07)) &
                                0x08) >> 3;
    let carry_out = ((first + second) & 0x10) >> 4;

    let result = (0u8, 0u8);

    if carry_in ^ carry_out == 1 {
        result.1 = 1 << FLAGS_CARRY;
    }

    let (_, full_carry) = first.overflowing_add(second);

    if full_carry {
        result.0 = 1 << FLAGS_HALF_CARRY;
    }

    result
}

fn extract_x_y_z_p_q(opcode: u8) -> (u8, u8, u8, u8, u8) {
    let (x, y, z) = (opcode & 0b11000000 >> 6,
                     opcode & 0b00111000 >> 3,
                     opcode & 0b00000111);
    
    let (p, q) = (y & 0b00000110 >> 1,
                  y & 0b00000001);

    (x, y, z, p, q)
}