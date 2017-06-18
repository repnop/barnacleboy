use cpu::cpu::*;
use interconnect::Interconnect;
use cpu::cpuconsts::*;
use cpu::instruction::Instruction;

pub fn ld_nn_sp(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let addr = ins.get_word();

    interconnect.mem_write_word(addr, cpu.stack_pointer);
    Ok(())
}

pub fn stop(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Stop)
}

pub fn jr_d(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let offset = ins.get_byte() as i8;

    let mut pc = cpu.program_counter as i16;
    pc += offset as i16;

    cpu.program_counter = pc as u16;

    Ok(())
}

pub fn jr_cc_d(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = (extract_x_y_z_p_q(ins.opcode()).1 - 4) as usize;
    let val = ins.get_byte();
    let mut flags = cpu.flags;

    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {

        cpu.program_counter = (cpu.program_counter as i16 + (val as i8) as i16) as u16;
    }

    Ok(())
}

pub fn ld_rp_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let data = ins.get_word();

    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;

    cpu.set_16bit_reg(RP[p], data);

    Ok(())
}

pub fn add_hl_rp(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;

    let (hl, xx) = (cpu.get_16bit_reg(WordRegister::HL), cpu.get_16bit_reg(RP[p]));

    let (fc, hc) = word_detect_chc(hl, xx);
    cpu.clear_flags(FLAG_CARRY | FLAG_HALF_CARRY | FLAG_SUB);
    cpu.set_flags(fc | hc);

    cpu.set_16bit_reg(WordRegister::HL, hl + xx);
    Ok(())
}

pub fn ld_bc_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let bc = cpu.get_16bit_reg(WordRegister::BC);

    interconnect.mem_write_byte(bc, cpu.registers[REG_A_INDEX]);

    Ok(())
}

pub fn ld_de_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let de = cpu.get_16bit_reg(WordRegister::DE);

    interconnect.mem_write_byte(de, cpu.registers[REG_A_INDEX]);

    Ok(())
}

pub fn ld_hlp_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    interconnect.mem_write_byte(hl, cpu.registers[REG_A_INDEX]);

    cpu.set_16bit_reg(WordRegister::HL, hl + 1);

    Ok(())
}

pub fn ld_hlm_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    interconnect.mem_write_byte(hl, cpu.registers[REG_A_INDEX]);

    cpu.set_16bit_reg(WordRegister::HL, hl - 1);

    Ok(())
}

pub fn ld_a_bc(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let bc = cpu.get_16bit_reg(WordRegister::BC);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(bc);

    Ok(())
}

pub fn ld_a_de(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let de = cpu.get_16bit_reg(WordRegister::DE);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(de);

    Ok(())
}

pub fn ld_a_hlp(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(hl);

    cpu.set_16bit_reg(WordRegister::HL, hl + 1);

    Ok(())
}

pub fn ld_a_hlm(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let hl = cpu.get_16bit_reg(WordRegister::HL);

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(hl);

    cpu.set_16bit_reg(WordRegister::HL, hl - 1);

    Ok(())
}

pub fn inc_rp(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;

    let val = cpu.get_16bit_reg(RP[p]);

    cpu.set_16bit_reg(RP[p], val + 1);

    Ok(())
}

pub fn dec_rp(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;

    let val = cpu.get_16bit_reg(RP[p]);

    cpu.set_16bit_reg(RP[p], val - 1);

    Ok(())
}

pub fn inc_r(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;

    let (_, hc) = byte_detect_chc(cpu.registers[R[y]], 1);

    cpu.clear_flags(FLAG_SUB | FLAG_ZERO);
    cpu.set_flags(hc);

    cpu.registers[R[y]] += 1;

    if cpu.registers[R[y]] == 0 {
        cpu.set_flags(FLAG_ZERO);
    }

    Ok(())
}

pub fn dec_r(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;

    let (_, hc) = byte_sub_detect_chc(cpu.registers[R[y]], 1);

    cpu.set_flags(hc | FLAG_SUB);
    cpu.clear_flags(FLAG_ZERO);

    cpu.registers[R[y]] -= 1;

    if cpu.registers[R[y]] == 0 {
        cpu.set_flags(FLAG_ZERO);
    }

    Ok(())
}

pub fn ld_r(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;

    let val = ins.get_byte();

    cpu.registers[R[y]] = val;

    Ok(())
}

pub fn rlca(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    if cpu.registers[REG_A_INDEX] & 0x80 == 0x80 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX].rotate_left(1);

    Ok(())
}

pub fn rrca(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    if cpu.registers[REG_A_INDEX] & 0x01 == 0x01 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX].rotate_right(1);

    Ok(())
}

pub fn rla(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

    if cpu.registers[REG_A_INDEX] & 0x80 == 0x80 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX] << 1 | if carry { 1 } else { 0 };

    Ok(())
}

pub fn rra(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

    if cpu.registers[REG_A_INDEX] & 0x01 == 0x01 {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

    cpu.registers[REG_A_INDEX] = cpu.registers[REG_A_INDEX] >> 1 | if carry { 1 << 7 } else { 0 };

    Ok(())
}

pub fn daa(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let is_sub = (cpu.flags & FLAG_SUB) >> FLAGS_SUB_INDEX == 1;
    let _cf = cpu.flags & FLAG_CARRY == FLAG_CARRY;
    let _hcf = cpu.flags & FLAG_HALF_CARRY == FLAG_HALF_CARRY;

    if is_sub {

    } else {

    }

    unimplemented!()
}

pub fn cpl(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.registers[REG_A_INDEX] = !cpu.registers[REG_A_INDEX];

    Ok(())
}

pub fn scf(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.set_flags(FLAG_CARRY);
    cpu.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);
    Ok(())
}

pub fn ccf(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    if cpu.flags & FLAG_CARRY == FLAG_CARRY {
        cpu.clear_flags(FLAG_CARRY);
    } else {
        cpu.set_flags(FLAG_CARRY);
    }

    cpu.clear_flags(FLAG_SUB | FLAG_HALF_CARRY);

    Ok(())
}

pub fn ld_r_r(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(ins.opcode());

    if y == 0b110 {
        interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), cpu.registers[R[z as usize]]);
    } else {
        cpu.registers[R[y as usize]] = cpu.registers[R[z as usize]];
    }

    Ok(())
}

pub fn halt(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    Err(CpuError::Halt)
}

pub fn alu_r(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (_, y, z, _, _) = extract_x_y_z_p_q(ins.opcode());

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

pub fn ret_cc(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;

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

pub fn ld_ff00_n_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = ins.get_byte();

    interconnect.mem_write_byte(0xFF00 + nn as u16, cpu.registers[REG_A_INDEX]);

    Ok(())
}

pub fn add_sp_d(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let d = ins.get_byte();

    cpu.clear_flags(FLAG_ZERO | FLAG_SUB);

    let (fc, hc) = word_addsigned_detect_chc(cpu.stack_pointer as i16, d as i16);

    cpu.set_flags(fc | hc);

    cpu.stack_pointer = (cpu.stack_pointer as i16 + d as i16) as u16;

    Ok(())
}

pub fn ld_a_ff00_n(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = ins.get_byte();

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(0xFF00 + nn as u16);

    Ok(())
}

pub fn ld_hl_sp_d(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let d = ins.get_byte();
    let sp = cpu.stack_pointer;
    cpu.clear_flags(FLAG_ZERO | FLAG_SUB);

    let (fc, hc) = word_addsigned_detect_chc(cpu.stack_pointer as i16, d as i16);

    cpu.set_flags(fc | hc);

    cpu.set_16bit_reg(WordRegister::HL,
                      (sp as i16 + d as i16) as u16);

    Ok(())
}

pub fn pop_rp2(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {

    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;

    let val = pop16(cpu, interconnect);

    cpu.set_16bit_reg(RP2[p], val);

    Ok(())
}

pub fn ret(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = pop16(cpu, interconnect);

    Ok(())
}

pub fn reti(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = pop16(cpu, interconnect);
    // TODO: Interrupt stuff
    Ok(())
}

pub fn jp_hl(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.program_counter = cpu.get_16bit_reg(WordRegister::HL);

    Ok(())
}

pub fn ld_sp_hl(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.stack_pointer = cpu.get_16bit_reg(WordRegister::HL);

    Ok(())
}

pub fn jp_cc_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;
    let val = ins.get_word();
    let mut flags = cpu.flags;

    if y % 2 == 0 {
        flags = !flags;
    }

    if flags & CC[y] == CC[y] {
        cpu.program_counter = val;
    }

    Ok(())
}

pub fn ld_ff00_c_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    interconnect.mem_write_byte(0xFF00 + cpu.registers[REG_C_INDEX] as u16,
                                cpu.registers[REG_A_INDEX]);

    Ok(())
}

pub fn ld_nn_a(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = ins.get_word();

    interconnect.mem_write_byte(nn, cpu.registers[REG_A_INDEX]);

    Ok(())
}

pub fn ld_a_ff00_c(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.registers[REG_A_INDEX] =
        interconnect.mem_read_byte(0xFF00 + cpu.registers[REG_C_INDEX] as u16);

    Ok(())
}

pub fn ld_a_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let nn = ins.get_word();

    cpu.registers[REG_A_INDEX] = interconnect.mem_read_byte(nn);

    Ok(())
}

pub fn jp_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let val = ins.get_word();

    cpu.program_counter = val;

    Ok(())
}

pub fn di(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.interrupts_enabled = false;

    Ok(())
}

pub fn ei(ins: &Instruction, cpu: &mut Cpu, _interconnect: &mut Interconnect) -> Result<(), CpuError> {
    cpu.interrupts_enabled = true;

    Ok(())
}

pub fn call_cc_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(ins.opcode()).1 as usize;
    let val = ins.get_word();
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

pub fn push_rp2(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let p = extract_x_y_z_p_q(ins.opcode()).3 as usize;
    let val = cpu.get_16bit_reg(RP2[p]);

    push16(cpu, interconnect, val);

    Ok(())
}

pub fn call_nn(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let val = ins.get_word();
    let pc = cpu.program_counter;

    push16(cpu, interconnect, pc);
    cpu.program_counter = val;

    Ok(())
}

pub fn alu_n(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(ins.opcode()).1;

    let reg_val = ins.get_byte();

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

pub fn rst_y8(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let y = extract_x_y_z_p_q(ins.opcode()).1;
    let pc = cpu.program_counter;

    push16(cpu, interconnect, pc);
    cpu.program_counter = (y * 8) as u16;

    Ok(())
}

pub fn prefixed(ins: &Instruction, cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<(), CpuError> {
    let (x, y, z, _, _) = extract_x_y_z_p_q(ins.opcode());

    match x {
        0 => match y {
            0 => {
                if z != 0b110 {
                    if cpu.registers[R[z as usize]] & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    cpu.registers[R[z as usize]] = cpu.registers[R[z as usize]].rotate_left(1);
                } else {
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    if hl & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    if hl.rotate_left(1) == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), hl.rotate_left(1));
                }
            },
            1 => {
                if z != 0b110 {
                    if cpu.registers[R[z as usize]] & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    cpu.registers[R[z as usize]] = cpu.registers[R[z as usize]].rotate_right(1);
                } else {
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    if hl & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    if hl.rotate_right(1) == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), hl.rotate_right(1));
                }
            },
            2 => {
                if z != 0b110 {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

                    if cpu.registers[R[z as usize]] & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = cpu.registers[R[z as usize]] << 1 | if carry { 1 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    cpu.registers[R[z as usize]] = val;
                } else {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    if hl & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = hl << 1 | if carry { 1 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), val);
                }
            },
            3 => {
                if z != 0b110 {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

                    if cpu.registers[R[z as usize]] & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = cpu.registers[R[z as usize]] >> 1 | if carry { 1 << 7 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    cpu.registers[R[z as usize]] = val;
                } else {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    if hl & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = hl >> 1 | if carry { 1 << 7 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), val);
                }
            },
            4 => {
                if z != 0b110 {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;

                    if cpu.registers[R[z as usize]] & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = cpu.registers[R[z as usize]] << 1;

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    cpu.registers[R[z as usize]] = val;
                } else {
                    let carry = cpu.flags & FLAG_CARRY == FLAG_CARRY;
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    if hl & 0x80 == 0x80 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = hl << 1;

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), val);
                }
            },
            5 => {
                if z != 0b110 {
                    let carry = cpu.registers[R[z as usize]] & 0x80 == 0x80;

                    if cpu.registers[R[z as usize]] & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = cpu.registers[R[z as usize]] >> 1 | if carry { 0x80 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    cpu.registers[R[z as usize]] = val;
                } else {
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    let carry = hl & 0x80 == 0x80;
                    if hl & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = hl >> 1 | if carry { 0x80 } else { 0 };

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), val);
                }
            },
            6 => {
                if z != 0b110 {
                    let top = cpu.registers[R[z as usize]] & 0xF0;
                    let bot = cpu.registers[R[z as usize]] & 0x0F;

                    cpu.registers[R[z as usize]] = (bot << 4) | (top >> 4);

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY | FLAG_CARRY);

                    if cpu.registers[R[z as usize]] == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                } else {
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));
                    let top = hl & 0xF0;
                    let bot = hl & 0x0F;

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), (bot << 4) | (top >> 4));

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY | FLAG_CARRY);

                    if (bot << 4) | (top >> 4) == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }
                }
            },
            7 => {
                if z != 0b110 {

                    if cpu.registers[R[z as usize]] & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = cpu.registers[R[z as usize]] >> 1;

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    cpu.registers[R[z as usize]] = val;
                } else {
                    let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));

                    if hl & 0x01 == 0x01 {
                        cpu.set_flags(FLAG_CARRY);
                    }

                    cpu.clear_flags(FLAG_ZERO | FLAG_SUB | FLAG_HALF_CARRY);

                    let val = hl >> 1;

                    if val == 0 {
                        cpu.set_flags(FLAG_ZERO);
                    }

                    interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), val);
                }
            },
            _ => unreachable!()
        },
        1 => {
            if z != 0b110 {
                let result = (1 << y) & cpu.registers[R[z as usize]];

                cpu.clear_flags(FLAG_SUB | FLAG_ZERO);
                cpu.set_flags(FLAG_HALF_CARRY);

                if result == 0 {
                    cpu.set_flags(FLAG_ZERO);
                }
            } else {
                let result = (1 << y) & interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));

                cpu.clear_flags(FLAG_SUB | FLAG_ZERO);
                cpu.set_flags(FLAG_HALF_CARRY);

                if result == 0 {
                    cpu.set_flags(FLAG_ZERO);
                }
            }
        },
        2 => {
            if z != 0b110 {
                cpu.registers[R[z as usize]] &= !(1 << y);
            } else {
                let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));

                interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), hl & !(1 << y));
            }
        }
        3 => {
            if z != 0b110 {
                cpu.registers[R[z as usize]] |= (1 << y);
            } else {
                let hl = interconnect.mem_read_byte(cpu.get_16bit_reg(WordRegister::HL));

                interconnect.mem_write_byte(cpu.get_16bit_reg(WordRegister::HL), hl | (1 << y));
            }
        },
        _ => unreachable!()
    };

    Ok(())
}

pub fn push16(cpu: &mut Cpu, interconnect: &mut Interconnect, val: u16) {
    interconnect.mem_write_byte(cpu.stack_pointer - 2, ((0xFF00 & val) >> 8) as u8);
    interconnect.mem_write_byte(cpu.stack_pointer - 1, (0x00FF & val) as u8);

    cpu.stack_pointer -= 2;
}

pub fn pop16(cpu: &mut Cpu, interconnect: &mut Interconnect) -> u16 {
    let low = interconnect.mem_read_byte(cpu.stack_pointer);
    let high = interconnect.mem_read_byte(cpu.stack_pointer + 1);

    cpu.stack_pointer += 2;

    ((low as u16) << 8) & 0xFF00 | (0x00FF & high as u16)
}

pub fn word_detect_chc(first: u16, second: u16) -> (u8, u8) {
    let carry_in = ((first & 0x07FF).overflowing_add(second & 0x07FF).0 & 0x0800) >> 11;
    let carry_out = (first.overflowing_add(second).0 & 0x1000) >> 12;

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

pub fn byte_detect_chc(first: u8, second: u8) -> (u8, u8) {
    let carry_in = ((first & 0x07).overflowing_add(second & 0x07).0 & 0x08) >> 3;
    let carry_out = (first.overflowing_add(second).0 & 0x10) >> 4;

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

pub fn word_sub_detect_chc(first: u16, second: u16) -> (u8, u8) {
    let carry_in = ((first & 0x07FF).overflowing_sub(second & 0x07FF).0 & 0x0800) >> 11;
    let carry_out = (first.overflowing_sub(second).0 & 0x1000) >> 12;

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

pub fn byte_sub_detect_chc(first: u8, second: u8) -> (u8, u8) {
    let carry_in = ((first & 0x07).overflowing_sub(second & 0x07).0 & 0x08) >> 3;
    let carry_out = (first.overflowing_sub(second).0 & 0x10) >> 4;

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

pub fn word_addsigned_detect_chc(first: i16, second: i16) -> (u8, u8) {
    let carry_in = ((first & 0x07FF).overflowing_add(second & 0x07FF).0 & 0x0800) >> 11;
    let carry_out = (first.overflowing_add(second).0 & 0x1000) >> 12;

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