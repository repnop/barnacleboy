//use cpu::instruction::{Instruction, Operation};
use interconnect::Interconnect;
use cpu::cpuconsts::*;
use cpu::instruction::*;
use cpu::cpu_exec_funcs::*;
use std::thread;
use std::time::Duration;

macro_rules! read_byte_at_pc {
    ($cpu:ident, $interconnect:ident) => ({
        let ret = $interconnect.mem_read_byte($cpu.program_counter);
        $cpu.program_counter = $cpu.program_counter.overflowing_add(1).0;
        ret
    })
}

macro_rules! read_word_at_pc {
    ($cpu:ident, $interconnect:ident) => ({
        let ret = $interconnect.mem_read_word($cpu.program_counter);
        $cpu.program_counter = $cpu.program_counter.overflowing_add(2).0;
        ret
    })
}

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
    Nop,
    GPError,
    MemRWError,
    Stop,
    Halt,
    UnknownOpcode,
}

#[derive(Clone, Copy)]
pub enum WordRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            registers: [0; 8],
            flags: 0,
            stack_pointer: 0,
            program_counter: 0,
            current_opcode: 0,
            interrupts_enabled: true
        }
    }

    pub fn step(&mut self, interconnect: &mut Interconnect) -> Result<(), (CpuError, Instruction)> {
        let result = Cpu::decode_instruction(self, interconnect);

        if result.is_ok() {
            let ins = result.ok().unwrap();

            println!("{:<20}[A: {:X},{:>2} B: {:X},{:>2} C: {:X},{:>2} D: {:X},{:>2} E: {:X},{:>2} H: {:X},{:>2} L: {:X}] OP: 0x{:x}", ins.disassemble(),  self.registers[REG_A_INDEX],
                                                                                                       " ", self.registers[REG_B_INDEX],
                                                                                                       " ", self.registers[REG_C_INDEX],
                                                                                                       " ", self.registers[REG_D_INDEX],
                                                                                                       " ", self.registers[REG_E_INDEX],
                                                                                                       " ", self.registers[REG_H_INDEX],
                                                                                                       " ", self.registers[REG_L_INDEX],
                                                                                                       ins.opcode());

            if let Err(e) = ins.execute(self, interconnect) {
                Err((e, ins))
            } else {
                Ok(())
            }
        } else {
            Err((result.err().unwrap(), Instruction::new(0, [0, 0, 0], halt, disassemble_special)))
        }
    }

    pub fn decode_instruction(cpu: &mut Cpu, interconnect: &mut Interconnect) -> Result<Instruction, CpuError> {
        let opcode = interconnect.mem_read_byte(cpu.program_counter);
        cpu.program_counter = cpu.program_counter.overflowing_add(1).0;
        let pc = cpu.program_counter - 1;

        //println!("PC: 0x{:x}, OPCODE: 0x{:x}", pc, opcode);

        let (x, y, z, p, q) = extract_x_y_z_p_q(opcode);

        match (x, y, z, p, q) {
            // x = 0
            // match on y when z = 0
            (0, 0, 0, _, _) => Err(CpuError::Nop),
            
            (0, 1, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    ld_nn_sp,
                                    disassemble_ld_nn_sp)),

            (0, 2, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    stop,
                                    disassemble_special)),

            (0, 3, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 
                                     read_byte_at_pc!(cpu, interconnect),
                                     0],
                                    jr_d,
                                    disassemble_jr_rel)),

            (0, 4...7, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 
                                     read_byte_at_pc!(cpu, interconnect),
                                     0],
                                    jr_cc_d,
                                    disassemble_jr_rel)),

            // match on q when z = 1
            (0, _, 1, _, 0) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    ld_rp_nn,
                                    disassemble_ld_rp_nn)),

            (0, _, 1, _, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    add_hl_rp,
                                    disassemble_add_hl_rp)),

            // match on p when z = 2, q = 0
            (0, _, 2, 0, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_bc_a,
                                    disassemble_ld_bc_a)),

            (0, _, 2, 1, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_de_a,
                                    disassemble_ld_de_a)),
                                    
            (0, _, 2, 2, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_hlp_a,
                                    disassemble_ld_hlp_a)),

            (0, _, 2, 3, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_hlm_a,
                                    disassemble_ld_hlm_a)),

            // match on p when z = 2, q = 1
            (0, _, 2, 0, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_a_bc,
                                    disassemble_ld_a_bc)),

            (0, _, 2, 1, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_a_de,
                                    disassemble_ld_a_de)),

            (0, _, 2, 2, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_a_hlp,
                                    disassemble_ld_a_hlp)),

            (0, _, 2, 3, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_a_hlm,
                                    disassemble_ld_a_hlm)),

            // match on q when z = 3
            (0, _, 3, _, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    inc_rp,
                                    disassemble_inc_rp)),
            (0, _, 3, _, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    dec_rp,
                                    disassemble_dec_rp)),

            // match on z = 4 ... 6
            (0, _, 4, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    inc_r,
                                    disassemble_inc_r)),

            (0, _, 5, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    dec_r,
                                    disassemble_dec_r)),

            (0, _, 6, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    ld_r,
                                    disassemble_ld_r)),

            // match on y when z = 7
            (0, 0, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    rlca,
                                    disassemble_rlca)),

            (0, 1, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    rrca,
                                    disassemble_rrca)),

            (0, 2, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    rla,
                                    disassemble_rla)),

            (0, 3, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    rra,
                                    disassemble_rra)),

            (0, 4, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    daa,
                                    disassemble_daa)),

            (0, 5, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    cpl,
                                    disassemble_cpl)),

            (0, 6, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    scf,
                                    disassemble_scf)),

            (0, 7, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ccf,
                                    disassemble_ccf)),

            // x = 1
            // match on z = 0 ... 6
            (1, _, 0...5, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_r_r,
                                    disassemble_ld_r_r)),

            (1, _, 6, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    halt,
                                    disassemble_special)),

            (1, _, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_r_r,
                                    disassemble_ld_r_r)),

            // x = 2
            (2, _, _, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    alu_r,
                                    disassemble_alu_r)),

            // x = 3
            // match on y when z = 0
            (3, 0...3, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ret_cc,
                                    disassemble_ret_cc)),

            (3, 4, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    ld_ff00_n_a,
                                    disassemble_ld_ff00_n_a)),

            (3, 5, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    add_sp_d,
                                    disassemble_add_sp_d)),

            (3, 6, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    ld_a_ff00_n,
                                    disassemble_ld_a_ff00_n)),

            (3, 7, 0, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    ld_hl_sp_d,
                                    disassemble_ld_hl_sp_d)),

            // match on q = 0, z = 1
            (3, _, 1, _, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    pop_rp2,
                                    disassemble_pop_rp2)),

            // match on p when q = 1, z = 1
            (3, _, 1, 0, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ret,
                                    disassemble_ret)),
            (3, _, 1, 1, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    reti,
                                    disassemble_reti)),
            (3, _, 1, 2, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    jp_hl,
                                    disassemble_jp_hl)),
            (3, _, 1, 3, 1) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_sp_hl,
                                    disassemble_ld_sp_hl)),

            // match on y when z = 2
            (3, 0...3, 2, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    jp_cc_nn,
                                    disassemble_jp_cc_nn)),

            (3, 4, 2, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_ff00_c_a,
                                    disassemble_ld_ff00_c_a)),

            (3, 5, 2, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    ld_nn_a,
                                    disassemble_ld_nn_a)),
            (3, 6, 2, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ld_a_ff00_c,
                                    disassemble_ld_a_ff00_c)),
            (3, 7, 2, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    ld_a_nn,
                                    disassemble_ld_a_nn)),

            // match on y when z = 3
            (3, 0, 3, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    jp_nn,
                                    disassemble_jp_nn)),

            (3, 1, 3, _, _) => Ok(Instruction::new(pc + 1,
                                    [read_byte_at_pc!(cpu, interconnect),
                                     0,
                                     0],
                                     prefixed,
                                     disassemble_prefixed)),
            (3, 2...5, 3, _, _) => Err(CpuError::Nop),

            (3, 6, 3, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    di,
                                    disassemble_special)),

            (3, 7, 3, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    ei,
                                    disassemble_special)),

            // match on y when z = 4
            (3, 0...3, 4, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    call_cc_nn,
                                    disassemble_call_cc_nn)),

            // match on q when z = 5
            (3, _, 5, _, 0) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    push_rp2,
                                    disassemble_push_rp2)),
            (3, _, 5, 0, 1) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     read_byte_at_pc!(cpu, interconnect)],
                                    call_nn,
                                    disassemble_call_nn)),

            // z = 6, 7
            (3, _, 6, _, _) => Ok(Instruction::new(pc, 
                                    [opcode,
                                     read_byte_at_pc!(cpu, interconnect), 
                                     0],
                                    alu_n,
                                    disassemble_alu_n)),

            (3, _, 7, _, _) => Ok(Instruction::new(pc, 
                                    [opcode, 0, 0],
                                    rst_y8,
                                    disassemble_rst_y8)),

            // shouldn't get here, and if we do, something has gone very wrong
            _ => Err(CpuError::UnknownOpcode)
        }
    }

    pub fn set_16bit_reg(&mut self, register: WordRegister, value: u16) {
        match register {
            WordRegister::AF => {
                self.registers[REG_A_INDEX] = ((0xFF00 & value) >> 8) as u8;
                self.flags = (0x00FF & value) as u8;
            }
            WordRegister::BC => {
                self.registers[REG_B_INDEX] = ((0xFF00 & value) >> 8) as u8;
                self.registers[REG_C_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::DE => {
                self.registers[REG_D_INDEX] = ((0xFF00 & value) >> 8) as u8;
                self.registers[REG_E_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::HL => {
                self.registers[REG_H_INDEX] = ((0xFF00 & value) >> 8) as u8;
                self.registers[REG_L_INDEX] = (0x00FF & value) as u8;
            }
            WordRegister::SP => {
                self.stack_pointer = value;
            }
        }
    }

    pub fn get_16bit_reg(&mut self, register: WordRegister) -> u16 {
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

    pub fn set_flags(&mut self, flags: u8) {
        self.flags |= flags;
    }

    pub fn clear_flags(&mut self, flags: u8) {
        self.flags &= !flags;
    }
}

pub fn extract_x_y_z_p_q(opcode: u8) -> (u8, u8, u8, u8, u8) {
    let (x, y, z) = ((opcode & 0b11000000) >> 6, (opcode & 0b00111000) >> 3, opcode & 0b00000111);

    let (p, q) = ((y & 0b00000110) >> 1, y & 0b00000001);

    (x, y, z, p, q)
}
