use cpu::instruction::{Instruction, Operation};
use ::interconnect::Interconnect;
use cpu::cpuconsts::*;

pub struct Cpu {
    registers: [u8; 8],
    flags: u8,
    stack_pointer: u16,
    program_counter: u16,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            registers: [0u8; 8], // Extra array slot for compatibility with opcode indices
            flags: 0u8,
            stack_pointer: 0u16,
            program_counter: 0u16,
        }
    }

    pub fn execute_instruction(&mut self, interconnect: &mut Interconnect) {
        let mut instr_byte = Instruction(interconnect.mem_read_byte(self.program_counter));

        let is_prefixed = instr_byte.is_prefixed();

        if is_prefixed {
            instr_byte = Instruction(interconnect.mem_read_byte(self.program_counter + 1));
            self.program_counter += 1; // Increment PC an extra time because of prefix byte
        }

        // Increment PC so that it points to the next byte,
        // in-instruction values will also inc the PC
        self.program_counter += 1;

        if !is_prefixed {
            match instr_byte.get_opcode_type_nonprefix() {
                Operation::Nop => {}
                Operation::Ld8 => {
                    // Since the only difference between many of the LD opcodes is specific values,
                    // we have to check them to know which operation to preform

                    // Because of how unfriendly the opcodes are, grouping them based on
                    // bits 7 and 6 to begin helps to group

                    // 01 BLOCK
                    if instr_byte.0 & 0b1100_0000 == 0b0100_0000 {

                        // 0b01XXXYYY where YYY != 110 is an 8-bit LD from one register to the other
                        // LD r, r' | r <- r' | r = XXX, r' = YYY
                        if instr_byte.0 & 0b0000_0111 != 0b0000_0110 {
                            let reg_to = instr_byte.0 & 0b0011_1000;
                            let reg_from = instr_byte.0 & 0b0000_0111;

                            self.registers[reg_to as usize] = self.registers[reg_from as usize];
                        }
                        // 0b01XXX110 loads into dest. register r the value pointed at by (HL)
                        // LD r, (HL) | r <- (HL) | r = XXX
                        else if instr_byte.0 & 0b0000_0111 == 0b0000_0110 {
                            let reg_to = instr_byte.0 & 0b0011_1000;
                            self.registers[reg_to as usize] =
                                interconnect.mem_read_byte(self.get_hl());
                        }
                        // 0b01110XXX loads into memory at (HL) from register r
                        // LD (HL), r | (HL) <- r | r = XXX
                        else if instr_byte.0 & 0b0011_1000 == 0b0011_0000 {
                            let reg_from = instr_byte.0 & 0b0000_0111;
                            interconnect.mem_write_byte(self.get_hl(),
                                                        self.registers[reg_from as usize]);
                        }

                    }
                    // END 01 BLOCK
                    //
                    // 00 BLOCK
                    else if instr_byte.0 & 0b1100_0000 == 0b0000_0000 {

                        // 0b00XXX110 is an 8-bit immediate load from the next byte
                        // LD r, n | r <- n | r = XXX, n follows the instruction byte
                        if instr_byte.0 & 0b0000_0111 == 0b0000_0110 {
                            let reg_to = instr_byte.0 & 0b0011_1000;
                            self.registers[reg_to as usize] =
                                interconnect.mem_read_byte(self.program_counter);
                            self.program_counter += 1;
                        }
                        // 0b00110110 is an 8-bit immediate LD from memory pointed to
                        //  by (HL) from the next byte
                        // LD (HL), n | n follows the instruction byte
                        else if instr_byte.0 & 0b0011_1111 == 0b0011_0110 {
                            let val = interconnect.mem_read_byte(self.program_counter);
                            interconnect.mem_write_byte(self.get_hl(), val);
                            self.program_counter += 1;
                        }
                        // 0b00001010 is an 8-bit load from memory pointed to
                        //  by (BC) into register A
                        // LD A, (BC) | A <- mem[BC]
                        else if instr_byte.0 == 0b00001010 {
                            self.registers[REG_A_INDEX] = interconnect.mem_read_byte(self.get_bc());
                        }
                        // 0b00011010 is an 8-bit load from memory pointed to
                        //  by (DE) into register A
                        // LD A, (DE) | A <- mem[DE]
                        else if instr_byte.0 == 0b00011010 {
                            self.registers[REG_A_INDEX] = interconnect.mem_read_byte(self.get_de());
                        }
                        // 0b00101010 OR 0b00111010
                        // These are the LDI A, (HL) and LDD A, (HL) instructions, respectively
                        // They are the same as LD A, (HL) except that
                        // LDI increments HL after storing the value in A
                        // LDD decrements HL after storing the value in A
                        else if instr_byte.0 & 0b11101111 == 0b00101010 {
                            self.registers[REG_A_INDEX] = interconnect.mem_read_byte(self.get_hl());

                            if instr_byte.0 & 0b00010000 == 0b00000000 {
                                let new_hl = self.get_hl() + 1;
                                self.set_hl(new_hl);
                            } else {
                                let new_hl = self.get_hl() - 1;
                                self.set_hl(new_hl);
                            }
                        }
                    }
                    // END 00 BLOCK
                    //
                    // 11 BLOCK
                    else {

                        // 0b11110010 is an 8-bit load from memory pointed to
                        //  by (0xFF00 + reg C) into register A
                        // LD A, (0xFF00 + C) | A <- mem[0xFF00 + C]
                        if instr_byte.0 == 0b11110010 {
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(0xFF00 +
                                                            self.registers[REG_C_INDEX] as u16);
                        }
                        // 0b11100010 is an 8-bit load from register A to
                        //  memory location (0xFF00 + C)
                        // LD (0xFF00 + C), A  | mem[0xFF00 + C] <- A
                        else if instr_byte.0 == 0b11100010 {
                            interconnect.mem_write_byte(0xFF00 + self.registers[REG_C_INDEX] as u16,
                                                self.registers[REG_A_INDEX]);
                        }
                        // 0b11110000 is an 8-bit load from memory pointed to
                        //  by (0xFF00 + n) to register A
                        // LD A, (0xFF00 + n) | A <- mem[0xFF00 + n]
                        else if instr_byte.0 == 0b11100010 {
                            let val = interconnect.mem_read_byte(self.program_counter);
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(0xFF00 + val as u16);
                            self.program_counter += 1;
                        }
                        // 0b11110000 is an 8-bit load to memory pointed to
                        //  by (0xFF00 + n) from register A
                        // LD (0xFF00 + n), A | mem[0xFF00 + n] <- A
                        else if instr_byte.0 == 0b11100010 {
                            let val = interconnect.mem_read_byte(self.program_counter);
                            interconnect.mem_write_byte(0xFF00 + val as u16,
                                                        self.registers[REG_A_INDEX]);
                            self.program_counter += 1;
                        }
                        // 0b11111010 is an 8-bit load from memory pointed to
                        //  by (nn) to register A
                        // LD A, (nn) | A <- mem[nn]
                        else if instr_byte.0 == 0b11111010 {
                            let val = interconnect.mem_read_word(self.program_counter);
                            self.registers[REG_A_INDEX] = interconnect.mem_read_byte(val);
                            self.program_counter += 2;
                        }
                        // 0b11101010 is an 8-bit load to memory pointed to
                        //  by (nn) from register A
                        // LD (nn), A | mem[nn] <- A
                        else if instr_byte.0 == 0b11101010 {
                            let val = interconnect.mem_read_word(self.program_counter);
                            interconnect.mem_write_byte(val, self.registers[REG_A_INDEX]);
                            self.program_counter += 2;
                        }
                    } // END 11 BLOCK
                }

                _ => unimplemented!(),
            }
        } else {
            match instr_byte.get_opcode_type_prefixed() {
                _ => unimplemented!(),
            }
        }
    }

    #[inline]
    pub fn set_af(&mut self, value: u16) {
        self.registers[REG_A_INDEX] = (value >> 8) as u8;
        self.flags = (value & 0x00FF) as u8;
    }

    #[inline]
    pub fn set_bc(&mut self, value: u16) {
        self.registers[REG_B_INDEX] = (value >> 8) as u8;
        self.registers[REG_C_INDEX] = (value & 0x00FF) as u8;
    }

    #[inline]
    pub fn set_de(&mut self, value: u16) {
        self.registers[REG_D_INDEX] = (value >> 8) as u8;
        self.registers[REG_E_INDEX] = (value & 0x00FF) as u8;
    }

    #[inline]
    pub fn set_hl(&mut self, value: u16) {
        self.registers[REG_H_INDEX] = (value >> 8) as u8;
        self.registers[REG_L_INDEX] = (value & 0x00FF) as u8;
    }

    #[inline]
    pub fn get_af(&self) -> u16 {
        ((self.registers[REG_A_INDEX] as u16) << 8) | (self.flags as u16)
    }

    #[inline]
    pub fn get_bc(&self) -> u16 {
        ((self.registers[REG_B_INDEX] as u16) << 8) | (self.registers[REG_C_INDEX] as u16)
    }

    #[inline]
    pub fn get_de(&self) -> u16 {
        ((self.registers[REG_D_INDEX] as u16) << 8) | (self.registers[REG_E_INDEX] as u16)
    }

    #[inline]
    pub fn get_hl(&self) -> u16 {
        ((self.registers[REG_H_INDEX] as u16) << 8) | (self.registers[REG_L_INDEX] as u16)
    }

    #[inline]
    pub fn zero_flag(&self) -> bool {
        self.flags & (1u8 << FLAGS_ZERO_INDEX) == 0b1000_0000
    }

    #[inline]
    pub fn subtraction_flag(&self) -> bool {
        self.flags & (1u8 << FLAGS_SUB_INDEX) == 0b0100_0000
    }

    #[inline]
    pub fn half_carry(&self) -> bool {
        self.flags & (1u8 << FLAGS_CARRY_BORROW_3) == 0b0010_0000
    }

    #[inline]
    pub fn full_carry(&self) -> bool {
        self.flags & (1u8 << FLAGS_CARRY_BORROW_7) == 0b0001_0000
    }
}
