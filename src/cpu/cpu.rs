use cpu::instruction::{Instruction, Operation};
use ::interconnect::Interconnect;
use cpu::cpuconsts::*;

pub struct Cpu {
    registers: [u8; 8],
    flags: u8,
    stack_pointer: u16,
    program_counter: u16,
}

enum Flags {
    Zero = FLAGS_ZERO_INDEX as isize,
    HalfCarry = FLAGS_CARRY_BORROW_3 as isize,
    FullCarry = FLAGS_CARRY_BORROW_7 as isize,
    Subtract = FLAGS_SUB_INDEX as isize,
}

enum RegisterPairs {
    AF,
    BC,
    DE,
    HL,
    SP,
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
                                interconnect.mem_read_byte(
                                        self.get_register_pair(RegisterPairs::HL));
                        }
                        // 0b01110XXX loads into memory at (HL) from register r
                        // LD (HL), r | (HL) <- r | r = XXX
                        else if instr_byte.0 & 0b0011_1000 == 0b0011_0000 {
                            let reg_from = instr_byte.0 & 0b0000_0111;
                            interconnect.mem_write_byte(self.get_register_pair(RegisterPairs::HL),
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
                            interconnect.mem_write_byte(
                                            self.get_register_pair(RegisterPairs::HL), val);
                            self.program_counter += 1;
                        }
                        // 0b00001010 is an 8-bit load from memory pointed to
                        //  by (BC) into register A
                        // LD A, (BC) | A <- mem[BC]
                        else if instr_byte.0 == 0b0000_1010 {
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(
                                        self.get_register_pair(RegisterPairs::BC));
                        }
                        // 0b00011010 is an 8-bit load from memory pointed to
                        //  by (DE) into register A
                        // LD A, (DE) | A <- mem[DE]
                        else if instr_byte.0 == 0b0001_1010 {
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(
                                        self.get_register_pair(RegisterPairs::DE));
                        }
                        // 0b00101010 OR 0b00111010
                        // These are the LDI A, (HL) and LDD A, (HL) instructions, respectively
                        // They are the same as LD A, (HL) except that
                        // LDI increments HL after storing the value in A
                        // LDD decrements HL after storing the value in A
                        else if instr_byte.0 & 0b1110_1111 == 0b0010_1010 {
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(
                                        self.get_register_pair(RegisterPairs::HL));

                            if instr_byte.0 & 0b0001_0000 == 0b0000_0000 {
                                let new_hl = self.get_register_pair(RegisterPairs::HL) + 1;
                                self.set_register_pair(RegisterPairs::HL, new_hl);
                            } else {
                                let new_hl = self.get_register_pair(RegisterPairs::HL) - 1;
                                self.set_register_pair(RegisterPairs::HL, new_hl);
                            }
                        }
                        // 0b00000010 is an 8-bit load from register A
                        // to the memory location pointed to by (BC)
                        else if instr_byte.0 == 0b0000_0010 {
                            interconnect.mem_write_byte(self.get_register_pair(RegisterPairs::BC),
                                                        self.registers[REG_A_INDEX]);
                        }
                        // 0b00010010 is an 8-bit load from register A
                        // to the memory location pointed to by (DE)
                        else if instr_byte.0 == 0b0000_0010 {
                            interconnect.mem_write_byte(self.get_register_pair(RegisterPairs::DE),
                                                        self.registers[REG_A_INDEX]);
                        }
                        // 0b00100010 OR 0b00110010
                        // These are the LDI (HL), A and LDD (HL), A instructions, respectively
                        // They are the same as LD A, (HL) except that
                        // LDI increments HL after storing the value from A
                        // LDD decrements HL after storing the value from A
                        else if instr_byte.0 & 0b1110_1111 == 0b0010_0010 {
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(
                                        self.get_register_pair(RegisterPairs::HL));

                            if instr_byte.0 & 0b0001_0000 == 0b0000_0000 {
                                let new_hl = self.get_register_pair(RegisterPairs::HL) + 1;
                                self.set_register_pair(RegisterPairs::HL, new_hl);
                            } else {
                                let new_hl = self.get_register_pair(RegisterPairs::HL) - 1;
                                self.set_register_pair(RegisterPairs::HL, new_hl);
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
                        else if instr_byte.0 == 0b1110_0010 {
                            interconnect.mem_write_byte(0xFF00 + self.registers[REG_C_INDEX] as u16,
                                                self.registers[REG_A_INDEX]);
                        }
                        // 0b11110000 is an 8-bit load from memory pointed to
                        //  by (0xFF00 + n) to register A
                        // LD A, (0xFF00 + n) | A <- mem[0xFF00 + n]
                        else if instr_byte.0 == 0b1110_0010 {
                            let val = interconnect.mem_read_byte(self.program_counter);
                            self.registers[REG_A_INDEX] =
                                interconnect.mem_read_byte(0xFF00 + val as u16);
                            self.program_counter += 1;
                        }
                        // 0b11110000 is an 8-bit load to memory pointed to
                        //  by (0xFF00 + n) from register A
                        // LD (0xFF00 + n), A | mem[0xFF00 + n] <- A
                        else if instr_byte.0 == 0b1110_0010 {
                            let val = interconnect.mem_read_byte(self.program_counter);
                            interconnect.mem_write_byte(0xFF00 + val as u16,
                                                        self.registers[REG_A_INDEX]);
                            self.program_counter += 1;
                        }
                        // 0b11111010 is an 8-bit load from memory pointed to
                        //  by (nn) to register A
                        // LD A, (nn) | A <- mem[nn]
                        else if instr_byte.0 == 0b1111_1010 {
                            let val = interconnect.mem_read_word(self.program_counter);
                            self.registers[REG_A_INDEX] = interconnect.mem_read_byte(val);
                            self.program_counter += 2;
                        }
                        // 0b11101010 is an 8-bit load to memory pointed to
                        //  by (nn) from register A
                        // LD (nn), A | mem[nn] <- A
                        else if instr_byte.0 == 0b1110_1010 {
                            let val = interconnect.mem_read_word(self.program_counter);
                            interconnect.mem_write_byte(val, self.registers[REG_A_INDEX]);
                            self.program_counter += 2;
                        }
                    } // END 11 BLOCK
                }
                Operation::Ld16 => {

                    // 0b00XX_0001 is a 16-bit load from a 16-bit constant following
                    // the opcode into the 16-bit register combinations
                    // XX is one of the following with the associated register
                    // 00 -> BC
                    // 01 -> DE
                    // 10 -> HL
                    // 11 -> SP
                    if instr_byte.0 & 0b1100_1111 == 0b0000_0001 {
                        let val = interconnect.mem_read_word_little_endian(self.program_counter);
                        match (instr_byte.0 >> 4) & 0x3 {
                            0b0000_0000 => self.set_register_pair(RegisterPairs::BC, val),
                            0b0000_0001 => self.set_register_pair(RegisterPairs::DE, val),
                            0b0000_0010 => self.set_register_pair(RegisterPairs::DE, val),
                            0b0000_0011 => self.stack_pointer = val,
                            _ => unreachable!(),
                        }

                        self.program_counter += 1;
                    }
                    // 0b1111_1001 is a 16-bit load from HL into the stack pointer
                    // LD SP, HL | SP <- HL
                    else if instr_byte.0 == 0b1111_1001 {
                        self.stack_pointer = self.get_register_pair(RegisterPairs::HL);
                    }
                    // 0b1111_1000 is a 16-bit load into HL from
                    // SP + e, where e is a signed 8-bit number following the
                    // opcode. This instruction clears the subtraction and zero
                    // flags while setting the H and CY flags if they occur.
                    // LDHL SP, e | HL <- SP + e
                    else if instr_byte.0 == 0b1111_1000 {
                        let val = interconnect.mem_read_byte(self.program_counter) as i8;
                        let carry_in = (((self.stack_pointer & 0x07FF) as i16 + 
                                            (val as i16)) & 0x0800) >> 11;
                        let carry_out = ((self.stack_pointer as i16 + val as i16) & 0x1000) >> 12;

                        if carry_in ^ carry_out == 1 {
                            self.set_flag(Flags::HalfCarry);
                        } else {
                            self.clear_flag(Flags::HalfCarry);
                        }

                        let (val, result) = (self.stack_pointer as i16).overflowing_add(val as i16);
                        self.set_register_pair(RegisterPairs::HL, val as u16);
                        if result {
                            self.set_flag(Flags::FullCarry);
                        } else {
                            self.clear_flag(Flags::FullCarry);
                        }

                        self.clear_flag(Flags::Zero);
                        self.clear_flag(Flags::Subtract);

                        self.program_counter += 1;
                    }

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
    pub fn set_register_pair(&mut self, pair: RegisterPairs, value: u16) {
        match pair {
            RegisterPairs::BC => {
                self.registers[REG_B_INDEX] = (value >> 8) as u8;
                self.registers[REG_C_INDEX] = (value & 0x00FF) as u8;
            }

            RegisterPairs::DE => {
                self.registers[REG_D_INDEX] = (value >> 8) as u8;
                self.registers[REG_E_INDEX] = (value & 0x00FF) as u8;
            }

            RegisterPairs::HL => {
                self.registers[REG_H_INDEX] = (value >> 8) as u8;
                self.registers[REG_L_INDEX] = (value & 0x00FF) as u8;
            }

            RegisterPairs::SP => {
                self.stack_pointer = value;
            }

            RegisterPairs::AF => {
                self.registers[REG_A_INDEX] = (value >> 8) as u8;
                self.flags = (value & 0x00FF) as u8;
            }
        }
    }

    #[inline]
    pub fn get_register_pair(&self, pair: RegisterPairs) -> u16 {
        match pair {
            RegisterPairs::BC => {
                ((self.registers[REG_B_INDEX] as u16) << 8) | (self.registers[REG_C_INDEX] as u16)
            }

            RegisterPairs::DE => {
                ((self.registers[REG_D_INDEX] as u16) << 8) | (self.registers[REG_E_INDEX] as u16)
            }

            RegisterPairs::HL => {
                ((self.registers[REG_H_INDEX] as u16) << 8) | (self.registers[REG_L_INDEX] as u16)
            }

            RegisterPairs::SP => self.stack_pointer,

            RegisterPairs::AF => ((self.registers[REG_A_INDEX] as u16) << 8) | (self.flags as u16),
        }
    }

    pub fn is_flag_set(&self, flag: Flags) -> bool {
        let set_bit = (1u8 << flag as u8);
        self.flags & set_bit == set_bit
    }

    #[inline]
    pub fn set_flag(&mut self, flag: Flags) {
        self.flags |= (1u8 << flag as u8)
    }

    #[inline]
    pub fn clear_flag(&mut self, flag: Flags) {
        self.flags &= !(1u8 << flag as u8)
    }
}
