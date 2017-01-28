//! # LR35902 CPU Emulator
//! 
//! Information credit:
//! All information in this documentation is found from:
//! [Gameboy CPU Manual v1.01 by DP](http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf)
//! Official Gameboy Programming Manual
//!
//! This module defines the structures and functions 
//! for use in emulating the LR35902 CPU architecture.
//! 
//! The LR35902 is a CPU based on the ZiLOG Z80 processor
//! which is a processor designed for compatibility with
//! the Intel 8080 microprocessor.
//!
//! ## Differences
//! The LR35902 does, however, have some differences from
//! the ZiLOG Z80:
//! 
//! ### Registers
//! Unlike the Z80, the LR35902 does not contain a set of
//! alternate registers. It also does not contain the index
//! registers IX and IY.
//!
//! ### Instructions
//! Some of the LR35902 instructions differ from their Z80
//! counterparts, along with some additions and removal of
//! various instructions.
//! 
//! #### Added Instructions
//! `ADD SP, nn` where `nn` is a signed byte.
//! `LDI (HL), A` Loads the value in A into HL and increments HL
//! `LDD (HL), A` Similar to above, except it decrements HL at the end
//! `LDI A, (HL)` Loads the value in HL into A and increments A
//! `LDD A, (HL)` Again, similar to above but decrements A at the end
//! `LD A, ($FF00 + nn)` where `nn` is a signed byte
//! `LD A, ($FF00 + C)`
//! `LD ($FF00 + nn), A` where `nn` is a signed byte
//! `LD ($FF00 + C), A`
//! `LD (nnnn), SP` Loads the value of SP into the 16-bit location nnnn
//! `LD HL, SP + nn` where `nn` is a signed byte
//! `STOP` Halts the processor and screen until input is received from buttons
//! `SWAP r` Swaps the high and low nibbles of register `r`
//!
//! #### Removed Instructions
//! All instructions that used the IX or IY index registers.
//! All IN/OUT instructions.
//! All exchange instructions.
//! All instructions prefixed with the byte `ED` except for the remapped `RETI`
//! All conditional jumps, call, and returns which use the parity/overflow and sign flags
//!
//! #### Changed Instructions
//! These instructions have different opcodes:
//! `LD A, (nnnn)`
//! `LD (nnnn), A`
//! `RETI`
//!
//! ## Useful Constants
//! Register binary values for LD r, 'r:
//! Register | Binary
//! -------- | ------
//! A | 111
//! B | 000
//! C | 001
//! D | 010
//! E | 011
//! H | 100
//! L | 101
//! 
//! 
pub mod cpu;
pub mod instruction;