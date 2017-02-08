use mmu::mmu::Memory;

pub struct Interconnect {
    memory: Memory,
}

impl Interconnect {
    pub fn new() -> Interconnect {
        Interconnect { memory: Memory::new() }
    }

    pub fn mem_read_byte(&self, mem_loc: u16) -> u8 {
        self.memory.read_byte(mem_loc)
    }

    pub fn mem_write_byte(&mut self, mem_loc: u16, value: u8) {
        self.memory.write_byte(mem_loc, value);
    }

    pub fn mem_read_word(&self, mem_loc: u16) -> u16 {
        self.memory.read_word(mem_loc)
    }

    pub fn mem_read_word_little_endian(&self, mem_loc: u16) -> u16 {
        self.memory.read_word_little_endian(mem_loc)
    }

    pub fn mem_write_word(&mut self, mem_loc: u16, value: u16) {
        self.memory.write_word(mem_loc, value);
    }
}
