use ::mmu::mmu::Memory;

pub struct Interconnect {
    memory: Memory
}

impl Interconnect {
    pub fn new() -> Interconnect {
        Interconnect {
            memory: Memory::new()
        }
    }

    pub fn mem_read_byte(&self, mem_loc: u16) -> u8 {
        self.memory.read_byte(mem_loc)
    }

    pub fn mem_write_byte(&mut self, mem_loc: u16, value: u8) {
        self.memory.write_byte(mem_loc, value);
    }
}