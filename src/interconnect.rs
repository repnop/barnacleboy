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
}