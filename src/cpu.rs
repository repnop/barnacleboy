pub trait Cpu {
    type Error;

    fn execute(&mut self) -> Result<(), Self::Error>;
    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error>;
}

pub struct SharpLR35902 {}

pub enum LRError {
    InvalidMemoryRead(u16),
    InvalidMemoryWrite(u16),
}

impl Cpu for SharpLR35902 {
    type Error = LRError;

    fn execute(&mut self) -> Result<(), LRError> {
        Ok(())
    }

    fn execute_with_cycles(&mut self, cycle_bound: usize) -> Result<(), Self::Error> {
        Ok(())
    }
}
