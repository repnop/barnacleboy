use std::ops::{Index, IndexMut};

pub trait MemoryInterface {
    type Word;
    type Index;
    type Error;

    fn read(&self, address: Self::Index) -> Result<Self::Word, Self::Error>;
    fn write(&mut self, address: Self::Index, data: Self::Word) -> Result<(), Self::Error>;
}
