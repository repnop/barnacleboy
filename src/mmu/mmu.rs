use mmu::memconsts::*;

pub struct Memory {
    cart_rom: Box<[u8]>,
    cart_ram: Box<[u8]>,
    video_ram: Box<[u8]>,
    work_ram: Box<[u8]>,
    sprite_attribute_table: Box<[u8]>,
    io_port_ram: Box<[u8]>,
    high_ram: Box<[u8]>,
    interrupt_register: u8
}

impl Memory {
    
    pub fn new() -> Memory {
        Memory {
            cart_rom: vec![0; CARTRIDGE_ROM_SIZE].into_boxed_slice(),
            cart_ram: vec![0; CART_EXTERNAL_RAM_SIZE].into_boxed_slice(),
            video_ram: vec![0; VRAM_SIZE].into_boxed_slice(),
            work_ram: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            sprite_attribute_table: vec![0; SPRITE_ATTRIBUTE_TABLE_SIZE].into_boxed_slice(),
            io_port_ram: vec![0; IO_RAM_SIZE].into_boxed_slice(),
            high_ram: vec![0; HIGH_RAM_SIZE].into_boxed_slice(),
            interrupt_register: 0
        }
    }
}