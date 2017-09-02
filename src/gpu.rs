use interconnect::Interconnect;
use constants::*;

//const COLORS: [u32; 4] = [0x00FFFFFF, 0x00606060, 0x00C0C0C0, 0x00000000];
//let mut clrs: [u32; 4] = [0x00FFFFFF, 0x00606060, 0x00C0C0C0, 0x00000000];

enum Colors {
    White = 0x00FFFFFF,
    LGray = 0x00606060,
    DGray = 0x00C0C0C0,
    Black = 0x00000000
}

macro_rules! is_bit_set {
    ($x:ident, $y:expr) => ({
        if $x & (1 << $y) == (1 << $y) {
            true
        } else {
            false
        }
    });
}

pub struct Gpu {
    in_vblank: bool,
    pub screen_buffer: Vec<u32>,
    pub blank_screen_buffer: Vec<u32>
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            in_vblank: false,
            screen_buffer: vec![0; SCREEN_WIDTH * SCREEN_HEIGHT],
            blank_screen_buffer: vec![0; SCREEN_WIDTH * SCREEN_HEIGHT]
        }
    }

    pub fn update(&mut self, ic: &mut Interconnect) {
        //self.update_bg(ic);
        //self.update_sprites(ic);
        let y_coord = ic.mem_read_byte(LCDC_Y_COORD);
        ic.mem_write_byte(LCDC_Y_COORD, if y_coord + 1 > 153 { 0 } else { y_coord + 1 });
        
        if y_coord == 144 {
            Gpu::request_vblank_interrupt(ic);
        }
    }

    fn request_vblank_interrupt(ic: &mut Interconnect) {
        let mut intflag = ic.mem_read_byte(0xFF0F);
        ic.mem_write_byte(0xFF0F, intflag | INTERRUPT_FLAG_VBLANK);
    }

    fn draw_tiles(&mut self, ic: &mut Interconnect) {

    }
}