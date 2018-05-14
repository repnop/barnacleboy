use cpu::Cpu;
use interconnect::Interconnect;
use rom::{Rom, BootRom};

#[test]
fn ld_nn_sp() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("ld nn,sp.bin")[..]));

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.sp, 0xFEED);
}

#[test]
fn stop() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("stop.bin")[..]));

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.stopped, true);
}

#[test]
fn jr_d() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("jr d.bin")[..]));

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.pc, 0x0D);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.pc, 0x06);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.pc, 0x10);
}

#[test] 
fn jr_cc_d() {

}

#[test]
fn push_pop_rp2() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("push pop rp2.bin")[..]));

    cpu.sp = 0xC500;
    cpu.regs.a = 0x11;
    cpu.regs.f = 0x22;
    cpu.regs.b = 0x33;
    cpu.regs.c = 0x44;
    cpu.regs.d = 0x55;
    cpu.regs.e = 0x66;
    cpu.regs.h = 0x77;
    cpu.regs.l = 0x88;

    // Push
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(cpu.sp), cpu.regs.b);
    assert_eq!(interconnect.mem_read_byte(cpu.sp + 1), cpu.regs.c);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(cpu.sp), cpu.regs.d);
    assert_eq!(interconnect.mem_read_byte(cpu.sp + 1), cpu.regs.e);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(cpu.sp), cpu.regs.h);
    assert_eq!(interconnect.mem_read_byte(cpu.sp + 1), cpu.regs.l);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(cpu.sp), cpu.regs.a);
    assert_eq!(interconnect.mem_read_byte(cpu.sp + 1), cpu.regs.f);
    


    // Pop
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.as_pairs().bc, 0x1122);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.as_pairs().de, 0x7788);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.as_pairs().hl, 0x5566);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.as_pairs().af, 0x3344);
}

#[test]
fn ld_r_r() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("ld r,r.bin")[..]));

    interconnect.mem_write_byte(0xC000, 0x32);
    assert!(interconnect.mem_read_byte(0xC000) == 0x32);
    // Initialize registers
    // A = 6
    // B = 0
    // C = 1
    // D = 2
    // E = 3
    // H = 4
    // L = 5
    for _ in 0..0x9 {
        cpu.step(&mut interconnect, true);
    }

    // LD B, r is 0x10 - 0x16
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD C, r is 0x20 - 0x26
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD D, r is 0x30 - 0x36
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD E, r is 0x40 - 0x46
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD H, r is 0x50 - 0x56
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD L, r is 0x60 - 0x66
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 6);

    for _ in 0..0x08 {
        cpu.step(&mut interconnect, true);
    }

    // LD A, r is 0x70 - 0x76
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 1);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 2);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 3);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 4);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 5);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 5);

    // LD HL, 0xC000
    cpu.step(&mut interconnect, true);
    cpu.step(&mut interconnect, true);

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 0x32);

    cpu.regs.l = 0x00;

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 0x32);

    cpu.regs.h = 0xC0;

    cpu.regs.a = 0x08;
    cpu.regs.b = 0x01;
    cpu.regs.c = 0x02;
    cpu.regs.d = 0x03;
    cpu.regs.e = 0x04;

    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.b);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.c);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.d);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.e);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.h);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.l);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), cpu.regs.a);
}

#[test]
fn ld_r() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new(Rom::new(&[0; 0x200]),
        BootRom::new(&include_bytes!("ld r.bin")[..]));

    interconnect.mem_write_byte(0xC000, 0x42);

    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.b, 0x30);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.c, 0x31);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.d, 0x32);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.e, 0x33);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.h, 0xC0);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.l, 0x00);
    cpu.step(&mut interconnect, true);
    assert_eq!(interconnect.mem_read_byte(0xC000), 0x34);
    cpu.step(&mut interconnect, true);
    assert_eq!(cpu.regs.a, 0x37);
}