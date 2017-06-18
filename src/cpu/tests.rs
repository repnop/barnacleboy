use cpu::cpu::*;
use cpu::cpuconsts::*;
use interconnect::Interconnect;

#[test]
fn extract() {
    let v = 0b10_10_1_010;
    let (x, y, z, p, q) = extract_x_y_z_p_q(v);

    assert!(x == 0b10);
    assert!(y == 0b101);
    assert!(z == 0b010);
    assert!(p == 0b10);
    assert!(q == 0b1);
}

#[test]
fn ld_rp_nn() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_byte(0, 0x10)
}

#[test]
fn jr_cc_d() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    // Test JR Z, r8
    cpu.flags |= FLAG_ZERO;

    interconnect.mem_write_byte(0, 0x28);
    interconnect.mem_write_byte(1, 5);

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 7);

    cpu.flags &= !FLAG_ZERO;
    cpu.program_counter = 0;

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 2);

    // Test JR C, r8
    cpu.program_counter = 0;
    cpu.flags |= FLAG_CARRY;

    interconnect.mem_write_byte(0, 0x38);
    interconnect.mem_write_byte(1, 5);

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 7);

    cpu.flags &= !FLAG_CARRY;
    cpu.program_counter = 0;

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 2);

    // Test JR NZ, r8
    cpu.program_counter = 0;
    cpu.flags |= FLAG_ZERO;

    interconnect.mem_write_byte(0, 0x20);
    interconnect.mem_write_byte(1, 5);

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 2);

    cpu.flags &= !FLAG_ZERO;
    cpu.program_counter = 0;

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 7);

    // Test JR NC, r8
    cpu.program_counter = 0;
    cpu.flags |= FLAG_CARRY;

    interconnect.mem_write_byte(0, 0x30);
    interconnect.mem_write_byte(1, 5);

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 2);

    cpu.flags &= !FLAG_CARRY;
    cpu.program_counter = 0;

    let _ = cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 7);
}

#[test]
fn jr_d() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_byte(0, 0x18);
    interconnect.mem_write_byte(1, 5);

    cpu.step(&mut interconnect);

    assert!(cpu.program_counter == 7);
}

#[test]
fn ld_nn_sp() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_word(10, 0xFFFF);
    interconnect.mem_write_byte(0, 0x08);
    interconnect.mem_write_word(1, 10);

    cpu.step(&mut interconnect);

    assert!(interconnect.mem_read_word(10) == 0x0000);
}

#[test]
fn push_pop() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_buffer(0, &[0xE8, 0x75,
                                       0x01, 0x00, 0xFF,
                                       0xC5,
                                       0x01, 0xFF, 0x00,
                                       0xC1, 0xF3]);

    let _ = cpu.step(&mut interconnect);
    let _ = cpu.step(&mut interconnect);
    println!("{}: {:X}{:X}", cpu.stack_pointer, interconnect.mem_read_byte(cpu.stack_pointer + 1), interconnect.mem_read_byte(cpu.stack_pointer + 2));
    let _ = cpu.step(&mut interconnect);
    println!("{}: {:X}{:X}", cpu.stack_pointer, interconnect.mem_read_byte(cpu.stack_pointer + 1), interconnect.mem_read_byte(cpu.stack_pointer + 2));
    let _ = cpu.step(&mut interconnect);
    let _ = cpu.step(&mut interconnect);
    let _ = cpu.step(&mut interconnect);

    println!("{}: {:X}{:X}", cpu.stack_pointer, interconnect.mem_read_byte(cpu.stack_pointer + 1), interconnect.mem_read_byte(cpu.stack_pointer + 2));


    assert!(cpu.get_16bit_reg(WordRegister::BC) == 0xFF00);
}

#[test]
fn inc_r() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_byte(0, 0x04);
    interconnect.mem_write_byte(1, 0x14);
    interconnect.mem_write_byte(2, 0x24);
    interconnect.mem_write_byte(3, 0x0C);
    interconnect.mem_write_byte(4, 0x1C);
    interconnect.mem_write_byte(5, 0x2C);
    interconnect.mem_write_byte(6, 0x3C);

    for _ in 0 .. 7 {
        cpu.step(&mut interconnect);
        println!("{:?}", cpu);
    }

    
    for i in 0 .. 8 {
        if i != 0b110 {
            assert!(cpu.registers[i] == 1);
        }
    } 
}