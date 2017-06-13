use cpu::cpu::*;
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
fn jr_d() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_byte(0, 0x18);
    interconnect.mem_write_byte(1, 5);

    cpu.run_next_instruction(&mut interconnect);

    assert!(cpu.program_counter == 7);
}

#[test]
fn ld_nn_sp() {
    let mut cpu = Cpu::new();
    let mut interconnect = Interconnect::new();

    interconnect.mem_write_word(10, 0xFFFF);
    interconnect.mem_write_byte(0, 0x08);
    interconnect.mem_write_word(1, 10);

    cpu.run_next_instruction(&mut interconnect);

    assert!(interconnect.mem_read_word(10) == 0x0000);
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

    for i in 0 .. 7 {
        cpu.run_next_instruction(&mut interconnect);
        println!("{:?}", cpu);
    }

    
    for i in 0 .. 8 {
        if i != 0b110 {
            assert!(cpu.registers[i] == 1);
        }
    } 
}