
use cpu::cpu::*;
use cpu::cpuconsts::*;
use interconnect::Interconnect;

pub struct Instruction {
    loc: u16,
    bytes: [u8; 3],
    execute_fn: fn(&Instruction, &mut Cpu, &mut Interconnect) -> Result<(), CpuError>,
    disassemble_fn: fn(&[u8]) -> String
}

impl Instruction {
    pub fn new(pc: u16, b: [u8; 3], efn: fn(&Instruction, &mut Cpu, &mut Interconnect) -> Result<(), CpuError>, dfn: fn(&[u8]) -> String) -> Instruction {
        Instruction {
            loc: pc,
            bytes: b,
            execute_fn: efn,
            disassemble_fn: dfn
        }
    }

    pub fn execute(&self, cpu: &mut Cpu, ic: &mut Interconnect) -> Result<(), CpuError> {
        (self.execute_fn)(self, cpu, ic)
    }

    pub fn disassemble(&self) -> String {
        (self.disassemble_fn)(&self.bytes)
    }

    pub fn opcode(&self) -> u8 {
        self.bytes[0]
    }

    pub fn get_word(&self) -> u16 {
        ((self.bytes[2] as u16) << 8) | (0x00FF & (self.bytes[1] as u16))
    }

    pub fn get_byte(&self) -> u8 {
        self.bytes[1]
    }
}

static R_STR: [&'static str; 8] = ["b", "c", "d", "e", "h", "l", "(hl)", "a"];

static RP_STR: [&'static str; 4] = ["bc", "de", "hl", "sp"];

static RP2_STR: [&'static str; 4] = ["bc", "de", "hl", "af"];

static CC_STR: [&'static str; 4] = [" nz", " z", " nc", " c"];

static ALU_STR: [&'static str; 8] = 
    ["add a,", "adc a,", "sub", "sbc a,", "and", "xor", "or", "cp"];

static ROT_STR: [&'static str; 8] = 
    ["rlc", "rrc", "rl", "rr", "sla", "sra", "swap", "srl"];

pub fn disassemble_ld_nn_sp(bytes: &[u8]) -> String {
    format!("ld (0x{:x}), sp", ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_special(bytes: &[u8]) -> String {

    String::from(match bytes[0] {
        0x00 => "nop",
        0x10 => "stop",
        0x76 => "halt",
        0xF3 => "di",
        0xFB => "ei",
        _ => "unknown special instruction"
    })
}

pub fn disassemble_jr_rel(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    let mut disas = String::from("jr");
    
    if y >= 4 && y <= 7 {
        disas.push_str(CC_STR[y-4]);
    }

    disas.push_str(&format!(" {}", bytes[1] as i16));
    
    disas
}

pub fn disassemble_ld_rp_nn(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("ld {}, 0x{:x}", RP_STR[p], (((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16))))
}

pub fn disassemble_add_hl_rp(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("add hl, {}", RP_STR[p])
}

pub fn disassemble_ld_bc_a(bytes: &[u8]) -> String {
    String::from("ld (bc), a")
}

pub fn disassemble_ld_de_a(bytes: &[u8]) -> String {
    String::from("ld (de), a")
}

pub fn disassemble_ld_hlp_a(bytes: &[u8]) -> String {
    String::from("ld (hl+), a")
}

pub fn disassemble_ld_hlm_a(bytes: &[u8]) -> String {
    String::from("ld (hl-), a")
}

pub fn disassemble_ld_a_bc(bytes: &[u8]) -> String {
    String::from("ld a, (bc)")
}

pub fn disassemble_ld_a_de(bytes: &[u8]) -> String {
    String::from("ld a, (de)")
}

pub fn disassemble_ld_a_hlp(bytes: &[u8]) -> String {
    String::from("ld a, (hl+)")
}

pub fn disassemble_ld_a_hlm(bytes: &[u8]) -> String {
    String::from("ld a, (hl-)")
}

pub fn disassemble_inc_rp(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("inc {}", RP_STR[p])
}

pub fn disassemble_dec_rp(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("inc {}", RP_STR[p])
}

pub fn disassemble_inc_r(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("inc {}", R_STR[y])
}

pub fn disassemble_dec_r(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("dec {}", R_STR[y])
}

pub fn disassemble_ld_r(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("ld {}, 0x{:x}", R_STR[y], bytes[1])
}

pub fn disassemble_rlca(bytes: &[u8]) -> String {
    String::from("rlca")
}

pub fn disassemble_rrca(bytes: &[u8]) -> String {
    String::from("rrca")
}

pub fn disassemble_rla(bytes: &[u8]) -> String {
    String::from("rla")
}

pub fn disassemble_rra(bytes: &[u8]) -> String {
    String::from("rra")
}

pub fn disassemble_daa(bytes: &[u8]) -> String {
    String::from("daa")
}

pub fn disassemble_cpl(bytes: &[u8]) -> String {
    String::from("cpl")
}

pub fn disassemble_scf(bytes: &[u8]) -> String {
    String::from("scf")
}

pub fn disassemble_ccf(bytes: &[u8]) -> String {
    String::from("ccf")
}

pub fn disassemble_ld_r_r(bytes: &[u8]) -> String {
    let (_, y, z, _, _) = extract_x_y_z_p_q(bytes[0]);
    format!("ld {}, {}", R_STR[y as usize], R_STR[z as usize])
}

pub fn disassemble_alu_r(bytes: &[u8]) -> String {
    let (_, y, z, _, _) = extract_x_y_z_p_q(bytes[0]);
    format!("{} {}", ALU_STR[y as usize], R_STR[z as usize])
}

pub fn disassemble_ret_cc(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("ret {}", CC_STR[y])
}

pub fn disassemble_ld_ff00_n_a(bytes: &[u8]) -> String {
    format!("ld 0xff00+0x{:x}, a", bytes[1])
}

pub fn disassemble_add_sp_d(bytes: &[u8]) -> String {
    format!("add sp, {}", bytes[1] as i8)
}

pub fn disassemble_ld_a_ff00_n(bytes: &[u8]) -> String {
    format!("ld a, 0xff00+0x{:x}", bytes[1])
}

pub fn disassemble_ld_hl_sp_d(bytes: &[u8]) -> String {
    format!("ld hl, sp+{}", bytes[1] as i8)
}

pub fn disassemble_pop_rp2(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("pop {}", RP2_STR[p])
}

pub fn disassemble_ret(bytes: &[u8]) -> String {
    String::from("ret")
}

pub fn disassemble_reti(bytes: &[u8]) -> String {
    String::from("reti")
}

pub fn disassemble_jp_hl(bytes: &[u8]) -> String {
    String::from("jp (hl)")
}

pub fn disassemble_ld_sp_hl(bytes: &[u8]) -> String {
    String::from("ld sp, hl")
}

pub fn disassemble_jp_cc_nn(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("jp {} {}", CC_STR[y], ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_ld_ff00_c_a(bytes: &[u8]) -> String {
    String::from("ld (0xff00+c), a")
}

pub fn disassemble_ld_nn_a(bytes: &[u8]) -> String {
    format!("ld ({:x}), a", ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_ld_a_ff00_c(bytes: &[u8]) -> String {
    String::from("ld a, (0xff00+c)")
}

pub fn disassemble_ld_a_nn(bytes: &[u8]) -> String {
    format!("ld a, ({:x})", ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_jp_nn(bytes: &[u8]) -> String {
    format!("jp {:x}", ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_call_cc_nn(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("call {} 0x{:x}", CC_STR[y], ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_push_rp2(bytes: &[u8]) -> String {
    let p = extract_x_y_z_p_q(bytes[0]).3 as usize;
    format!("push {}", RP2_STR[p])
}

pub fn disassemble_call_nn(bytes: &[u8]) -> String {
    format!("call 0x{:x}", ((bytes[2] as u16) << 8) | (0x00FF & (bytes[1] as u16)))
}

pub fn disassemble_alu_n(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1 as usize;
    format!("{} {:x}", ALU_STR[y], bytes[1])
}

pub fn disassemble_rst_y8(bytes: &[u8]) -> String {
    let y = extract_x_y_z_p_q(bytes[0]).1;
    format!("rst 0x{:x}", y * 8)
}

pub fn disassemble_prefixed(bytes: &[u8]) -> String {
    let (x, y, z, _, _) = extract_x_y_z_p_q(bytes[0]);

    match x {
        0 => {
            format!("{} {}", ROT_STR[y as usize], R_STR[z as usize])
        },
        1 => {
            format!("bit {}, {}", y, R_STR[z as usize])
        },
        2 => {
            format!("res {}, {}", y, R_STR[z as usize])
        },
        3 => {
            format!("set {}, {}", y, R_STR[z as usize])
        },
        _ => String::from("Unknown prefixed op")
    }
}