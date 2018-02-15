extern crate clap;
extern crate minifb;

mod cpu;
mod rom;
mod mmu;
mod gpu;
mod tests;
mod gameboy;
#[macro_use]
mod debugger;
mod constants;
mod interconnect;

use clap::{App, Arg};
use gameboy::Gameboy;

fn main() {
    let arguments = App::new("barnacleboy")
        .version("0.1.0")
        .author("repnop <repnop@outlook.com>")
        .about("Rust Gameboy Emulator")
        .arg(Arg::with_name("bootrom")
                 .short("b")
                 .value_name("BOOTROM")
                 .help("Specifies the BOOTROM file to load")
                 .takes_value(true))
        .arg(Arg::with_name("rom")
                 .short("r")
                 .value_name("ROM")
                 .help("Specifies the ROM file to load")
                 .takes_value(true))
        .arg(Arg::with_name("debug")
                 .short("d")
                 .help("Specifies whether to run in debug mode or not")
                 .takes_value(false))
        .arg(Arg::with_name("noverify")
                 .short("n")
                 .help("Specifies to not checksum the ROM")
                 .takes_value(false))
        .get_matches();

    let mut debug = false;
    let mut verify = true;
    let dmg;
    let rom;

    if let Some(r) = arguments.value_of("bootrom") {
        dmg = r;
    } else {
        println!("No BOOTROM file specified, exiting...");
        return;
    }

    if let Some(r) = arguments.value_of("rom") {
        rom = r;
    } else {
        println!("No ROM file specified, exiting...");
        return;
    }

    if arguments.occurrences_of("debug") > 0 {
        debug = true;
    }

    if arguments.occurrences_of("noverify") > 0 {
        verify = false;
    }

    let result = Gameboy::new(dmg.to_string(), rom.to_string(), debug, verify);

    if let Ok(mut gameboy) = result {
        gameboy.run();
    } else if let Err(err) = result {
        println!("{}", err);
    }
}