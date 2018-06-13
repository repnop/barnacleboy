mod cpu;
mod memory;
mod rom;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    println!("Reading {:?}...", file);
    println!("{:#?}", rom::GameBoyCartridge::from_file(file));
}
