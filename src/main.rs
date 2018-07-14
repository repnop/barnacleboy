mod cpu;
mod memory;
mod rom;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    println!("Reading {:?}...", file);
    let cart = rom::GameBoyCartridge::from_file(file).unwrap();
    let contents = cart.contents.clone();
    println!("{:#?}\n\n", cart.header);
    let mem_ctrlr = memory::Mbc1::from_rom(cart);
    //println!("{:X?}", mem_ctrlr);
}
