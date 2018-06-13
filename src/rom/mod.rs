use std::{fs, io::Read, path::Path};

#[derive(Debug)]
pub struct GameBoyCartridge {
    header: CartridgeHeader,
}

impl GameBoyCartridge {
    pub fn from_file(file: impl AsRef<Path>) -> Result<GameBoyCartridge, Box<::std::error::Error>> {
        const BANK_SIZE: usize = 8 * 1024;

        let bytes = fs::read(file)?;

        let mut title = String::new();

        for &b in &bytes[0x134..=0x143] {
            if b != 0 {
                title.push(b as char);
            }
        }

        let new_licensee_code = [bytes[0x144] as char, bytes[0x145] as char];

        let sgb_support = if bytes[0x146] == 0x03 { true } else { false };

        let bank_type = MemoryBankType::from_byte(bytes[0x147]).unwrap();

        let rom_size = if bytes[0x148] <= 0x07 {
            (32 * 1024) << (bytes[0x148] as usize)
        } else {
            match bytes[0x148] {
                0x52 => 72 * BANK_SIZE,
                0x53 => 80 * BANK_SIZE,
                0x54 => 96 * BANK_SIZE,
                _ => panic!("Unknown ROM size"),
            }
        };

        let ram_size = match bytes[0x149] {
            0x00 => None,
            0x01 => Some(2 * 1024),
            0x02 => Some(8 * 1024),
            0x03 => Some(32 * 1024),
            _ => panic!("Unknown RAM size"),
        };

        let destination = if bytes[0x14A] == 0 {
            Destination::Japan
        } else {
            Destination::AnywhereElse
        };

        let old_licensee_code = bytes[0x14B];

        let version_number = bytes[0x14C];

        let header_checksum = bytes[0x14D];

        let global_checksum = ((bytes[0x14E] as u16) << 8) | bytes[0x14F] as u16;

        Ok(GameBoyCartridge {
            header: CartridgeHeader {
                title,
                new_licensee_code,
                sgb_support,
                bank_type,
                rom_size,
                ram_size,
                destination,
                old_licensee_code,
                version_number,
                header_checksum,
                global_checksum,
            },
        })
    }
}

#[derive(Debug)]
pub struct CartridgeHeader {
    title: String,
    new_licensee_code: [char; 2],
    sgb_support: bool,
    bank_type: MemoryBankType,
    rom_size: usize,
    ram_size: Option<usize>,
    destination: Destination,
    old_licensee_code: u8,
    version_number: u8,
    header_checksum: u8,
    global_checksum: u16,
}

#[derive(Debug)]
pub enum MemoryBankType {
    RomOnly {
        ram: bool,
        battery: bool,
    },
    MBC1 {
        ram: bool,
        battery: bool,
    },
    MBC2 {
        battery: bool,
    },
    MMM01 {
        ram: bool,
        battery: bool,
    },
    MBC3 {
        timer: bool,
        ram: bool,
        battery: bool,
    },
    MBC4 {
        ram: bool,
        battery: bool,
    },
    MBC5 {
        ram: bool,
        battery: bool,
        rumble: bool,
    },
    PocketCamera,
    BandaiTama5,
    HuC3,
    HuC1,
}

impl MemoryBankType {
    pub fn from_byte(b: u8) -> Result<MemoryBankType, ()> {
        use self::MemoryBankType::*;

        let ty = match b {
            0x00 => RomOnly {
                ram: false,
                battery: false,
            },
            0x01 => MBC1 {
                ram: false,
                battery: false,
            },
            0x02 => MBC1 {
                ram: true,
                battery: false,
            },
            0x03 => MBC1 {
                ram: true,
                battery: true,
            },
            0x05 => MBC2 { battery: false },
            0x06 => MBC2 { battery: true },
            0x08 => RomOnly {
                ram: true,
                battery: false,
            },
            0x09 => RomOnly {
                ram: true,
                battery: true,
            },
            0x0B => MMM01 {
                ram: false,
                battery: false,
            },
            0x0C => MMM01 {
                ram: true,
                battery: false,
            },
            0x0D => MMM01 {
                ram: true,
                battery: true,
            },
            0x0F => MBC3 {
                ram: false,
                battery: true,
                timer: true,
            },
            0x10 => MBC3 {
                ram: true,
                battery: true,
                timer: true,
            },
            0x11 => MBC3 {
                ram: false,
                battery: false,
                timer: false,
            },
            0x12 => MBC3 {
                ram: true,
                battery: false,
                timer: false,
            },
            0x13 => MBC3 {
                ram: true,
                battery: true,
                timer: false,
            },
            0x15 => MBC4 {
                ram: false,
                battery: false,
            },
            0x16 => MBC4 {
                ram: true,
                battery: false,
            },
            0x17 => MBC4 {
                ram: true,
                battery: true,
            },
            0x19 => MBC5 {
                ram: false,
                battery: false,
                rumble: false,
            },
            0x1A => MBC5 {
                ram: true,
                battery: false,
                rumble: false,
            },
            0x1B => MBC5 {
                ram: true,
                battery: true,
                rumble: false,
            },
            0x1C => MBC5 {
                ram: false,
                battery: false,
                rumble: true,
            },
            0x1D => MBC5 {
                ram: true,
                battery: false,
                rumble: true,
            },
            0x1E => MBC5 {
                ram: true,
                battery: true,
                rumble: true,
            },
            0xFC => PocketCamera,
            0xFD => BandaiTama5,
            0xFE => HuC3,
            0xFF => HuC1,
            _ => return Err(()),
        };

        Ok(ty)
    }
}

#[derive(Debug)]
pub enum Destination {
    Japan,
    AnywhereElse,
}
