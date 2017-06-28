use std::convert::{From, Into};

pub struct RomHeader {
    pub entry_point: [u8; 0x4],
    pub logo: [u8; 0x30],
    pub title: [u8; 0x10],
    pub manufacturer_code: [u8; 0x4],
    pub cgb_flag: u8,
    pub new_licensee_code: [u8; 0x2],
    pub sgb_flag: u8,
    pub cartridge_type: u8,
    pub rom_size: u8,
    pub ram_size: u8,
    pub destination_code: u8,
    pub old_licensee_code: u8,
    pub mask_rom_version_number: u8,
    pub header_checksum: u8,
    pub global_checksum: u16
}

impl Default for RomHeader {
    fn default() -> RomHeader {
        RomHeader {
            entry_point: [0x00, 0x00, 0x00, 0x00],
            logo: [0x00; 0x30],
            title: [0x00; 0x10],
            manufacturer_code: [0x00, 0x00, 0x00, 0x00],
            cgb_flag: 0x00,
            new_licensee_code: [0x00, 0x00],
            sgb_flag: 0x00,
            cartridge_type: 0x00,
            rom_size: 0x00,
            ram_size: 0x00,
            destination_code: 0x00,
            old_licensee_code: 0x00,
            mask_rom_version_number: 0x00,
            header_checksum: 0x00,
            global_checksum: 0x0000
        }
    }
}

impl PartialEq for RomHeader {
    fn eq(&self, other: &RomHeader) -> bool {
        (&self.entry_point[..] == &other.entry_point[..]) &&
        (&self.logo[..] == &other.logo[..]) &&
        (&self.title[..] == &other.title[..]) &&
        (&self.manufacturer_code[..] == &other.manufacturer_code[..]) &&
        (self.cgb_flag == other.cgb_flag) &&
        (&self.new_licensee_code[..] == &other.new_licensee_code[..]) &&
        (self.sgb_flag == other.sgb_flag) &&
        (self.cartridge_type == other.cartridge_type) &&
        (self.rom_size == other.rom_size) &&
        (self.ram_size == other.ram_size) &&
        (self.destination_code == other.destination_code) &&
        (self.old_licensee_code == other.old_licensee_code) &&
        (self.mask_rom_version_number == other.mask_rom_version_number) &&
        (self.header_checksum == other.header_checksum) &&
        (self.global_checksum == other.global_checksum)
    }
}

impl<'a> From<&'a [u8]> for RomHeader {
    fn from(bytes: &'a [u8]) -> RomHeader {
        let mut header = RomHeader::default();

        if bytes.len() >= 0x50 {
            header.entry_point.copy_from_slice(&bytes[0x0..0x4]);
            header.logo.copy_from_slice(&bytes[0x4..0x34]);
            header.title.copy_from_slice(&bytes[0x34..0x44]);
            header.manufacturer_code.copy_from_slice(&bytes[0x3F..0x43]);
            header.cgb_flag = bytes[0x43];
            header.new_licensee_code.copy_from_slice(&bytes[0x44..0x46]);
            header.sgb_flag = bytes[0x46];
            header.cartridge_type = bytes[0x47];
            header.rom_size = bytes[0x48];
            header.ram_size = bytes[0x49];
            header.destination_code = bytes[0x4A];
            header.old_licensee_code = bytes[0x4B];
            header.mask_rom_version_number = bytes[0x4C];
            header.header_checksum = bytes[0x4D];
            header.global_checksum = ((bytes[0x4E] as u16) << 8) | bytes[0x4F] as u16;
        }

        header
    }
}

impl Into<[u8; 0x50]> for RomHeader {
    fn into(self) -> [u8; 0x50] {
        let mut array = [0u8; 0x50];
        let mut index = 0usize;
        
        for byte in self.entry_point.iter() {
            array[index] = *byte;
            index += 1;
        }

        for byte in self.logo.iter() {
            array[index] = *byte;
            index += 1;
        }

        for byte in self.title.iter() {
            array[index] = *byte;
            index += 1;
        }

        for byte in self.new_licensee_code.iter() {
            array[index] = *byte;
            index += 1;
        }

        array[index] = self.sgb_flag;
        index += 1;

        array[index] = self.cartridge_type;
        index += 1;

        array[index] = self.rom_size;
        index += 1;

        array[index] = self.ram_size;
        index += 1;

        array[index] = self.destination_code;
        index += 1;

        array[index] = self.old_licensee_code;
        index += 1;

        array[index] = self.mask_rom_version_number;
        index += 1;

        array[index] = self.header_checksum;
        index += 1;

        array[index] = (self.global_checksum >> 8) as u8;
        index += 1;

        array[index] = (self.global_checksum & 0x00FF) as u8;

        array
    }
}

pub struct Rom {
    pub ivt: [u8; 0x100],
    pub header: RomHeader,
    pub banks: Vec<u8>,
}

impl Rom {
    pub fn new(bytes: &[u8]) -> Rom {
        let mut rom = Rom {
            ivt: [0; 0x100],
            header: RomHeader::default(),
            banks: Vec::new()
        };

        rom.ivt.copy_from_slice(&bytes[..0x100]);
        rom.header = RomHeader::from(&bytes[0x100..0x150]);
        rom.banks.extend_from_slice(&bytes[0x150..]);

        rom
    }
}

pub struct BootRom(pub [u8; 0x100]);

impl BootRom {
    pub fn new(bytes: &[u8]) -> BootRom {
        let mut br = BootRom([0; 0x100]);

        br.0.copy_from_slice(&bytes[..]);

        br
    }
}

#[test]
fn test_ivt() {
    use std::fs::File;
    use std::env;
    use std::io::Read;

    // If we can't find the ROM, just assume the test passes.
    // Makes it so TravisCI wont say the build failed because it
    //  doesn't have access to the ROMs
    if let Err(_) = env::var("ROM_PATH") {
        assert!(true);
    } else {
        let mut file = File::open(env::var("ROM_PATH").expect("ROM_PATH")).expect("ROM not found");

        let mut bytes: Vec<u8> = Vec::new();

        let _ = file.read_to_end(&mut bytes);

        let ivt = [0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xC3, 0x24, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xC3, 0x06, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC3, 0x25, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0xD9, 0xAF, 0xE0, 0x0F, 0xF0, 0xFF, 0x47, 0xCB, 0x87, 0xE0, 0xFF, 0xF0, 0x44, 0xFE, 0x91, 0x20, 
                0xFA, 0xF0, 0x40, 0xE6, 0x7F, 0xE0, 0x40, 0x78, 0xE0, 0xFF, 0xC9, 0xF0, 0x40, 0xCB, 0xFF, 0xE0, 
                0x40, 0xC9, 0xAF, 0x21, 0x00, 0xC3, 0x06, 0xA0, 0x22, 0x05, 0x20, 0xFC, 0xC9, 0x3E, 0xA0, 0x21, 
                0x00, 0xC3, 0x11, 0x04, 0x00, 0x06, 0x28, 0x77, 0x19, 0x05, 0x20, 0xFB, 0xC9, 0xEA, 0xE9, 0xCE, 
                0xF0, 0xB8, 0xF5, 0xFA, 0xE9, 0xCE, 0xE0, 0xB8, 0xEA, 0x00, 0x20, 0xCD, 0xB5, 0x00, 0xF1, 0xE0, 
                0xB8, 0xEA, 0x00, 0x20, 0xC9, 0x2A, 0x12, 0x13, 0x0B, 0x79, 0xB0, 0x20, 0xF8, 0xC9, 0x00, 0x00, 
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];

        let rom = Rom::new(&bytes[..]);

        let mut index = 0usize;

        while index < 0x100 {
            assert!(rom.ivt[index] == ivt[index]);
            index += 1;
        }
    }
}

#[test]
fn test_header() {
    use std::fs::File;
    use std::env;
    use std::io::Read;

    // If we can't find the ROM, just assume the test passes.
    // Makes it so TravisCI wont say the build failed because it
    //  doesn't have access to the ROMs
    if let Err(_) = env::var("ROM_PATH") {
        assert!(true);
    } else {
        let mut file = File::open(env::var("ROM_PATH").expect("ROM_PATH")).expect("ROM not found");

        let mut bytes: Vec<u8> = Vec::new();

        let _ = file.read_to_end(&mut bytes);

        let header = RomHeader::from(&bytes[0x100..]);

        let header2 = RomHeader {
            entry_point: [0x00, 0xC3, 0x50, 0x01],
            logo: [0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 
                0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 
                0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 
                0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 
                0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 
                0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E],
            title: [0x50, 0x4F, 0x4B, 0x45, 0x4D, 0x4F, 0x4E, 0x20, 
                    0x52, 0x45, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00],
            manufacturer_code: [0x00, 0x00, 0x00, 0x00],
            cgb_flag: 0x00,
            new_licensee_code: [0x30, 0x31],
            sgb_flag: 0x03,
            cartridge_type: 0x13,
            rom_size: 0x05,
            ram_size: 0x03,
            destination_code: 0x01,
            old_licensee_code: 0x33,
            mask_rom_version_number: 0x00,
            header_checksum: 0x20,
            global_checksum: 0x91E6
        };

        assert!(header == header2);

        let headerbytes: [u8; 0x50] = header.into();

        let mut index = 0usize;

        while index < 0x50 {
            assert!(headerbytes[index] == bytes[0x100 + index]);
            index += 1;
        }
    }
}