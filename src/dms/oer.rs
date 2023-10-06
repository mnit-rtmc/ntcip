// oer.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
//! OER encoding

/// OER encoder
pub struct Oer(Vec<u8>);

impl From<Vec<u8>> for Oer {
    fn from(vec: Vec<u8>) -> Self {
        Oer(vec)
    }
}

impl From<Oer> for Vec<u8> {
    fn from(oer: Oer) -> Self {
        oer.0
    }
}

impl Oer {
    /// Integer constrained to 0 - 255
    pub fn u8(&mut self, v: u8) {
        self.0.push(v);
    }

    /// Integer constrained to 0 - 65_535
    pub fn u16(&mut self, v: u16) {
        self.0.push((v >> 8) as u8);
        self.0.push(v as u8);
    }

    // idk, is this even a thing?
    fn u24(&mut self, v: u32) {
        self.0.push((v >> 16) as u8);
        self.0.push((v >> 8) as u8);
        self.0.push(v as u8);
    }

    /// Integer constrained to 0 - 4_294_967_295
    pub fn u32(&mut self, v: u32) {
        self.0.push((v >> 24) as u8);
        self.0.push((v >> 16) as u8);
        self.0.push((v >> 8) as u8);
        self.0.push(v as u8);
    }

    /// Encode a length prefix
    fn length_prefix(&mut self, len: u32) {
        if len <= 0x7F {
            self.0.push(len as u8);
        } else if len <= 0xFF {
            self.0.push(128 | 1);
            self.u8(len as u8);
        } else if len <= 0xFFFF {
            self.0.push(128 | 2);
            self.u16(len as u16);
        } else if len <= 0xFFFFFF {
            self.0.push(128 | 3);
            self.u24(len);
        } else {
            self.0.push(128 | 4);
            self.u32(len);
        }
    }

    // Unsigned integer (unconstrained)
    pub fn uint(&mut self, v: u32) {
        if v <= 0xFF {
            self.length_prefix(1);
            self.u8(v as u8);
        } else if v <= 0xFFFF {
            self.length_prefix(2);
            self.u16(v as u16);
        } else if v <= 0xFFFFFF {
            self.length_prefix(3);
            self.u24(v);
        } else {
            self.length_prefix(4);
            self.u32(v);
        }
    }

    /// Octet string (unconstrained)
    pub fn octet_string(&mut self, octets: &[u8]) {
        self.length_prefix(octets.len() as u32);
        self.0.extend(octets);
    }

    /// Octet string (constrained)
    pub fn octet_str(&mut self, octets: &[u8]) {
        self.0.extend(octets);
    }
}
