// tfon.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
use super::{CharacterEntry, Font, FontError};
use std::io::{Read, Write};
use std::str::Lines;

/// Symbols for all ASCII + Latin 1 characters
const SYMBOL: &[&str] = &[
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF",
    "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK",
    "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP", "!",
    "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
    "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]",
    "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
    "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{",
    "|", "}", "~", "DEL", "PAD", "HOP", "BPH", "NBH", "IND", "NEL", "SSA",
    "ESA", "HTS", "HTJ", "LTS", "PLD", "PLU", "RI", "SS2", "SS3", "DCS", "PU1",
    "PU2", "STS", "CCH", "MW", "SPA", "EPA", "SOS", "SGCI", "SCI", "CSI", "ST",
    "OSC", "PM", "APC", "NBSP", "¡", "¢", "£", "¤", "¥", "¦", "§", "¨", "©",
    "ª", "«", "¬", "SHY", "®", "¯", "°", "±", "²", "³", "´", "µ", "¶", "·",
    "¸", "¹", "º", "»", "¼", "½", "¾", "¿", "À", "Á", "Â", "Ã", "Ä", "Å", "Æ",
    "Ç", "È", "É", "Ê", "Ë", "Ì", "Í", "Î", "Ï", "Ð", "Ñ", "Ò", "Ó", "Ô", "Õ",
    "Ö", "×", "Ø", "Ù", "Ú", "Û", "Ü", "Ý", "Þ", "ß", "à", "á", "â", "ã", "ä",
    "å", "æ", "ç", "è", "é", "ê", "ë", "ì", "í", "î", "ï", "ð", "ñ", "ò", "ó",
    "ô", "õ", "ö", "÷", "ø", "ù", "ú", "û", "ü", "ý", "þ", "ÿ",
];

/// `.tfon` format error
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O {0}")]
    Io(#[from] std::io::Error),

    #[error("Parse int: {0}")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("Parse: {0} on line {1}")]
    Parse(&'static str, usize),

    #[error("Unexpected end")]
    UnexpectedEnd,

    #[error("Font: {0}")]
    Invalid(#[from] FontError),
}

/// Result type
type Result<T> = std::result::Result<T, Error>;

/// Tfon file line iterator
struct TfonIter<'a> {
    lines: Lines<'a>,
    line: Option<&'a str>,
    line_num: usize,
}

/// Character row
struct Row<'a> {
    line: &'a str,
    width: u8,
    line_num: usize,
}

/// Character bitmap
#[derive(Clone, Debug, Default)]
struct Bitmap {
    /// Bytes in character
    bytes: Vec<u8>,
    /// Last byte
    last: u8,
    /// Count of bits
    count: usize,
}

/// Read a font in `.tfon` format
pub fn read<R: Read>(mut reader: R) -> Result<Font> {
    let mut buf = String::with_capacity(4096);
    reader.read_to_string(&mut buf)?;
    parse(&buf)
}

/// Parse a font in `.tfon` format
pub fn parse<const C: usize>(buf: &str) -> Result<Font<C>> {
    let mut lines = TfonIter::new(buf);
    let name = lines.parse_str("name")?;
    let number = lines.parse_u8("font_number")?;
    let char_spacing = lines.parse_u8("char_spacing")?;
    let line_spacing = lines.parse_u8("line_spacing")?;
    let mut height = 0;
    // workaround const generic default limitation
    let mut characters: [CharacterEntry; C] =
        [(); C].map(|_| CharacterEntry::default());
    let mut row = 0;
    while lines.peek_line().is_some() {
        let (ch, h) = lines.read_char()?;
        if height > 0 {
            if h != height {
                return Err(Error::Parse("height", lines.line_num));
            }
        } else {
            height = h;
        }
        characters[row] = ch;
        row += 1;
    }
    let font = Font {
        number,
        name,
        height,
        char_spacing,
        line_spacing,
        characters,
    };
    Ok(font)
}

impl<'a> Iterator for TfonIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line.is_some() {
            self.line.take()
        } else {
            for line in self.lines.by_ref() {
                self.line_num += 1;
                if !line.is_empty() {
                    return Some(line);
                }
            }
            None
        }
    }
}

impl<'a> TfonIter<'a> {
    /// Create a new tfon iterator
    fn new(buf: &'a str) -> Self {
        let lines = buf.lines();
        TfonIter {
            lines,
            line: None,
            line_num: 0,
        }
    }

    /// Get the next line
    fn next_line(&mut self) -> Result<&'a str> {
        self.next().ok_or(Error::UnexpectedEnd)
    }

    /// Peek the next line
    fn peek_line(&mut self) -> Option<&'a str> {
        if self.line.is_none() {
            for line in self.lines.by_ref() {
                self.line_num += 1;
                if !line.is_empty() {
                    self.line = Some(line);
                    break;
                }
            }
        }
        self.line
    }

    /// Parse a key/value pair
    fn parse_kv(&mut self, key: &'static str) -> Result<&str> {
        let line = self.next_line()?;
        if let Some((ky, val)) = line.split_once(": ") {
            if ky == key {
                return Ok(val);
            }
        }
        Err(Error::Parse(key, self.line_num))
    }

    /// Parse a key/value as string
    fn parse_str(&mut self, key: &'static str) -> Result<String> {
        Ok(self.parse_kv(key)?.to_string())
    }

    /// Parse a key/value as u8
    fn parse_u8(&mut self, key: &'static str) -> Result<u8> {
        Ok(self.parse_kv(key)?.parse()?)
    }

    /// Parse a character line
    fn parse_ch(&mut self) -> Result<u16> {
        let value = self.parse_kv("ch")?;
        if let Some((val, symbol)) = value.split_once(' ') {
            let cp = val.parse()?;
            if symbol == SYMBOL[usize::from(cp)] {
                return Ok(cp);
            }
        }
        Err(Error::Parse("ch", self.line_num))
    }

    /// Check if the next line is a character row
    fn is_row(&mut self) -> bool {
        if let Some(line) = self.peek_line() {
            if let Ok(row) = Row::new(line, self.line_num) {
                return row.pixels().all(|p| p.is_ok());
            }
        }
        false
    }

    /// Parse a row of pixels
    fn parse_row(&mut self, width: Option<u8>) -> Result<Row> {
        let line = self.next_line()?;
        let row = Row::new(line, self.line_num)?;
        if let Some(width) = width {
            if width != row.width {
                return Err(Error::Parse("width", self.line_num));
            }
        }
        Ok(row)
    }

    /// Read a character entry
    fn read_char(&mut self) -> Result<(CharacterEntry, u8)> {
        let number = self.parse_ch()?;
        let row = self.parse_row(None)?;
        let width = row.width;
        let mut height = 1;
        let mut bitmap = Bitmap::default();
        bitmap.push_bits(row)?;
        while self.is_row() {
            let row = self.parse_row(Some(width))?;
            bitmap.push_bits(row)?;
            height += 1;
        }
        let bitmap = Vec::<u8>::from(bitmap);
        Ok((
            CharacterEntry {
                number,
                width,
                bitmap,
            },
            height,
        ))
    }
}

impl<'a> Row<'a> {
    fn new(line: &'a str, line_num: usize) -> Result<Self> {
        let width = u8::try_from(line.len())
            .or(Err(Error::Parse("width", line_num)))?;
        Ok(Row {
            line,
            width,
            line_num,
        })
    }

    fn pixels(&'a self) -> impl Iterator<Item = Result<bool>> + 'a {
        self.line.chars().map(|c| match c {
            '.' => Ok(false),
            '@' => Ok(true),
            _ => Err(Error::Parse("bitmap", self.line_num)),
        })
    }
}

impl Bitmap {
    fn push_bits(&mut self, row: Row) -> Result<()> {
        for bit in row.pixels() {
            if bit? {
                self.last |= 1 << (7 - (self.count & 0b111));
            }
            self.count += 1;
            if self.count & 0b111 == 0 {
                self.bytes.push(self.last);
                self.last = 0;
            }
        }
        Ok(())
    }
}

impl From<Bitmap> for Vec<u8> {
    fn from(bitmap: Bitmap) -> Self {
        let mut bytes = bitmap.bytes;
        if bitmap.count & 0b111 != 0 {
            bytes.push(bitmap.last);
        }
        bytes
    }
}

/// Write a font to an `.tfon` file
pub fn write<W: Write>(mut writer: W, font: &Font) -> Result<()> {
    font.validate()?;
    writeln!(writer, "name: {}", font.name)?;
    writeln!(writer, "font_number: {}", font.number)?;
    writeln!(writer, "char_spacing: {}", font.char_spacing)?;
    writeln!(writer, "line_spacing: {}", font.line_spacing)?;
    for character in &font.characters {
        let cp = character.number;
        if let Some(symbol) = SYMBOL.get(usize::from(cp)) {
            writeln!(writer)?;
            writeln!(writer, "ch: {cp} {symbol}")?;
            for row in 0..usize::from(font.height) {
                for col in 0..usize::from(character.width) {
                    if character.is_pixel_lit(row, col) {
                        write!(writer, "@")?;
                    } else {
                        write!(writer, ".")?;
                    }
                }
                writeln!(writer)?;
            }
        }
    }
    Ok(())
}
