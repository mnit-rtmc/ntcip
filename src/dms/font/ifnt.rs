// ifnt.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
use super::{CharacterEntry, Font};
use std::io::{BufRead, BufReader, Lines, Read, Write};

/// Font error
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O {0}")]
    Io(#[from] std::io::Error),

    #[error("Parse int: {0}")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("Parse: {0}")]
    Parse(&'static str),

    #[error("Unexpected end")]
    UnexpectedEnd,

    #[error("Invalid font")]
    InvalidFont,
}

/// Result type
type Result<T> = std::result::Result<T, Error>;

/// Character row
struct Row<'a> {
    line: &'a str,
    width: u8,
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

/// Read a font in .ifnt format
pub fn read<R: Read>(reader: R) -> Result<Font> {
    let mut lines = BufReader::new(reader).lines();
    let name = parse_string(&next_line(&mut lines)?, "name")?;
    let number = parse_u8(&next_line(&mut lines)?, "font_number")?;
    let height = parse_u8(&next_line(&mut lines)?, "height")?;
    let _width = parse_u8(&next_line(&mut lines)?, "width")?;
    let char_spacing = parse_u8(&next_line(&mut lines)?, "char_spacing")?;
    let line_spacing = parse_u8(&next_line(&mut lines)?, "line_spacing")?;
    let mut font = Font {
        number,
        name,
        height,
        char_spacing,
        line_spacing,
        ..Default::default()
    };
    while let Some(ch) = read_character(&mut lines, height)? {
        font.characters.push(ch);
    }
    Ok(font)
}

/// Read a character entry
fn read_character<R: BufRead>(
    lines: &mut Lines<R>,
    height: u8,
) -> Result<Option<CharacterEntry>> {
    let Ok(num_line) = next_line(lines) else {
        return Ok(None);
    };
    let number = parse_cp(&num_line)?;
    let line = next_line(lines)?;
    let row = parse_row(&line)?;
    let width = row.width;
    let mut bitmap = Bitmap::default();
    bitmap.push_bits(row)?;
    for _ in 1..height {
        let line = next_line(lines)?;
        let row = parse_row(&line)?;
        if row.width != width {
            return Err(Error::Parse("width"));
        }
        bitmap.push_bits(row)?;
    }
    let bitmap = Vec::<u8>::from(bitmap);
    Ok(Some(CharacterEntry {
        number,
        width,
        bitmap,
    }))
}

/// Get the next non-empty line
fn next_line<R: BufRead>(lines: &mut Lines<R>) -> Result<String> {
    for line in lines {
        let line = line?;
        if !line.is_empty() {
            return Ok(line);
        }
    }
    Err(Error::UnexpectedEnd)
}

/// Parse a value from an .ifnt file
fn parse_kv<'a>(line: &'a str, key: &'static str) -> Result<&'a str> {
    if let Some((ky, val)) = line.split_once(": ") {
        if ky == key {
            return Ok(val);
        }
    }
    Err(Error::Parse(key))
}

/// Parse a string value from an .ifnt file
fn parse_string(line: &str, key: &'static str) -> Result<String> {
    Ok(parse_kv(line, key)?.to_string())
}

/// Parse a u8 value from an .ifnt file
fn parse_u8(line: &str, key: &'static str) -> Result<u8> {
    Ok(parse_kv(line, key)?.parse()?)
}

/// Parse a codepoint from an .ifnt file
fn parse_cp(line: &str) -> Result<u16> {
    let value = parse_kv(line, "codepoint")?;
    if let Some((val, ch)) = value.split_once(' ') {
        let cp = val.parse()?;
        if ch.len() == 1 && ch.chars().next() == char::from_u32(u32::from(cp)) {
            return Ok(cp);
        }
    }
    Err(Error::Parse("codepoint"))
}

/// Parse a row of pixels
fn parse_row(line: &str) -> Result<Row> {
    let width = u8::try_from(line.len()).or(Err(Error::Parse("width")))?;
    Ok(Row { line, width })
}

impl<'a> Row<'a> {
    fn pixels(&'a self) -> impl Iterator<Item = Result<bool>> + 'a {
        self.line.chars().map(|c| match c {
            '.' => Ok(false),
            'X' => Ok(true),
            _ => Err(Error::Parse("bitmap")),
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

/// Write a font to an .ifnt file
pub fn write<W: Write>(mut writer: W, font: &Font) -> Result<()> {
    if !font.is_valid() {
        return Err(Error::InvalidFont);
    }
    writeln!(writer, "name: {}", font.name)?;
    writeln!(writer, "font_number: {}", font.number)?;
    writeln!(writer, "height: {}", font.height)?;
    writeln!(writer, "width: {}", font.width())?;
    writeln!(writer, "char_spacing: {}", font.char_spacing)?;
    writeln!(writer, "line_spacing: {}", font.line_spacing)?;
    for character in &font.characters {
        let cp = u32::from(character.number);
        if let Some(ch) = char::from_u32(cp) {
            writeln!(writer)?;
            writeln!(writer, "codepoint: {cp} {ch}")?;
            for row in 0..usize::from(font.height) {
                for col in 0..usize::from(character.width) {
                    if character.is_pixel_lit(row, col) {
                        write!(writer, "X")?;
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
