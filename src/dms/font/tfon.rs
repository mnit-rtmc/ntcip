// tfon.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
use super::{Font, FontError};
use fstr::FStr;
use std::io::{Read, Write};
use tfon::tfon::Parser;
use tfon::{Bitmap, Prop};

/// `.tfon` format error
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O {0}")]
    Io(#[from] std::io::Error),

    #[error("tfon: {0}")]
    Tfon(#[from] tfon::Error),

    #[error("height mismatch")]
    HeightMismatch(),

    #[error("Font: {0}")]
    Invalid(#[from] FontError),
}

/// Result type
type Result<T> = std::result::Result<T, Error>;

/// Read a font in `.tfon` format
pub fn read<const C: usize, R: Read>(mut reader: R) -> Result<Font<C>> {
    let mut buf = String::with_capacity(4096);
    reader.read_to_string(&mut buf)?;
    parse(&buf)
}

/// Parse a font in `.tfon` format
pub fn parse<const C: usize>(buf: &str) -> Result<Font<C>> {
    let mut font = Font::default();
    let mut row = 0;
    for prop in Parser::new(buf) {
        match prop {
            Prop::FontName(nm) => font.name = FStr::from_str_lossy(nm, b'\0'),
            Prop::FontNumber(num) => font.number = num,
            Prop::CharSpacing(cs) => font.char_spacing = cs,
            Prop::LineSpacing(ls) => font.line_spacing = ls,
            Prop::CodePoint(cp) => font.characters[row].number = cp,
            Prop::Bitmap(bmap) => {
                if font.height > 0 {
                    if bmap.height() != font.height {
                        return Err(Error::HeightMismatch());
                    }
                } else {
                    font.height = bmap.height();
                }
                font.characters[row].width = bmap.width();
                font.characters[row].bitmap = bmap.into_bits();
                row += 1;
            }
            _ => (),
        }
    }
    Ok(font)
}

/// Write a font to an `.tfon` file
pub fn write<const C: usize, W: Write>(
    writer: W,
    font: &Font<C>,
) -> Result<()> {
    font.validate()?;
    let mut props = Vec::with_capacity(128);
    props.push(Prop::FontName(font.name.as_str()));
    props.push(Prop::FontNumber(font.number));
    props.push(Prop::CharSpacing(font.char_spacing));
    props.push(Prop::LineSpacing(font.line_spacing));
    let height = font.height;
    for ch in &font.characters {
        if ch.number > 0 {
            if let Some(bmap) =
                Bitmap::from_bits(height, ch.width, ch.bitmap.clone())
            {
                props.push(Prop::CodePoint(ch.number));
                props.push(Prop::Bitmap(bmap));
            }
        }
    }
    tfon::tfon::write(writer, props.into_iter())?;
    Ok(())
}
