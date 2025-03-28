// font.rs
//
// Copyright (C) 2018-2025  Minnesota Department of Transportation
//
//! Font support for dynamic message signs
use crate::dms::oer::Oer;
use crc::Crc;
use fstr::FStr;
use log::debug;
use pix::{Raster, rgb::SRgb8};

/// Read/write fonts in `.tfon` format
pub mod tfon;

/// CRC-16 for calculating `fontVersionId`
const CRC: Crc<u16> = Crc::<u16>::new(&crc::CRC_16_IBM_SDLC);

/// Font error
#[derive(Debug, thiserror::Error)]
pub enum FontError {
    #[error("Invalid number")]
    InvalidNumber,

    #[error("Duplicate number: {0}")]
    DuplicateNumber(u8),

    #[error("Invalid character: {0}")]
    InvalidChar(char),

    #[error("Duplicate character: {0}")]
    DuplicateChar(char),

    #[error("Invalid height")]
    InvalidHeight,

    #[error("Invalid character height")]
    InvalidCharHeight,
}

/// Result type
type Result<T> = std::result::Result<T, FontError>;

/// Character for a bitmap [font]
///
/// [font]: struct.Font.html
#[derive(Clone, Default)]
pub struct CharacterEntry {
    /// Character number (code point) — `characterNumber`
    pub number: u16,
    /// Width (pixels) — `characterWidth`
    pub width: u8,
    /// Bitmap data (by rows) — `characterBitmap`
    pub bitmap: Vec<u8>,
}

/// Bitmap font — `fontEntry`
#[derive(Clone)]
pub struct Font<const C: usize = 256> {
    /// Font number — `fontNumber`
    pub number: u8,
    /// Name of font — `fontName`
    pub name: FStr<64>,
    /// Height (pixels) — `fontHeight`
    pub height: u8,
    /// Default pixel spacing between characters — `fontCharSpacing`
    pub char_spacing: u8,
    /// Default pixel spacing between lines — `fontLineSpacing`
    pub line_spacing: u8,
    /// Characters in font — `characterTable`
    pub characters: [CharacterEntry; C],
}

/// Table of fonts
///
/// This represents the `fontDefinition` of a DMS.
#[derive(Clone)]
pub struct FontTable<const C: usize = 256, const F: usize = 24> {
    /// Fonts in table
    fonts: [Font<C>; F],
}

impl CharacterEntry {
    /// Check if character is valid
    fn is_valid(&self, height: u8) -> bool {
        self.number == 0 || {
            let bits = usize::from(self.width) * usize::from(height);
            self.bitmap.len() == (bits + 7) / 8
        }
    }

    /// Check if a pixel is lit
    pub fn is_pixel_lit(&self, row: usize, col: usize) -> bool {
        let width = usize::from(self.width);
        if col < width {
            let pos = row * width + col;
            let off = pos / 8;
            if off < self.bitmap.len() {
                let bit = 7 - (pos & 0b111);
                return (self.bitmap[off] >> bit) & 1 != 0;
            }
        }
        false
    }

    /// Render the character to a raster
    ///
    /// * `page` Raster to render on.
    /// * `x` Left position of character (0-based).
    /// * `y` Top position of character (0-based).
    /// * `height` Font height in pixels.
    /// * `cf` Foreground color.
    fn render_char(
        &self,
        page: &mut Raster<SRgb8>,
        x: i32,
        y: i32,
        height: i32,
        cf: SRgb8,
    ) {
        let width = i32::from(self.width);
        debug!("render_char: {} @ {x},{y} width: {width}", self.number);
        let mut xx = 0;
        let mut yy = 0;
        for by in &self.bitmap {
            for bi in 0..8 {
                if (by >> (7 - bi)) & 1 != 0 {
                    *page.pixel_mut(x + xx, y + yy) = cf;
                }
                xx += 1;
                if xx >= width {
                    xx = 0;
                    yy += 1;
                    if yy >= height {
                        break;
                    }
                }
            }
        }
    }
}

impl<const C: usize> Default for Font<C> {
    fn default() -> Self {
        // workaround const generic default limitation
        let characters: [CharacterEntry; C] =
            [(); C].map(|_| CharacterEntry::default());
        Font {
            number: 0,
            name: FStr::default(),
            height: 0,
            char_spacing: 0,
            line_spacing: 0,
            characters,
        }
    }
}

impl<const C: usize> Font<C> {
    /// Check if all character numbers are unique
    fn validate_char_numbers(&self) -> Result<()> {
        for i in 1..self.characters.len() {
            let num = self.characters[i - 1].number;
            if num > 0 && self.characters[i..].iter().any(|c| c.number == num) {
                return Err(FontError::DuplicateChar(
                    char::from_u32(num.into()).unwrap(),
                ));
            }
        }
        Ok(())
    }

    /// Check if font is valid
    pub fn validate(&self) -> Result<()> {
        if self.number < 1 {
            Err(FontError::InvalidNumber)
        } else if self.height < 1 {
            Err(FontError::InvalidHeight)
        } else if !self.characters.iter().all(|c| c.is_valid(self.height)) {
            Err(FontError::InvalidCharHeight)
        } else {
            self.validate_char_numbers()?;
            Ok(())
        }
    }

    /// Get version ID (`fontVersionId`)
    pub fn version_id(&self) -> u16 {
        // OER of FontInformation:
        let mut oer = Oer::from(Vec::with_capacity(256));
        oer.u8(self.number);
        oer.u8(self.height);
        oer.u8(self.char_spacing);
        oer.u8(self.line_spacing);
        oer.uint(self.characters.iter().filter(|c| c.number > 0).count() as u32);
        for ch in &self.characters {
            if ch.number > 0 {
                oer.u16(ch.number);
                oer.u8(ch.width);
                oer.octet_string(&ch.bitmap)
            }
        }
        let buf = Vec::from(oer);
        u16::from_be(CRC.checksum(&buf))
    }

    /// Get width (if fixed-width), or 0
    pub fn width(&self) -> u8 {
        let width =
            self.characters.first().map(|c| c.width).unwrap_or_default();
        if self.characters.iter().all(|c| c.width == width) {
            width
        } else {
            0
        }
    }

    /// Get a character
    pub fn character(&self, ch: char) -> Option<&CharacterEntry> {
        if let Ok(n) = u16::try_from(u32::from(ch)) {
            if let Some(c) = self.characters.iter().find(|c| c.number == n) {
                return Some(c);
            }
        }
        None
    }

    /// Calculate the width of a span of text
    ///
    /// * `text` Span of text.
    /// * `cs` Character spacing in pixels.
    pub fn text_width(&self, text: &str, cs: Option<u16>) -> Result<u16> {
        let mut width = 0;
        let cs = cs.unwrap_or_else(|| u16::from(self.char_spacing));
        for ch in text.chars() {
            let c = self.character(ch).ok_or(FontError::InvalidChar(ch))?;
            if width > 0 {
                width += cs;
            }
            width += u16::from(c.width);
        }
        Ok(width)
    }

    /// Render a span of text
    ///
    /// * `page` Raster to render on.
    /// * `text` Span of text.
    /// * `x` Left position of first character (0-based).
    /// * `y` Top position of first character (0-based).
    /// * `cs` Character spacing in pixels.
    /// * `cf` Foreground color.
    pub(crate) fn render_text(
        &self,
        page: &mut Raster<SRgb8>,
        text: &str,
        x: i32,
        y: i32,
        cs: i32,
        cf: SRgb8,
    ) -> Result<()> {
        let height = i32::from(self.height);
        debug!(
            "render_text: font number {}, name {}",
            self.number, self.name
        );
        debug!("render_text: {text} @ {x},{y} height: {height}");
        let mut xx = 0;
        for ch in text.chars() {
            let c = self.character(ch).ok_or(FontError::InvalidChar(ch))?;
            if xx > 0 {
                xx += cs;
            }
            c.render_char(page, x + xx, y, height, cf);
            xx += i32::from(c.width);
        }
        Ok(())
    }
}

impl<const C: usize, const F: usize> Default for FontTable<C, F> {
    fn default() -> Self {
        // workaround const generic default limitation
        let fonts: [Font<C>; F] = [(); F].map(|_| Font::<C>::default());
        FontTable { fonts }
    }
}

impl<const C: usize, const F: usize> FontTable<C, F> {
    /// Validate the font table
    pub fn validate(&self) -> Result<()> {
        for font in &self.fonts {
            if font.number > 0 {
                font.validate()?;
            }
        }
        self.validate_font_numbers()
    }

    /// Check if all font numbers are unique
    fn validate_font_numbers(&self) -> Result<()> {
        for i in 1..self.fonts.len() {
            let num = self.fonts[i - 1].number;
            if num > 0 && self.fonts[i..].iter().any(|f| f.number == num) {
                return Err(FontError::DuplicateNumber(num));
            }
        }
        Ok(())
    }

    /// Lookup a font by number
    pub fn font(&self, fnum: u8) -> Option<&Font<C>> {
        self.fonts.iter().find(|f| f.number == fnum)
    }

    /// Lookup a mutable font by number
    pub fn font_mut(&mut self, fnum: u8) -> Option<&mut Font<C>> {
        self.fonts.iter_mut().find(|f| f.number == fnum)
    }

    /// Lookup a font by name
    pub fn font_by_name<'a>(&'a self, name: &str) -> Option<&'a Font<C>> {
        self.fonts
            .iter()
            .find(|f| f.name.slice_to_terminator('\0') == name)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::font::tfon;

    fn font_table() -> FontTable<128, 2> {
        let mut fonts = FontTable::default();
        let buf = include_str!("../../../test/F02.tfon");
        let f = fonts.font_mut(0).unwrap();
        *f = tfon::parse(&buf[..]).unwrap();
        let buf = include_str!("../../../test/F08.tfon");
        let f = fonts.font_mut(0).unwrap();
        *f = tfon::parse(&buf[..]).unwrap();
        fonts.validate().unwrap();
        fonts
    }

    #[test]
    fn font_version_id() {
        let fonts = font_table();
        let font = fonts.font(2).unwrap();
        assert_eq!(font.version_id(), 0xED52);
        let font = fonts.font(8).unwrap();
        assert_eq!(font.version_id(), 0x28EB);
    }

    #[test]
    fn font_by_name() {
        let fonts = font_table();
        let font = fonts.font_by_name("fontVersionId example").unwrap();
        assert_eq!(font.number, 2);
    }
}
