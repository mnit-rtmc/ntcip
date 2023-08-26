// font.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Font support for dynamic message signs
use crate::dms::multi::SyntaxError;
use crate::dms::Result;
use log::debug;
use pix::{rgb::SRgb8, Raster};

/// Read/write .ifnt format
pub mod ifnt;

/// Character for a bitmap [font]
///
/// [font]: struct.Font.html
#[derive(Clone, Default)]
pub struct CharacterEntry {
    /// Character number (code point)
    pub number: u16,
    /// Width in pixels
    pub width: u8,
    /// Bitmap data (by rows)
    pub bitmap: Vec<u8>,
}

/// Bitmap font
#[derive(Clone, Default)]
pub struct Font {
    /// Font number
    pub number: u8,
    /// Name (max 64 characters)
    pub name: String,
    /// Height in pixels
    pub height: u8,
    /// Default pixel spacing between characters
    pub char_spacing: u8,
    /// Default pixel spacing between lines
    pub line_spacing: u8,
    /// Characters in font
    pub characters: Vec<CharacterEntry>,
    /// Version ID hash
    pub version_id: u16,
}

/// Table of fonts
///
/// This represents the `fontDefinition` of a Dms.
#[derive(Clone, Default)]
pub struct FontTable {
    /// Fonts in table
    fonts: Vec<Font>,
}

impl CharacterEntry {
    /// Check if character is valid
    fn is_valid(&self, height: u8) -> bool {
        self.number > 0 && {
            let bits = usize::from(self.width) * usize::from(height);
            self.bitmap.len() == (bits + 7) / 8
        }
    }

    /// Check if a pixel is lit
    pub fn is_pixel_lit(&self, row: usize, col: usize) -> bool {
        let pos = row * usize::from(self.width) + col;
        let off = pos / 8;
        let bit = 7 - (pos & 0b111);
        (self.bitmap[off] >> bit) & 1 != 0
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
                if by >> (7 - bi) & 1 != 0 {
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

impl Font {
    /// Check if all character numbers are unique
    fn are_char_numbers_unique(&self) -> bool {
        for i in 1..self.characters.len() {
            let num = self.characters[i - 1].number;
            if self.characters[i..].iter().any(|c| c.number == num) {
                return false;
            }
        }
        true
    }

    /// Check if font is valid
    pub fn is_valid(&self) -> bool {
        self.number > 0
            && self.height > 0
            && self.characters.iter().all(|c| c.is_valid(self.height))
            && self.are_char_numbers_unique()
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
    pub fn character(&self, ch: char) -> Result<&CharacterEntry> {
        if let Ok(n) = u16::try_from(u32::from(ch)) {
            if let Some(c) = self.characters.iter().find(|c| c.number == n) {
                return Ok(c);
            }
        }
        Err(SyntaxError::CharacterNotDefined(ch))
    }

    /// Calculate the width of a span of text
    ///
    /// * `text` Span of text.
    /// * `cs` Character spacing in pixels.
    pub(crate) fn text_width(
        &self,
        text: &str,
        cs: Option<u16>,
    ) -> Result<u16> {
        let mut width = 0;
        let cs = cs.unwrap_or_else(|| u16::from(self.char_spacing));
        for ch in text.chars() {
            let c = self.character(ch)?;
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
            let c = self.character(ch)?;
            if xx > 0 {
                xx += cs;
            }
            c.render_char(page, x + xx, y, height, cf);
            xx += i32::from(c.width);
        }
        Ok(())
    }

    /// Get version ID hash
    pub fn version_id(&self) -> u16 {
        self.version_id
    }
}

impl FontTable {
    /// Push a font into the table
    pub fn push(&mut self, font: Font) -> Result<()> {
        if !font.is_valid() {
            return Err(SyntaxError::Other("Invalid font"));
        }
        if self.fonts.iter().any(|f| f.number == font.number) {
            return Err(SyntaxError::Other("Duplicate font number"));
        }
        // FIXME: calculate version ID
        self.fonts.push(font);
        Ok(())
    }

    /// Sort by font number
    pub fn sort(&mut self) {
        self.fonts.sort_by(|a, b| a.number.cmp(&b.number))
    }

    /// Lookup a font by number
    pub fn lookup(&self, fnum: u8, version_id: Option<u16>) -> Result<&Font> {
        let font = self.fonts.iter().find(|f| f.number == fnum);
        match (font, version_id) {
            (Some(f), Some(vid)) => {
                if vid == f.version_id {
                    Ok(f)
                } else {
                    Err(SyntaxError::FontVersionID)
                }
            }
            (Some(f), None) => Ok(f),
            (None, _) => Err(SyntaxError::FontNotDefined(fnum)),
        }
    }

    /// Lookup a font by name
    pub fn lookup_name<'a>(&'a self, name: &str) -> Option<&'a Font> {
        self.fonts.iter().find(|f| f.name == name)
    }
}
