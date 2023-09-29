// font.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Font support for dynamic message signs
use crate::dms::multi::{Result, SyntaxError};
use log::debug;
use pix::{rgb::SRgb8, Raster};

/// Read/write fonts in `.ifnt` format
pub mod ifnt;

/// Font error
#[derive(Debug, thiserror::Error)]
pub enum FontError {
    #[error("Invalid number")]
    InvalidNumber,

    #[error("Duplicate number")]
    DuplicateNumber,

    #[error("Duplicate character number")]
    DuplicateCharNumber,

    #[error("Invalid height")]
    InvalidHeight,

    #[error("Invalid character height")]
    InvalidCharHeight,
}

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
}

/// Table of fonts
///
/// This represents the `fontDefinition` of a Dms.
#[derive(Clone)]
pub struct FontTable<const F: usize> {
    /// Fonts in table
    fonts: [Font; F],
    /// Version IDs
    version_ids: [Option<u16>; F],
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
            if num > 0 && self.characters[i..].iter().any(|c| c.number == num) {
                return false;
            }
        }
        true
    }

    /// Check if font is valid
    pub fn validate(&self) -> std::result::Result<(), FontError> {
        if self.number < 1 {
            Err(FontError::InvalidNumber)
        } else if self.height < 1 {
            Err(FontError::InvalidHeight)
        } else if !self.characters.iter().all(|c| c.is_valid(self.height)) {
            Err(FontError::InvalidCharHeight)
        } else if !self.are_char_numbers_unique() {
            Err(FontError::DuplicateCharNumber)
        } else {
            Ok(())
        }
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
}

impl<const F: usize> Default for FontTable<F> {
    fn default() -> Self {
        // workaround const generic default limitation
        let fonts: [Font; F] = [(); F].map(|_| Font::default());
        let version_ids: [Option<u16>; F] = [(); F].map(|_| None);
        FontTable { fonts, version_ids }
    }
}

impl<const F: usize> FontTable<F> {
    /// Validate the font table
    pub fn validate(&mut self) -> std::result::Result<(), FontError> {
        for (i, font) in self.fonts.iter().enumerate() {
            if font.number > 0 {
                if self.version_ids[i].is_none() {
                    // FIXME: calculate version ID
                }
                font.validate()?;
            }
        }
        self.validate_font_numbers()
    }

    /// Check if all font numbers are unique
    fn validate_font_numbers(&self) -> std::result::Result<(), FontError> {
        for i in 1..self.fonts.len() {
            let num = self.fonts[i - 1].number;
            if num > 0 && self.fonts[i..].iter().any(|f| f.number == num) {
                return Err(FontError::DuplicateNumber);
            }
        }
        Ok(())
    }

    /// Lookup a font by number
    pub fn lookup(&self, fnum: u8) -> Option<&Font> {
        self.fonts.iter().find(|f| f.number == fnum)
    }

    /// Lookup a mutable font by number
    pub fn lookup_mut(&mut self, fnum: u8) -> Option<&mut Font> {
        self.fonts
            .iter_mut()
            .enumerate()
            .find(|(_i, f)| f.number == fnum)
            .map(|(i, f)| {
                self.version_ids[i] = None;
                f
            })
    }

    /// Lookup a font by name
    pub fn lookup_name<'a>(&'a self, name: &str) -> Option<&'a Font> {
        self.fonts.iter().find(|f| f.name == name)
    }

    /// Get a font version ID
    pub fn version_id(&self, fnum: u8) -> Option<u16> {
        self.fonts
            .iter()
            .zip(self.version_ids)
            .find(|(f, _v)| f.number == fnum)
            .and_then(|(_f, v)| v)
    }
}
