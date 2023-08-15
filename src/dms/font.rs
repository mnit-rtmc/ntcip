// font.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Bitmap fonts are used on dynamic message signs.
use crate::dms::multi::SyntaxError;
use crate::dms::Result;
use log::debug;
use pix::{rgb::SRgb8, Raster};

/// A character for a bitmap [font]
///
/// [font]: struct.Font.html
#[derive(Clone)]
pub struct CharacterEntry {
    /// Character number (code point)
    pub number: u16,
    /// Width in pixels
    pub width: u8,
    /// Bitmap data (by rows)
    pub bitmap: Vec<u8>,
}

/// A bitmap font
#[derive(Clone)]
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

/// A table of fonts
#[derive(Clone, Default)]
pub struct FontTable {
    /// Fonts in table
    fonts: Vec<Font>,
}

impl CharacterEntry {
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
        debug!(
            "render_char: {} @ {},{} width: {}",
            self.number, x, y, width
        );
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
    /// Get a character
    pub fn character(&self, ch: char) -> Result<&CharacterEntry> {
        let code_point = u32::from(ch);
        if code_point <= u32::from(std::u16::MAX) {
            let n = code_point as u16;
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
        debug!("render_text: {} @ {},{} height: {}", text, x, y, height);
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
        // FIXME: check font is valid
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
                // FIXME: calculate version_id
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
