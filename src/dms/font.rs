// font.rs
//
// Copyright (C) 2018-2020  Minnesota Department of Transportation
//
//! Bitmap fonts are used on dynamic message signs.
use crate::dms::multi::SyntaxError;
use crate::dms::Result;
use log::debug;
use pix::{rgb::SRgb8, Raster};
use std::collections::HashMap;

/// A character for a bitmap [font].
///
/// [font]: struct.Font.html
#[derive(Deserialize, Serialize)]
pub struct Character {
    /// Character number (code point)
    number: u16,
    /// Width in pixels
    width: u8,
    /// Bitmap data (by rows)
    #[serde(with = "super::base64")]
    bitmap: Vec<u8>,
}

/// A bitmap font.
///
/// Text can be rendered onto a raster using [render_text](#method.render_text).
#[derive(Deserialize, Serialize)]
pub struct Font {
    /// Font number
    number: u8,
    /// Name (max 64 characters)
    name: String,
    /// Height in pixels
    height: u8,
    /// Default pixel spacing between characters
    char_spacing: u8,
    /// Default pixel spacing between lines
    line_spacing: u8,
    /// Characters in font
    characters: Vec<Character>,
    /// Version ID hash
    version_id: u16,
}

/// A cache of fonts.
///
/// Fonts can be deserialized from a JSON file using [serde].
///
/// ## Example
///
/// ```rust
/// use ntcip::dms::{Font, FontCache};
///
/// let mut fonts = FontCache::default();
/// let fts: Vec<Font> =
///     serde_json::from_str(include_str!("../../test/font.json")).unwrap();
/// for font in fts {
///     fonts.insert(font);
/// }
/// ```
/// [serde]: https://serde.rs/
#[derive(Default)]
pub struct FontCache {
    /// Fonts in cache
    fonts: HashMap<u8, Font>,
}

impl Character {
    /// Get number (code point)
    pub fn number(&self) -> u16 {
        self.number
    }

    /// Get width in pixels
    pub fn width(&self) -> u8 {
        self.width
    }

    /// Render the character to a raster.
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

impl<'a> Font {
    /// Get font number
    pub fn number(&self) -> u8 {
        self.number
    }

    /// Get font name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get font height
    pub fn height(&self) -> u8 {
        self.height
    }

    /// Get default pixel spacing between characters
    pub fn char_spacing(&self) -> u8 {
        self.char_spacing
    }

    /// Get default pixel spacing between lines
    pub fn line_spacing(&self) -> u8 {
        self.line_spacing
    }

    /// Get a character
    pub fn character(&'a self, ch: char) -> Result<&'a Character> {
        let code_point = u32::from(ch);
        if code_point <= u32::from(std::u16::MAX) {
            let n = code_point as u16;
            if let Some(c) = self.characters.iter().find(|c| c.number == n) {
                return Ok(c);
            }
        }
        Err(SyntaxError::CharacterNotDefined(ch))
    }

    /// Calculate the width of a span of text.
    ///
    /// * `text` Span of text.
    /// * `cs` Character spacing in pixels.
    pub fn text_width(&self, text: &str, cs: Option<u16>) -> Result<u16> {
        let mut width = 0;
        let cs = cs.unwrap_or_else(|| u16::from(self.char_spacing));
        for ch in text.chars() {
            let c = self.character(ch)?;
            if width > 0 {
                width += cs;
            }
            width += u16::from(c.width());
        }
        Ok(width)
    }

    /// Render a span of text.
    ///
    /// * `page` Raster to render on.
    /// * `text` Span of text.
    /// * `x` Left position of first character (0-based).
    /// * `y` Top position of first character (0-based).
    /// * `cs` Character spacing in pixels.
    /// * `cf` Foreground color.
    pub fn render_text(
        &self,
        page: &mut Raster<SRgb8>,
        text: &str,
        x: i32,
        y: i32,
        cs: i32,
        cf: SRgb8,
    ) -> Result<()> {
        let height = i32::from(self.height());
        debug!(
            "render_text: font number {}, name {}",
            self.number(),
            self.name()
        );
        debug!("render_text: {} @ {},{} height: {}", text, x, y, height);
        let mut xx = 0;
        for ch in text.chars() {
            let c = self.character(ch)?;
            if xx > 0 {
                xx += cs;
            }
            c.render_char(page, x + xx, y, height, cf);
            xx += i32::from(c.width());
        }
        Ok(())
    }

    /// Get version ID hash
    pub fn version_id(&self) -> u16 {
        self.version_id
    }
}

impl FontCache {
    /// Insert a font into the cache
    pub fn insert(&mut self, font: Font) {
        self.fonts.insert(font.number(), font);
    }

    /// Lookup a font by number
    pub fn lookup<'a>(
        &'a self,
        fnum: u8,
        version_id: Option<u16>,
    ) -> Result<&'a Font> {
        match (self.fonts.get(&fnum), version_id) {
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
        self.fonts.values().find(|f| f.name() == name)
    }
}
