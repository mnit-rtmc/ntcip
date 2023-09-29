// graphic.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Graphic image support
use crate::dms::multi::{
    Color, ColorClassic, ColorCtx, ColorScheme, SyntaxError,
};
use log::debug;
use pix::{
    el::Pixel,
    rgb::{SRgb8, SRgba8},
    Raster,
};

/// Graphic error
#[derive(Debug, thiserror::Error)]
pub enum GraphicError {
    #[error("Invalid number")]
    InvalidNumber,

    #[error("Duplicate number")]
    DuplicateNumber,

    #[error("Invalid height")]
    InvalidHeight,

    #[error("Invalid width")]
    InvalidWidth,

    #[error("Invalid bitmap")]
    InvalidBitmap,

    #[error("Invalid transparent color")]
    InvalidTransparentColor,
}

/// Graphic image — `dmsGraphicEntry`
#[derive(Clone, Default)]
pub struct Graphic {
    /// Graphic number — `dmsGraphicNumber`
    pub number: u8,
    /// Name of graphic — `dmsGraphicName`
    pub name: String,
    /// Height (pixels) — `dmsGraphicHeight`
    pub height: u8,
    /// Width (pixels) — `dmsGraphicWidth`
    pub width: u16,
    /// Graphic type — `dmsGraphicType`
    pub gtype: ColorScheme,
    /// `dmsGraphicTransparentEnabled` / `dmsGraphicTransparentColor`
    pub transparent_color: Option<Color>,
    /// Bitmap data — `dmsGraphicBitmapTable`
    pub bitmap: Vec<u8>,
}

/// Table of graphics
#[derive(Clone)]
pub struct GraphicTable<const G: usize> {
    /// Graphics in table
    graphics: [Graphic; G],
    /// Version IDs
    version_ids: [Option<u16>; G],
}

impl Graphic {
    // Check if bitmap length is valid
    fn is_bitmap_valid(&self) -> bool {
        let pix = usize::from(self.height) * usize::from(self.width);
        let len = match self.gtype {
            ColorScheme::Monochrome1Bit => (pix + 7) / 8,
            ColorScheme::Color24Bit => pix * 3,
            _ => pix,
        };
        len == self.bitmap.len()
    }

    // Check if transparent color type is valid
    fn is_transparent_color_valid(&self) -> bool {
        matches!(
            (self.gtype, self.transparent_color),
            (_, None)
                | (ColorScheme::Monochrome1Bit, Some(Color::Legacy(_)))
                | (ColorScheme::Monochrome8Bit, Some(Color::Legacy(_)))
                | (ColorScheme::ColorClassic, Some(Color::Legacy(_)))
                | (ColorScheme::Color24Bit, Some(Color::Rgb(_, _, _)))
        )
    }

    /// Check if graphic is valid
    pub fn validate(&self) -> Result<(), GraphicError> {
        if self.number < 1 {
            Err(GraphicError::InvalidNumber)
        } else if self.height < 1 {
            Err(GraphicError::InvalidHeight)
        } else if self.width < 1 {
            Err(GraphicError::InvalidWidth)
        } else if !self.is_bitmap_valid() {
            Err(GraphicError::InvalidBitmap)
        } else if !self.is_transparent_color_valid() {
            Err(GraphicError::InvalidTransparentColor)
        } else {
            Ok(())
        }
    }

    /// Convert graphic to a raster
    pub fn to_raster(&self) -> Raster<SRgba8> {
        let fg = match self.gtype {
            ColorScheme::Monochrome1Bit | ColorScheme::Monochrome8Bit => {
                ColorClassic::White.rgb()
            }
            _ => ColorClassic::Amber.rgb(),
        };
        let ctx = ColorCtx::new(
            ColorScheme::Color24Bit,
            fg,
            ColorClassic::Black.rgb(),
        );
        let width = self.width.into();
        let height = self.height.into();
        let mut raster =
            Raster::with_clear(self.width.into(), self.height.into());
        for y in 0..height {
            for x in 0..width {
                if let Some(clr) = self.pixel_fn(x, y, &ctx) {
                    *raster.pixel_mut(x, y) = clr.convert();
                }
            }
        }
        raster
    }

    /// Render graphic onto a Raster
    pub(crate) fn render_graphic(
        &self,
        page: &mut Raster<SRgb8>,
        x: i32,
        y: i32,
        ctx: &ColorCtx,
    ) -> Result<(), SyntaxError> {
        debug_assert!(x > 0);
        debug_assert!(y > 0);
        let x = x - 1;
        let y = y - 1;
        let w = i32::from(self.width);
        let h = i32::from(self.height);
        let width = i32::try_from(page.width()).unwrap();
        let height = i32::try_from(page.height()).unwrap();
        if x + w > width || y + h > height {
            // There is no GraphicTooBig syntax error
            return Err(SyntaxError::Other("Graphic too big"));
        }
        for yy in 0..h {
            for xx in 0..w {
                if let Some(clr) = self.pixel_fn(xx, yy, ctx) {
                    *page.pixel_mut(x + xx, y + yy) = clr;
                }
            }
        }
        Ok(())
    }

    /// Get one pixel of a graphic
    fn pixel_fn(&self, x: i32, y: i32, ctx: &ColorCtx) -> Option<SRgb8> {
        match self.gtype {
            ColorScheme::Monochrome1Bit => self.pixel_1(x, y, ctx),
            ColorScheme::Monochrome8Bit | ColorScheme::ColorClassic => {
                self.pixel_8(x, y, ctx)
            }
            ColorScheme::Color24Bit => self.pixel_24(x, y),
        }
    }

    /// Get one pixel of a monochrome 1-bit graphic
    fn pixel_1(&self, x: i32, y: i32, ctx: &ColorCtx) -> Option<SRgb8> {
        let offset = y * i32::from(self.width) + x;
        let by = offset as usize / 8;
        let bi = 7 - (offset & 7);
        let lit = ((self.bitmap[by] >> bi) & 1) != 0;
        match (lit, self.transparent_color) {
            (false, Some(Color::Legacy(0))) => None,
            (true, Some(Color::Legacy(1))) => None,
            (false, _) => {
                let (red, green, blue) = ctx.rgb(ctx.background())?;
                Some(SRgb8::new(red, green, blue))
            }
            (true, _) => {
                let (red, green, blue) = ctx.rgb(ctx.foreground())?;
                Some(SRgb8::new(red, green, blue))
            }
        }
    }

    /// Get one pixel of an 8-bit (monochrome or classic) color graphic
    fn pixel_8(&self, x: i32, y: i32, ctx: &ColorCtx) -> Option<SRgb8> {
        let offset = y * i32::from(self.width) + x;
        let v = self.bitmap[offset as usize];
        if let Some(Color::Legacy(c)) = self.transparent_color {
            if v == c {
                return None;
            }
        }
        match ctx.rgb(Color::Legacy(v)) {
            Some((red, green, blue)) => Some(SRgb8::new(red, green, blue)),
            None => {
                debug!("pixel_8 -- Bad color {}", v);
                None
            }
        }
    }

    /// Get one pixel of a 24-bit color graphic
    fn pixel_24(&self, x: i32, y: i32) -> Option<SRgb8> {
        let offset = 3 * (y * i32::from(self.width) + x) as usize;
        // BGR order for dmsGraphicBitmapTable with 24-bit color
        let blue = self.bitmap[offset];
        let green = self.bitmap[offset + 1];
        let red = self.bitmap[offset + 2];
        if let Some(Color::Rgb(r, g, b)) = self.transparent_color {
            if red == r && green == g && blue == b {
                return None;
            }
        }
        Some(SRgb8::new(red, green, blue))
    }
}

impl<const G: usize> Default for GraphicTable<G> {
    fn default() -> Self {
        // workaround const generic default limitation
        let graphics: [Graphic; G] = [(); G].map(|_| Graphic::default());
        let version_ids: [Option<u16>; G] = [(); G].map(|_| None);
        GraphicTable {
            graphics,
            version_ids,
        }
    }
}

impl<const G: usize> GraphicTable<G> {
    /// Validate the graphic table
    pub fn validate(&mut self) -> Result<(), GraphicError> {
        for (i, graphic) in self.graphics.iter().enumerate() {
            if graphic.number > 0 {
                if self.version_ids[i].is_none() {
                    // FIXME: calculate version ID
                }
                graphic.validate()?;
            }
        }
        self.validate_graphic_numbers()
    }

    /// Check if all graphic numbers are unique
    fn validate_graphic_numbers(&self) -> Result<(), GraphicError> {
        for i in 1..self.graphics.len() {
            let num = self.graphics[i - 1].number;
            if num > 0 && self.graphics[i..].iter().any(|g| g.number == num) {
                return Err(GraphicError::DuplicateNumber);
            }
        }
        Ok(())
    }

    /// Get a graphic
    pub fn get(&self, index: usize) -> Option<&Graphic> {
        self.graphics.get(index)
    }

    /// Get a mutable graphic
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Graphic> {
        if let Some(vid) = self.version_ids.get_mut(index) {
            *vid = None;
        }
        self.graphics.get_mut(index)
    }

    /// Get a graphic version ID
    pub fn version_id(&self, gnum: u8) -> Option<u16> {
        self.graphics
            .iter()
            .zip(self.version_ids)
            .find(|(g, _v)| g.number == gnum)
            .and_then(|(_g, v)| v)
    }

    /// Lookup a graphic by number
    pub fn lookup(&self, gnum: u8) -> Option<&Graphic> {
        self.graphics.iter().find(|g| g.number == gnum)
    }
}
