// graphic.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Graphic image support
use crate::dms::multi::{
    Color, ColorClassic, ColorCtx, ColorScheme, SyntaxError,
};
use crate::dms::Result;
use log::debug;
use pix::{
    el::Pixel,
    rgb::{SRgb8, SRgba8},
    Raster,
};

/// Graphic image — `dmsGraphicEntry`
#[derive(Clone)]
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
    /// Transparent color
    ///
    /// `dmsGraphicTransparentEnabled` / `dmsGraphicTransparentColor`
    pub transparent_color: Option<Color>,
    /// Bitmap data — `dmsGraphicBitmapTable` (uncompressed BGR)
    pub bitmap: Vec<u8>,
}

/// A table of graphics
#[derive(Clone, Default)]
pub struct GraphicTable {
    /// Graphics in table
    graphics: Vec<Graphic>,
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
    pub fn is_valid(&self) -> bool {
        self.number > 0
            && self.height > 0
            && self.width > 0
            && self.is_bitmap_valid()
            && self.is_transparent_color_valid()
    }

    /// Convert graphic to a raster
    pub fn to_raster(&self) -> Raster<SRgba8> {
        let ctx = ColorCtx::new(
            ColorScheme::Color24Bit,
            ColorClassic::Amber.rgb(),
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
    ) -> Result<()> {
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

impl GraphicTable {
    /// Push a graphic into the table
    pub fn push(&mut self, graphic: Graphic) -> Result<()> {
        if !graphic.is_valid() {
            return Err(SyntaxError::Other("Invalid graphic"));
        }
        if self.graphics.iter().any(|g| g.number == graphic.number) {
            return Err(SyntaxError::Other("Duplicate graphic number"));
        }
        self.graphics.push(graphic);
        Ok(())
    }

    /// Sort by graphic number
    pub fn sort(&mut self) {
        self.graphics.sort_by(|a, b| a.number.cmp(&b.number))
    }

    /// Lookup a graphic by number
    pub fn lookup(
        &self,
        gnum: u8,
        version_id: Option<u16>,
    ) -> Result<&Graphic> {
        let graphic = self.graphics.iter().find(|g| g.number == gnum);
        match (graphic, version_id) {
            (Some(g), None) => Ok(g),
            // FIXME: calculate and check version_id
            (Some(g), Some(_vid)) => Ok(g),
            (None, _) => Err(SyntaxError::GraphicNotDefined(gnum)),
        }
    }
}
