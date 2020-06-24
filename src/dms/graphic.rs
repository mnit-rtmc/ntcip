// graphic.rs
//
// Copyright (C) 2018-2020  Minnesota Department of Transportation
//
//! Graphics are used on dynamic message signs.
use crate::dms::multi::{Color, ColorCtx, ColorScheme, SyntaxError};
use crate::dms::Result;
use log::debug;
use pix::{rgb::SRgb8, Raster};
use std::collections::HashMap;
use std::convert::TryFrom;

/// An uncompressed DMS graphic.
#[derive(Deserialize, Serialize)]
pub struct Graphic {
    /// Graphic number
    number: u8,
    /// Name (max 64 characters)
    name: String,
    /// Height in pixels
    height: u8,
    /// Width in pixels
    width: u16,
    /// Color scheme, or dmsGraphicType from NTCIP 1203
    color_scheme: String,
    /// Transparent color (RGB)
    transparent_color: Option<i32>,
    /// Bitmap data (by rows)
    #[serde(with = "super::base64")]
    bitmap: Vec<u8>,
}

/// A cache of graphics.
#[derive(Default)]
pub struct GraphicCache {
    /// Graphics in cache
    graphics: HashMap<u8, Graphic>,
}

/// Function to lookup a pixel from a graphic buffer
type PixFn = dyn Fn(&Graphic, i32, i32, &ColorCtx, &[u8]) -> Option<SRgb8>;

impl Graphic {
    /// Get the number
    pub fn number(&self) -> u8 {
        self.number
    }

    /// Get the name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the height in pixels
    pub fn height(&self) -> u32 {
        self.height.into()
    }

    /// Get the width in pixels
    pub fn width(&self) -> u32 {
        self.width.into()
    }

    /// Get the color scheme
    pub fn color_scheme(&self) -> &str {
        &self.color_scheme
    }

    /// Get the transparent color
    pub fn transparent_color(&self) -> Option<i32> {
        self.transparent_color
    }

    /// Render graphic onto a Raster
    pub fn render_graphic(
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
        let pix_fn = self.pixel_fn();
        for yy in 0..h {
            for xx in 0..w {
                if let Some(clr) = pix_fn(self, xx, yy, ctx, &self.bitmap) {
                    *page.pixel_mut(x + xx, y + yy) = clr;
                }
            }
        }
        Ok(())
    }

    /// Get pixel lookup function for the color scheme
    fn pixel_fn(&self) -> &PixFn {
        match self.color_scheme[..].into() {
            ColorScheme::Monochrome1Bit => &Graphic::pixel_1,
            ColorScheme::Monochrome8Bit | ColorScheme::ColorClassic => {
                &Graphic::pixel_8
            }
            ColorScheme::Color24Bit => &Graphic::pixel_24,
        }
    }

    /// Get one pixel of a monochrome 1-bit graphic
    fn pixel_1(
        &self,
        x: i32,
        y: i32,
        ctx: &ColorCtx,
        buf: &[u8],
    ) -> Option<SRgb8> {
        let offset = y * i32::from(self.width) + x;
        let by = offset as usize / 8;
        let bi = 7 - (offset & 7);
        let lit = ((buf[by] >> bi) & 1) != 0;
        match (lit, self.transparent_color) {
            (false, Some(0)) => None,
            (true, Some(1)) => None,
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
    fn pixel_8(
        &self,
        x: i32,
        y: i32,
        ctx: &ColorCtx,
        buf: &[u8],
    ) -> Option<SRgb8> {
        let offset = y * i32::from(self.width) + x;
        let v: u8 = buf[offset as usize];
        if self.transparent_color == Some(v.into()) {
            return None;
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
    fn pixel_24(
        &self,
        x: i32,
        y: i32,
        _ctx: &ColorCtx,
        buf: &[u8],
    ) -> Option<SRgb8> {
        let offset = 3 * (y * i32::from(self.width) + x) as usize;
        let red = buf[offset];
        let green = buf[offset + 1];
        let blue = buf[offset + 2];
        if let Some(tc) = self.transparent_color {
            let rgb =
                ((red as i32) << 16) + ((green as i32) << 8) + blue as i32;
            if rgb == tc {
                return None;
            }
        }
        Some(SRgb8::new(red, green, blue))
    }
}

impl GraphicCache {
    /// Insert a graphiu into the cache
    pub fn insert(&mut self, graphic: Graphic) {
        self.graphics.insert(graphic.number(), graphic);
    }

    /// Lookup a graphic by number
    pub fn lookup<'a>(
        &'a self,
        gnum: u8,
        version_id: Option<u16>,
    ) -> Result<&'a Graphic> {
        match (self.graphics.get(&gnum), version_id) {
            // FIXME: calculate and check version_id
            (Some(g), Some(_vid)) => Ok(g),
            (Some(g), None) => Ok(g),
            (None, _) => Err(SyntaxError::GraphicNotDefined(gnum)),
        }
    }
}
