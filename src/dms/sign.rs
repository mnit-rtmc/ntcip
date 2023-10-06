// sign.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
use crate::dms::config::{CfgError, MultiCfg, SignCfg, VmsCfg};
use crate::dms::font::{FontError, FontTable};
use crate::dms::graphic::{GraphicError, GraphicTable};
use crate::dms::multi::{ColorCtx, ColorScheme, Rectangle};

/// Sign error
#[derive(Debug, thiserror::Error)]
pub enum SignError {
    #[error("Config error: {0}")]
    CfgValidation(#[from] CfgError),

    #[error("Font error: {0}")]
    FontValidation(#[from] FontError),

    #[error("Graphic error: {0}")]
    GraphicValidation(#[from] GraphicError),
}

/// Builder for DMS
#[derive(Clone, Default)]
pub struct DmsBuilder<const F: usize, const G: usize> {
    sign_cfg: SignCfg,
    vms_cfg: VmsCfg,
    font_definition: FontTable<F>,
    multi_cfg: MultiCfg,
    graphic_definition: GraphicTable<G>,
}

/// Dynamic message sign
///
/// This is the root node of the 1203 Message Information Base (MIB)
#[derive(Clone)]
pub struct Dms<const F: usize, const G: usize> {
    /// Configuration common to all signs — `dmsSignCfg`
    pub(crate) sign_cfg: SignCfg,
    /// Configuration for variable message signs — `vmsCfg`
    pub(crate) vms_cfg: VmsCfg,
    /// Font definition — `fontDefinition`
    pub(crate) font_definition: FontTable<F>,
    /// MULTI configuration — `multiCfg`
    pub(crate) multi_cfg: MultiCfg,
    /// Graphic definition — `graphicDefinition`
    pub(crate) graphic_definition: GraphicTable<G>,
}

impl<const F: usize, const G: usize> DmsBuilder<F, G> {
    /// Set sign configuration
    pub fn with_sign_cfg(mut self, cfg: SignCfg) -> Self {
        self.sign_cfg = cfg;
        self
    }

    /// Set VMS configuration
    pub fn with_vms_cfg(mut self, cfg: VmsCfg) -> Self {
        self.vms_cfg = cfg;
        self
    }

    /// Set font definition
    pub fn with_font_definition(mut self, fonts: FontTable<{ F }>) -> Self {
        self.font_definition = fonts;
        self
    }

    /// Set MULTI configuration
    pub fn with_multi_cfg(mut self, cfg: MultiCfg) -> Self {
        self.multi_cfg = cfg;
        self
    }

    /// Set graphic definition
    pub fn with_graphic_definition(
        mut self,
        graphics: GraphicTable<{ G }>,
    ) -> Self {
        self.graphic_definition = graphics;
        self
    }

    /// Build the DMS with validation
    pub fn build(self) -> Result<Dms<{ F }, { G }>, SignError> {
        self.multi_cfg.validate()?;
        self.font_definition.validate()?;
        self.graphic_definition.validate()?;
        Ok(Dms {
            sign_cfg: self.sign_cfg,
            vms_cfg: self.vms_cfg,
            font_definition: self.font_definition,
            multi_cfg: self.multi_cfg,
            graphic_definition: self.graphic_definition,
        })
    }
}

impl<const F: usize, const G: usize> Dms<F, G> {
    /// Create a DMS builder
    pub fn builder() -> DmsBuilder<F, G> {
        DmsBuilder::default()
    }

    /// Convert back into builder
    pub fn into_builder(self) -> DmsBuilder<F, G> {
        DmsBuilder {
            sign_cfg: self.sign_cfg,
            vms_cfg: self.vms_cfg,
            font_definition: self.font_definition,
            multi_cfg: self.multi_cfg,
            graphic_definition: self.graphic_definition,
        }
    }

    /// Get font definition
    pub fn font_definition(&self) -> &FontTable<{ F }> {
        &self.font_definition
    }

    /// Get graphic definition
    pub fn graphic_definition(&self) -> &GraphicTable<{ G }> {
        &self.graphic_definition
    }

    /// Get the face width (mm)
    pub fn face_width_mm(&self) -> f32 {
        self.sign_cfg.sign_width as f32
    }

    /// Get the width of the sign (pixels)
    fn pixel_width(&self) -> u16 {
        self.vms_cfg.sign_width_pixels
    }

    /// Get the horizontal border (mm)
    ///
    /// Sanity checked in case values are stupid.
    fn border_horiz_mm(&self) -> f32 {
        let pix = self.pixel_width() + self.gap_char_count();
        let min_width = (pix as f32) * self.pitch_horiz_mm();
        let extra = (self.face_width_mm() - min_width).max(0.0);
        if self.gap_char_count() > 0 {
            let border = self.sign_cfg.horizontal_border as f32;
            border.min(extra / 2.0)
        } else {
            // Ignore border_horiz if there are no character gaps
            extra / 2.0
        }
    }

    /// Get the pixel width (mm)
    fn pixel_width_mm(&self) -> f32 {
        self.face_width_mm() - self.border_horiz_mm() * 2.0
    }

    /// Get the horizontal pitch
    fn pitch_horiz(&self) -> u8 {
        self.vms_cfg.horizontal_pitch
    }

    /// Get the horizontal pitch (mm)
    ///
    /// Sanity checked in case values are stupid.
    fn pitch_horiz_mm(&self) -> f32 {
        let pitch = self.pitch_horiz() as f32;
        let pix = self.pixel_width() + self.gap_char_count();
        if pix > 0 {
            pitch.min(self.face_width_mm() / pix as f32)
        } else {
            pitch
        }
    }

    /// Get the number of gaps between characters
    fn gap_char_count(&self) -> u16 {
        let cw = self.char_width().into();
        if cw > 1 && self.pixel_width() > cw {
            (self.pixel_width() - 1) / cw
        } else {
            0
        }
    }

    /// Get the X-position of a pixel on the sign
    pub fn pixel_x(&self, x: u32, width: u32) -> f32 {
        let border = self.border_horiz_mm();
        let offset = self.char_offset_mm(x);
        let x = x as f32 + 0.5; // shift to center of pixel
        let pos = border + offset + x * self.pitch_horiz_mm();
        width as f32 * pos / self.face_width_mm()
    }

    /// Get the horizontal character offset (mm)
    fn char_offset_mm(&self, x: u32) -> f32 {
        let cw = u32::from(self.char_width());
        if cw > 1 {
            let char_num = (x / cw) as f32;
            char_num * self.gap_char_mm()
        } else {
            0.0
        }
    }

    /// Get the character gap (mm)
    fn gap_char_mm(&self) -> f32 {
        let gaps = self.gap_char_count();
        if gaps > 0 {
            self.excess_char_mm() / gaps as f32
        } else {
            0.0
        }
    }

    /// Get excess width for character gaps (mm)
    fn excess_char_mm(&self) -> f32 {
        let pix_mm = self.pitch_horiz_mm() * self.pixel_width() as f32;
        (self.pixel_width_mm() - pix_mm).max(0.0)
    }

    /// Get the face height (mm)
    pub fn face_height_mm(&self) -> f32 {
        self.sign_cfg.sign_height as f32
    }

    /// Get the height of the sign (pixels)
    fn pixel_height(&self) -> u16 {
        self.vms_cfg.sign_height_pixels
    }

    /// Get the vertical border (mm)
    ///
    /// Sanity checked in case values are stupid.
    fn border_vert_mm(&self) -> f32 {
        let pix = self.pixel_height() + self.gap_line_count();
        let min_height = (pix as f32) * self.pitch_vert_mm();
        let extra = (self.face_height_mm() - min_height).max(0.0);
        if self.gap_line_count() > 0 {
            let border = self.sign_cfg.vertical_border as f32;
            border.min(extra / 2.0)
        } else {
            // Ignore border_vert if there are no line gaps
            extra / 2.0
        }
    }

    /// Get the pixel height (mm)
    fn pixel_height_mm(&self) -> f32 {
        self.face_height_mm() - self.border_vert_mm() * 2.0
    }

    /// Get the vertical pitch
    fn pitch_vert(&self) -> u8 {
        self.vms_cfg.vertical_pitch
    }

    /// Get the vertical pitch (mm)
    ///
    /// Sanity checked in case values are stupid.
    fn pitch_vert_mm(&self) -> f32 {
        let pitch = self.pitch_vert() as f32;
        let pix = self.pixel_height() + self.gap_line_count();
        if pix > 0 {
            pitch.min(self.face_height_mm() / pix as f32)
        } else {
            pitch
        }
    }

    /// Get the number of gaps between lines
    fn gap_line_count(&self) -> u16 {
        let ch = self.char_height().into();
        if ch > 1 && self.pixel_height() > ch {
            (self.pixel_height() - 1) / ch
        } else {
            0
        }
    }

    /// Get the Y-position of a pixel on the sign
    pub fn pixel_y(&self, y: u32, height: u32) -> f32 {
        let border = self.border_vert_mm();
        let offset = self.line_offset_mm(y);
        let y = y as f32 + 0.5; // shift to center of pixel
        let pos = border + offset + y * self.pitch_vert_mm();
        height as f32 * pos / self.face_height_mm()
    }

    /// Get the vertical line offset (mm)
    fn line_offset_mm(&self, y: u32) -> f32 {
        let ch = u32::from(self.char_height());
        if ch > 1 {
            let line_num = (y / ch) as f32;
            line_num * self.gap_line_mm()
        } else {
            0.0
        }
    }

    /// Get the line gap (mm)
    fn gap_line_mm(&self) -> f32 {
        let gaps = self.gap_line_count();
        if gaps > 0 {
            self.excess_line_mm() / gaps as f32
        } else {
            0.0
        }
    }

    /// Get excess height for line gaps (mm).
    fn excess_line_mm(&self) -> f32 {
        let pix_mm = self.pitch_vert_mm() * self.pixel_height() as f32;
        (self.pixel_height_mm() - pix_mm).max(0.0)
    }

    /// Get the character width as u8
    pub(crate) fn char_width(&self) -> u8 {
        self.vms_cfg.char_width_pixels
    }

    /// Get the character height as u8
    pub(crate) fn char_height(&self) -> u8 {
        self.vms_cfg.char_height_pixels
    }

    /// Get the color scheme value
    fn color_scheme(&self) -> ColorScheme {
        self.multi_cfg.color_scheme
    }

    /// Get the default foreground color
    fn foreground_default_rgb(&self) -> (u8, u8, u8) {
        match self.color_scheme() {
            ColorScheme::ColorClassic | ColorScheme::Color24Bit => {
                self.multi_cfg.default_foreground_rgb.rgb()
            }
            _ => {
                let mono = &self.vms_cfg.monochrome_color;
                (mono[0], mono[1], mono[2])
            }
        }
    }

    /// Get the default background color
    fn background_default_rgb(&self) -> (u8, u8, u8) {
        match self.color_scheme() {
            ColorScheme::ColorClassic | ColorScheme::Color24Bit => {
                self.multi_cfg.default_background_rgb.rgb()
            }
            _ => {
                let mono = &self.vms_cfg.monochrome_color;
                (mono[3], mono[4], mono[5])
            }
        }
    }

    /// Get the sign's color context
    pub(crate) fn color_ctx(&self) -> ColorCtx {
        let color_scheme = self.color_scheme();
        let fg_default = self.foreground_default_rgb();
        let bg_default = self.background_default_rgb();
        ColorCtx::new(color_scheme, fg_default, bg_default)
    }

    /// Get a rectangle of the full sign
    pub(crate) fn full_rect(&self) -> Rectangle {
        Rectangle::new(1, 1, self.pixel_width(), self.pixel_height())
    }

    /// Get the number of lines in a text rectangle
    pub(crate) fn rect_lines(&self, rect: Rectangle, font_num: u8) -> usize {
        let mut n_lines = 0;
        if let Some(font) = self.font_definition().lookup(font_num) {
            let height = u16::from(font.height);
            let spacing = height + u16::from(font.line_spacing);
            let mut rheight = rect.height;
            while rheight >= height {
                n_lines += 1;
                rheight = rheight.saturating_sub(spacing);
            }
        }
        n_lines
    }
}
