// render.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! This module is for NTCIP 1203 DMS rendering.
use crate::dms::font::{Font, FontTable};
use crate::dms::graphic::Graphic;
use crate::dms::multi::{
    ColorCtx, JustificationLine, JustificationPage, MultiStr, Rectangle,
    SyntaxError, Tag, Value,
};
use crate::dms::sign::Dms;
use fstr::FStr;
use log::debug;
use pix::{rgb::SRgb8, Raster, Region};
use std::fmt::Write;

/// Result type
type Result<T> = std::result::Result<T, SyntaxError>;

/// Maximum number of text rectangles per page
const MAX_TEXT_RECTANGLES: u32 = 50;

/// Rendered DMS page
pub struct Page {
    /// Page raster
    pub raster: Raster<SRgb8>,

    /// Page duration (1/10 s)
    pub duration_ds: u16,
}

/// Page state
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PageState {
    /// Page ON with flag for more pages
    On(bool),

    /// Page OFF with flag for more pages
    Off(bool),

    /// All pages done
    Done,
}

/// Rendering state
#[derive(Clone)]
struct RenderState {
    /// Color context
    color_ctx: ColorCtx,

    /// Current page-on time in deciseconds
    page_on_time_ds: u8,

    /// Current page-off time in deciseconds
    page_off_time_ds: u8,

    /// Current text rectangle
    text_rectangle: Rectangle,

    /// Current page justification
    just_page: JustificationPage,

    /// Current line justification
    just_line: JustificationLine,

    /// Font number
    font_num: u8,

    /// Font version_id
    font_version_id: Option<u16>,

    /// Current specified line spacing
    line_spacing: Option<u8>,

    /// Current specified char spacing
    char_spacing: Option<u8>,

    /// Current line number
    line_number: u8,

    /// Current text span number
    span_number: u8,
}

/// Span of text
#[derive(Clone)]
enum Span<'a> {
    /// Slice of text
    Text(RenderState, &'a str),

    /// Hexadecimal character
    HexChar(RenderState, FStr<4>),
}

/// Text line
#[derive(Clone)]
struct TextLine {
    /// Height in pixels
    height: u16,

    /// Font spacing
    font_spacing: u16,

    /// Specified line spacing
    line_spacing: Option<u16>,
}

/// Page renderer for dynamic message signs
pub struct Pages<'a, const C: usize, const F: usize, const G: usize> {
    /// Sign to render
    dms: &'a Dms<C, F, G>,

    /// Default rendering state
    default_state: RenderState,

    /// Current render state
    render_state: RenderState,

    /// Page state
    page_state: PageState,

    /// MULTI string iterator
    values: MultiStr<'a>,

    /// Spans for current text rectangle
    spans: Vec<Span<'a>>,
}

impl PageState {
    /// Get state for new page
    fn new_page(page_off: bool) -> Self {
        if page_off {
            PageState::Off(true)
        } else {
            PageState::On(true)
        }
    }

    /// Get state for end of message
    fn done(page_off: bool) -> Self {
        if page_off {
            PageState::Off(false)
        } else {
            PageState::Done
        }
    }

    /// Get next page state
    fn next_state(self) -> Self {
        match self {
            PageState::On(true) => PageState::Off(true),
            PageState::On(false) => PageState::Done,
            PageState::Off(true) => PageState::On(false),
            PageState::Off(false) => PageState::Done,
            PageState::Done => PageState::Done,
        }
    }
}

impl RenderState {
    /// Create a new render state
    fn new<const C: usize, const F: usize, const G: usize>(
        dms: &Dms<C, F, G>,
    ) -> Self {
        RenderState {
            color_ctx: dms.color_ctx(),
            page_on_time_ds: dms.multi_cfg.default_page_on_time,
            page_off_time_ds: dms.multi_cfg.default_page_off_time,
            text_rectangle: Rectangle::new(
                1,
                1,
                dms.vms_cfg.sign_width_pixels,
                dms.vms_cfg.sign_height_pixels,
            ),
            just_page: dms.multi_cfg.default_justification_page,
            just_line: dms.multi_cfg.default_justification_line,
            font_num: dms.multi_cfg.default_font,
            font_version_id: None,
            line_spacing: None,
            char_spacing: None,
            line_number: 0,
            span_number: 0,
        }
    }

    /// Get the background RGB color
    fn background_rgb(&self) -> SRgb8 {
        let (r, g, b) = self.color_ctx.background_rgb();
        SRgb8::new(r, g, b)
    }

    /// Get the foreground RGB color
    fn foreground_rgb(&self) -> SRgb8 {
        let (r, g, b) = self.color_ctx.foreground_rgb();
        SRgb8::new(r, g, b)
    }

    /// Check if states match for text spans
    fn matches_span(&self, rhs: &Self) -> bool {
        self.just_page == rhs.just_page
            && self.line_number == rhs.line_number
            && self.just_line == rhs.just_line
    }

    /// Check if states match for lines
    fn matches_line(&self, rhs: &Self) -> bool {
        self.just_page == rhs.just_page
    }

    /// Lookup current font in cache
    fn font<'a, const C: usize, const F: usize>(
        &self,
        fonts: &'a FontTable<C, F>,
    ) -> Result<&'a Font<C>> {
        match (fonts.font(self.font_num), self.font_version_id) {
            (Some(f), Some(vid)) => {
                if vid == f.version_id() {
                    Ok(f)
                } else {
                    Err(SyntaxError::FontVersionID)
                }
            }
            (Some(f), None) => Ok(f),
            (None, _) => Err(SyntaxError::FontNotDefined(self.font_num)),
        }
    }
}

impl<'a> Span<'a> {
    /// Get span as a str slice
    fn as_str(&self) -> &str {
        match self {
            Span::Text(_state, text) => text,
            Span::HexChar(_state, hc) => hc.slice_to_terminator('\0'),
        }
    }

    /// Get the render state
    fn state(&self) -> &RenderState {
        match self {
            Span::Text(state, _text) => state,
            Span::HexChar(state, _hc) => state,
        }
    }

    /// Get the width of a text span
    fn width<const C: usize, const F: usize>(
        &self,
        fonts: &FontTable<C, F>,
    ) -> Result<u16> {
        let font = self.state().font(fonts)?;
        let cs = self.char_spacing_fonts(fonts)?;
        Ok(font.text_width(self.as_str(), Some(cs))?)
    }

    /// Get the char spacing
    fn char_spacing_fonts<const C: usize, const F: usize>(
        &self,
        fonts: &FontTable<C, F>,
    ) -> Result<u16> {
        let state = self.state();
        match state.char_spacing {
            Some(sp) => Ok(sp.into()),
            None => Ok(state.font(fonts)?.char_spacing.into()),
        }
    }

    /// Get the char spacing
    fn char_spacing_font<const C: usize>(&self, font: &Font<C>) -> u8 {
        match self.state().char_spacing {
            Some(sp) => sp,
            None => font.char_spacing,
        }
    }

    /// Get the char spacing from a previous span
    fn char_spacing_between<const C: usize, const F: usize>(
        &self,
        prev: &Span,
        fonts: &FontTable<C, F>,
    ) -> Result<u16> {
        if let Some(c) = self.state().char_spacing {
            Ok(c.into())
        } else {
            // NTCIP 1203 fontCharSpacing:
            // "... the average character spacing of the two fonts,
            // rounded up to the nearest whole pixel ..." ???
            let psc = prev.char_spacing_fonts(fonts)?;
            let sc = self.char_spacing_fonts(fonts)?;
            Ok(((psc + sc) >> 1) + ((psc + sc) & 1))
        }
    }

    /// Get the height of a text span
    fn height<const C: usize, const F: usize>(
        &self,
        fonts: &FontTable<C, F>,
    ) -> Result<u16> {
        Ok(self.state().font(fonts)?.height.into())
    }

    /// Get the font line spacing
    fn font_spacing<const C: usize, const F: usize>(
        &self,
        fonts: &FontTable<C, F>,
    ) -> Result<u16> {
        Ok(self.state().font(fonts)?.line_spacing.into())
    }

    /// Get the line spacing
    fn line_spacing(&self) -> Option<u16> {
        self.state().line_spacing.map(|sp| sp.into())
    }

    /// Render the text span
    fn render_text<const C: usize>(
        &self,
        raster: &mut Raster<SRgb8>,
        font: &Font<C>,
        x: i32,
        y: i32,
    ) -> Result<()> {
        let cs = self.char_spacing_font(font).into();
        let cf = self.state().foreground_rgb();
        Ok(font.render_text(raster, self.as_str(), x, y, cs, cf)?)
    }
}

impl TextLine {
    /// Create a new text line.
    fn new(height: u16, font_spacing: u16, line_spacing: Option<u16>) -> Self {
        TextLine {
            height,
            font_spacing,
            line_spacing,
        }
    }

    /// Combine a text line with another.
    fn combine(&mut self, rhs: &Self) {
        self.height = self.height.max(rhs.height);
        self.font_spacing = self.font_spacing.max(rhs.font_spacing);
        self.line_spacing = self.line_spacing.or(rhs.line_spacing);
    }

    /// Get the spacing between two text lines.
    fn spacing(&self, rhs: &Self) -> u16 {
        if let Some(ls) = self.line_spacing {
            ls
        } else {
            // NTCIP 1203 fontLineSpacing:
            // "The number of pixels between adjacent lines
            // is the average of the 2 line spacings of each
            // line, rounded up to the nearest whole pixel."
            let ps = rhs.font_spacing;
            let fs = self.font_spacing;
            ((ps + fs) >> 1) + ((ps + fs) & 1)
        }
    }
}

impl<'a, const C: usize, const F: usize, const G: usize> Pages<'a, C, F, G> {
    /// Create a new DMS page renderer.
    ///
    /// * `dms` Sign to render.
    /// * `ms` MULTI string to render.
    ///
    /// Some tags are not supported:
    ///
    /// * `[f…]`: [Field]
    /// * `[fl…]`: [Flash]
    /// * `[ms…]`: [Manufacturer Specific]
    /// * `[mv…]`: [Moving Text]
    ///
    /// [field]: multi/enum.Tag.html#variant.F1
    /// [flash]: multi/enum.Tag.html#variant.Fl
    /// [manufacturer specific]: multi/enum.Tag.html#variant.Ms
    /// [moving text]: multi/enum.Tag.html#variant.Mv
    pub fn new(dms: &'a Dms<C, F, G>, ms: &'a str) -> Self {
        let default_state = RenderState::new(dms);
        let render_state = default_state.clone();
        Pages {
            dms,
            default_state,
            render_state,
            page_state: PageState::On(true),
            values: MultiStr::new(ms),
            spans: Vec::new(),
        }
    }

    /// Get the font definition
    fn fonts(&self) -> &FontTable<C, F> {
        self.dms.font_definition()
    }

    /// Get the character width (1 for variable width)
    fn char_width(&self) -> u16 {
        self.dms.char_width().max(1).into()
    }

    /// Get the character height (1 for variable height)
    fn char_height(&self) -> u16 {
        self.dms.char_height().max(1).into()
    }

    /// Get the page-on time (deciseconds)
    fn page_on_time_ds(&self) -> u16 {
        self.render_state.page_on_time_ds.into()
    }

    /// Get the page-off time (deciseconds)
    fn page_off_time_ds(&self) -> u16 {
        self.render_state.page_off_time_ds.into()
    }

    /// Render an OFF page
    fn render_off_page(&mut self) -> Page {
        self.page_state = self.page_state.next_state();
        Page {
            raster: self.build_raster(),
            duration_ds: self.page_off_time_ds(),
        }
    }

    /// Build a raster
    fn build_raster(&self) -> Raster<SRgb8> {
        let width = self.render_state.text_rectangle.width.into();
        let height = self.render_state.text_rectangle.height.into();
        let clr = self.render_state.background_rgb();
        Raster::with_color(width, height, clr)
    }

    /// Render an ON page
    fn render_on_page(&mut self) -> Result<Page> {
        self.check_unsupported()?;
        self.update_page_state()?;
        let mut raster = self.build_raster();
        debug!("render_on_page {}x{}", raster.width(), raster.height());
        let mut n_text_rectangles = 0;
        self.page_state = PageState::On(false);
        while self.page_state == PageState::On(false) {
            self.render_graphics(&mut raster)?;
            self.render_text_rectangle(&mut raster)?;
            n_text_rectangles += 1;
            if n_text_rectangles > MAX_TEXT_RECTANGLES {
                return Err(SyntaxError::Other("Too many text rectangles"));
            }
        }
        Ok(Page {
            raster,
            duration_ds: self.page_on_time_ds(),
        })
    }

    /// Check for unsupported MULTI tags in a page
    fn check_unsupported(&self) -> Result<()> {
        for value in self.values.clone() {
            let val = value?;
            if let Some(tag) = val.tag() {
                if !self.dms.multi_cfg.supported_multi_tags.contains(tag) {
                    return Err(SyntaxError::UnsupportedTag(val.into()));
                }
                if tag == Tag::Np {
                    break;
                }
            }
        }
        Ok(())
    }

    /// Iterate through page values to update its state
    fn update_page_state(&mut self) -> Result<()> {
        let ds = &self.default_state;
        let rs = &mut self.render_state;
        // Set these back to default values
        rs.text_rectangle = ds.text_rectangle;
        rs.line_spacing = ds.line_spacing;
        rs.line_number = 0;
        rs.span_number = 0;
        for value in self.values.clone() {
            let val = value?;
            match val {
                Value::ColorBackground(clr) | Value::PageBackground(clr) => {
                    rs.color_ctx.set_background(clr, &val)?;
                }
                Value::NewPage() => break,
                Value::PageTime(on, off) => {
                    rs.page_on_time_ds = on.unwrap_or(ds.page_on_time_ds);
                    rs.page_off_time_ds = off.unwrap_or(ds.page_off_time_ds);
                }
                _ => (),
            }
        }
        Ok(())
    }

    /// Render graphics and color rectangles
    fn render_graphics(&mut self, raster: &mut Raster<SRgb8>) -> Result<()> {
        let mut rs = self.render_state.clone();
        for value in self.values.clone() {
            let val = value?;
            match val {
                Value::ColorBackground(clr) => {
                    rs.color_ctx.set_background(clr, &val)?;
                }
                Value::ColorForeground(clr) => {
                    rs.color_ctx.set_foreground(clr, &val)?;
                }
                Value::ColorRectangle(rect, clr) => {
                    let mut ctx = rs.color_ctx.clone();
                    // only set foreground color in cloned context
                    ctx.set_foreground(Some(clr), &val)?;
                    let (r, g, b) = ctx.foreground_rgb();
                    let rgb = SRgb8::new(r, g, b);
                    render_rect(raster, rect, rgb, &val)?;
                }
                Value::Field(_, _) => unimplemented!(),
                Value::Flash(_, _, _) => unimplemented!(),
                Value::FlashEnd() => unimplemented!(),
                Value::Graphic(gn, None) => {
                    let g = self.graphic(gn, None)?;
                    g.render_graphic(raster, 1, 1, &rs.color_ctx)?;
                }
                Value::Graphic(gn, Some((x, y, gid))) => {
                    let g = self.graphic(gn, gid)?;
                    let x = x.into();
                    let y = y.into();
                    g.render_graphic(raster, x, y, &rs.color_ctx)?;
                }
                Value::ManufacturerSpecific(_, _) => unimplemented!(),
                Value::ManufacturerSpecificEnd(_, _) => unimplemented!(),
                Value::MovingText(_, _, _, _, _, _) => unimplemented!(),
                Value::NewPage() | Value::TextRectangle(_) => break,
                _ => (),
            }
        }
        Ok(())
    }

    /// Lookup a graphic from the table
    fn graphic(&self, gn: u8, gid: Option<u16>) -> Result<&'a Graphic> {
        let graphics = self.dms.graphic_definition();
        match (graphics.graphic(gn), gid) {
            (Some(g), None) => Ok(g),
            (Some(g), Some(gid)) => {
                if gid == g.version_id() {
                    Ok(g)
                } else {
                    Err(SyntaxError::GraphicID)
                }
            }
            (None, _) => Err(SyntaxError::GraphicNotDefined(gn)),
        }
    }

    /// Render one text rectangle
    fn render_text_rectangle(
        &mut self,
        raster: &mut Raster<SRgb8>,
    ) -> Result<()> {
        let is_char_matrix = self.dms.char_width() > 0;
        let is_char_or_line_matrix = self.dms.char_height() > 0;
        let page_off = self.page_off_time_ds() > 0;
        let ds = &self.default_state;
        let mut line_blank = true;
        self.page_state = PageState::done(page_off);
        self.spans.clear();
        for value in self.values.by_ref() {
            let val = value?;
            match val {
                Value::ColorForeground(clr) => {
                    self.render_state.color_ctx.set_foreground(clr, &val)?;
                }
                Value::Font(f) => {
                    let rs = &mut self.render_state;
                    rs.font_num = f.map_or(ds.font_num, |t| t.0);
                    rs.font_version_id = f.map_or(ds.font_version_id, |t| t.1);
                }
                #[allow(deprecated)]
                Value::JustificationLine(Some(JustificationLine::Other)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationLine(Some(JustificationLine::Full)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationLine(jl) => {
                    let rs = &mut self.render_state;
                    rs.just_line = jl.unwrap_or(ds.just_line);
                    rs.span_number = 0;
                }
                #[allow(deprecated)]
                Value::JustificationPage(Some(JustificationPage::Other)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationPage(jp) => {
                    let rs = &mut self.render_state;
                    rs.just_page = jp.unwrap_or(ds.just_page);
                    rs.line_number = 0;
                    rs.span_number = 0;
                }
                Value::NewLine(ls) => {
                    if let Some(ls) = ls {
                        if is_char_or_line_matrix && ls > 0 {
                            return Err(SyntaxError::UnsupportedTagValue(
                                val.into(),
                            ));
                        }
                    }
                    let rs = &mut self.render_state;
                    // Insert an empty text span for blank lines.
                    if line_blank {
                        self.spans.push(Span::Text(rs.clone(), ""));
                    }
                    line_blank = true;
                    rs.line_spacing = ls;
                    rs.line_number += 1;
                    rs.span_number = 0;
                }
                Value::NewPage() => {
                    self.page_state = PageState::new_page(page_off);
                    break;
                }
                Value::SpacingCharacter(sc) => {
                    if is_char_matrix && sc > 0 {
                        return Err(SyntaxError::UnsupportedTagValue(
                            val.into(),
                        ));
                    }
                    self.render_state.char_spacing = Some(sc);
                }
                Value::SpacingCharacterEnd() => {
                    self.render_state.char_spacing = None;
                }
                Value::TextRectangle(rect) => {
                    self.page_state = PageState::On(false);
                    match self.update_text_rectangle(rect) {
                        Some(rect) => {
                            let rs = &mut self.render_state;
                            rs.text_rectangle = rect;
                            rs.line_number = 0;
                            rs.span_number = 0;
                        }
                        None => {
                            return Err(SyntaxError::UnsupportedTagValue(
                                val.into(),
                            ))
                        }
                    }
                    break;
                }
                Value::Text(t) => {
                    let rs = &mut self.render_state;
                    self.spans.push(Span::Text(rs.clone(), t));
                    rs.span_number += 1;
                    line_blank = false;
                }
                Value::HexadecimalCharacter(hc) => {
                    match std::char::from_u32(hc.into()) {
                        Some(c) => {
                            let rs = &mut self.render_state;
                            let mut fs = FStr::repeat(0);
                            write!(fs.writer(), "{c}").unwrap();
                            self.spans.push(Span::HexChar(rs.clone(), fs));
                            rs.span_number += 1;
                            line_blank = false;
                        }
                        None => {
                            // Invalid code point (surrogate in D800-DFFF range)
                            return Err(SyntaxError::UnsupportedTagValue(
                                val.into(),
                            ));
                        }
                    }
                }
                _ => (),
            }
        }
        self.render_text_spans(raster)?;
        Ok(())
    }

    /// Update the text rectangle
    fn update_text_rectangle(&self, rect: Rectangle) -> Option<Rectangle> {
        let rect = rect.extend_width_height(self.default_state.text_rectangle);
        if rect.intersection(self.default_state.text_rectangle) != rect {
            return None;
        }
        let cw = self.char_width();
        debug_assert!(cw > 0);
        // Check text rectangle matches character boundaries
        let x = rect.x - 1;
        if x % cw != 0 || rect.width % cw != 0 {
            return None;
        }
        let lh = self.char_height();
        debug_assert!(lh > 0);
        // Check text rectangle matches line boundaries
        let y = rect.y - 1;
        if y % lh != 0 || rect.height % lh != 0 {
            return None;
        }
        Some(rect)
    }

    /// Render spans for the current text rectangle
    fn render_text_spans(&self, raster: &mut Raster<SRgb8>) -> Result<()> {
        self.check_justification()?;
        for span in &self.spans {
            let x = self.span_x(span)?.into();
            let y = self.span_y(span)?.into();
            let font = span.state().font(self.fonts())?;
            span.render_text(raster, font, x, y)?;
        }
        Ok(())
    }

    /// Check page and line justification ordering
    fn check_justification(&self) -> Result<()> {
        #[allow(deprecated)]
        let mut jp = JustificationPage::Other;
        #[allow(deprecated)]
        let mut jl = JustificationLine::Other;
        let mut ln = 0;
        for span in &self.spans {
            let just_page = span.state().just_page;
            let just_line = span.state().just_line;
            let line_number = span.state().line_number;
            if just_page < jp
                || (just_page == jp && line_number == ln && just_line < jl)
            {
                return Err(SyntaxError::TagConflict);
            }
            jp = just_page;
            jl = just_line;
            ln = line_number;
        }
        Ok(())
    }

    /// Get the X position of a text span
    fn span_x(&self, span: &Span) -> Result<u16> {
        match span.state().just_line {
            JustificationLine::Left => self.span_x_left(span),
            JustificationLine::Center => self.span_x_center(span),
            JustificationLine::Right => self.span_x_right(span),
            _ => unreachable!(),
        }
    }

    /// Get the X position of a left-justified text span
    fn span_x_left(&self, span: &Span) -> Result<u16> {
        let left = span.state().text_rectangle.x - 1;
        let (before, _) = self.offset_horiz(span)?;
        Ok(left + before)
    }

    /// Get the X position of a center-justified text span
    fn span_x_center(&self, span: &Span) -> Result<u16> {
        let left = span.state().text_rectangle.x - 1;
        let w = span.state().text_rectangle.width;
        let (before, after) = self.offset_horiz(span)?;
        let offset = (w - before - after) / 2; // offset for centering
        let x = left + offset + before;
        let cw = self.char_width();
        // Truncate to character-width boundary
        Ok((x / cw) * cw)
    }

    /// Get the X position of a right-justified span
    fn span_x_right(&self, span: &Span) -> Result<u16> {
        let left = span.state().text_rectangle.x - 1;
        let w = span.state().text_rectangle.width;
        let (_, after) = self.offset_horiz(span)?;
        Ok(left + w - after)
    }

    /// Calculate horizontal offsets of a span.
    ///
    /// Returns a tuple of (before, after) widths of matching spans.
    fn offset_horiz(&self, text_span: &Span) -> Result<(u16, u16)> {
        debug!("offset_horiz '{}'", text_span.as_str());
        let rs = &text_span.state();
        let mut before = 0;
        let mut after = 0;
        let mut pspan = None;
        for span in self.spans.iter().filter(|s| rs.matches_span(s.state())) {
            if let Some(ps) = pspan {
                let w = span.char_spacing_between(ps, self.fonts())?;
                if span.state().span_number <= rs.span_number {
                    before += w
                } else {
                    after += w
                }
                debug!("  spacing {w} before {before} after {after}");
            }
            let w = span.width(self.fonts())?;
            if span.state().span_number < rs.span_number {
                before += w
            } else {
                after += w
            }
            debug!("  span '{}'  before {before} after {after}", span.as_str());
            pspan = Some(span);
        }
        if before + after <= rs.text_rectangle.width {
            Ok((before, after))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }

    /// Get the Y position of a text span
    fn span_y(&self, span: &Span) -> Result<u16> {
        let b = self.baseline(span)?;
        let h = span.height(self.fonts())?;
        debug_assert!(b >= h);
        Ok(b - h)
    }

    /// Get the baseline of a text span
    fn baseline(&self, span: &Span) -> Result<u16> {
        match span.state().just_page {
            JustificationPage::Top => self.baseline_top(span),
            JustificationPage::Middle => self.baseline_middle(span),
            JustificationPage::Bottom => self.baseline_bottom(span),
            _ => unreachable!(),
        }
    }

    /// Get the baseline of a top-justified span
    fn baseline_top(&self, span: &Span) -> Result<u16> {
        let top = span.state().text_rectangle.y - 1;
        let (above, _) = self.offset_vert(span)?;
        Ok(top + above)
    }

    /// Get the baseline of a middle-justified span
    fn baseline_middle(&self, span: &Span) -> Result<u16> {
        let top = span.state().text_rectangle.y - 1;
        let h = span.state().text_rectangle.height;
        let (above, below) = self.offset_vert(span)?;
        let offset = (h - above - below) / 2; // offset for centering
        let y = top + offset + above;
        let ch = self.char_height();
        // Truncate to line-height boundary
        Ok((y / ch) * ch)
    }

    /// Get the baseline of a bottom-justified span
    fn baseline_bottom(&self, span: &Span) -> Result<u16> {
        let top = span.state().text_rectangle.y - 1;
        let h = span.state().text_rectangle.height;
        let (_, below) = self.offset_vert(span)?;
        Ok(top + h - below)
    }

    /// Calculate vertical offset of a span.
    ///
    /// Returns a tuple of (above, below) heights of matching lines.
    fn offset_vert(&self, text_span: &Span) -> Result<(u16, u16)> {
        debug!("offset_vert '{}'", text_span.as_str());
        let rs = &text_span.state();
        let mut lines = vec![];
        for span in self.spans.iter().filter(|s| rs.matches_line(s.state())) {
            let ln = usize::from(span.state().line_number);
            let h = span.height(self.fonts())?;
            let fs = span.font_spacing(self.fonts())?;
            let ls = span.line_spacing();
            let line = TextLine::new(h, fs, ls);
            if ln >= lines.len() {
                lines.push(line);
            } else {
                lines[ln].combine(&line);
            }
        }
        let sln = usize::from(rs.line_number);
        let mut above = 0;
        let mut below = 0;
        for ln in 0..lines.len() {
            let line = &lines[ln];
            if ln > 0 {
                let h = line.spacing(&lines[ln - 1]);
                if ln <= sln {
                    above += h
                } else {
                    below += h
                }
                debug!("  spacing {}  above {} below {}", h, above, below);
            }
            let h = line.height;
            if ln <= sln {
                above += h
            } else {
                below += h
            }
            debug!("  line {}  above {} below {}", ln, above, below);
        }
        if above + below <= rs.text_rectangle.height {
            Ok((above, below))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }
}

impl<'a, const C: usize, const F: usize, const G: usize> Iterator
    for Pages<'a, C, F, G>
{
    type Item = Result<Page>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.page_state {
            PageState::On(_) => Some(self.render_on_page()),
            PageState::Off(_) => Some(Ok(self.render_off_page())),
            _ => None,
        }
    }
}

/// Render a color rectangle.
fn render_rect(
    raster: &mut Raster<SRgb8>,
    rect: Rectangle,
    clr: SRgb8,
    value: &Value,
) -> Result<()> {
    debug_assert!(rect.x > 0);
    debug_assert!(rect.y > 0);
    let width = raster.width().try_into().unwrap();
    let height = raster.height().try_into().unwrap();
    let full_rect = Rectangle::new(1, 1, width, height);
    let rect = rect.extend_width_height(full_rect);
    if rect.intersection(full_rect) == rect {
        let rx = i32::from(rect.x) - 1;
        let ry = i32::from(rect.y) - 1;
        let rw = u32::from(rect.width);
        let rh = u32::from(rect.height);
        let region = Region::new(rx, ry, rw, rh);
        raster.copy_color(region, clr);
        Ok(())
    } else {
        Err(SyntaxError::UnsupportedTagValue(value.into()))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::config::{MultiCfg, VmsCfg};
    use crate::dms::font::tfon;
    use crate::dms::multi::{ColorClassic, ColorScheme};

    fn font_table() -> FontTable<128, 4> {
        let mut fonts = FontTable::default();
        let buf = include_str!("../../test/F07-C.tfon");
        let f = fonts.font_mut(0).unwrap();
        *f = tfon::parse(&buf[..]).unwrap();
        let buf = include_str!("../../test/F07-L.tfon");
        let f = fonts.font_mut(0).unwrap();
        *f = tfon::parse(&buf[..]).unwrap();
        let buf = include_str!("../../test/F08.tfon");
        let f = fonts.font_mut(0).unwrap();
        *f = tfon::parse(&buf[..]).unwrap();
        fonts
    }

    fn render_full(ms: &str) -> Result<Vec<Page>> {
        let dms = Dms::<128, 4, 0>::builder()
            .with_vms_cfg(VmsCfg {
                char_height_pixels: 0,
                char_width_pixels: 0,
                sign_height_pixels: 30,
                sign_width_pixels: 60,
                ..Default::default()
            })
            .with_font_definition(font_table())
            .with_multi_cfg(MultiCfg {
                default_justification_line: JustificationLine::Left,
                default_justification_page: JustificationPage::Top,
                default_font: 8,
                color_scheme: ColorScheme::Color24Bit,
                default_foreground_rgb: ColorClassic::White.rgb().into(),
                ..Default::default()
            })
            .build()
            .unwrap();
        Pages::new(&dms, ms).collect()
    }

    #[test]
    fn page_count() {
        assert_eq!(render_full("").unwrap().len(), 1);
        assert_eq!(render_full("1").unwrap().len(), 1);
        assert_eq!(render_full("[np]").unwrap().len(), 2);
        assert_eq!(render_full("1[NP]").unwrap().len(), 2);
        assert_eq!(render_full("1[Np]2").unwrap().len(), 2);
        assert_eq!(render_full("1[np]2[nP]").unwrap().len(), 3);
        assert_eq!(render_full("[pto1]1[np]2").unwrap().len(), 4);
        assert_eq!(render_full("[pto1][np]").unwrap().len(), 4);
        let pages = render_full(
            "[fo8][jl2][cf255,255,255]RAMP A[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP B[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP C[jl4][cf255,255,0]FULL",
        )
        .unwrap();
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn page_times() {
        assert_eq!(render_full("").unwrap()[0].duration_ds, 30);
        assert_eq!(render_full("[pt25o10]").unwrap()[0].duration_ds, 25);
        assert_eq!(render_full("[pt20o10]").unwrap()[1].duration_ds, 10);
        assert_eq!(render_full("[pt30o5][np]").unwrap()[2].duration_ds, 30);
        assert_eq!(render_full("[pto15][np]").unwrap()[3].duration_ds, 15);
    }

    fn justify_dot(ms: &str, i: usize) {
        let mut raster = Raster::<SRgb8>::with_clear(60, 30);
        raster.pixels_mut()[i] = SRgb8::new(255, 255, 255);
        let pages = render_full(ms).unwrap();
        assert_eq!(pages.len(), 1);
        let page = &pages[0].raster;
        for (i, (p0, p1)) in
            page.pixels().iter().zip(raster.pixels()).enumerate()
        {
            dbg!(i);
            assert_eq!(p0, p1);
        }
        assert_eq!(page.pixels(), raster.pixels());
    }

    #[test]
    fn left_justify() {
        // 60 pixels wide * 7 = 420
        justify_dot(".", 420);
    }

    #[test]
    fn center_justify() {
        justify_dot("[jl3].", 449);
    }

    #[test]
    fn right_justify() {
        justify_dot("[jl4].", 478);
    }

    #[test]
    fn middle_justify() {
        justify_dot("[jp3].", 1080);
    }

    #[test]
    fn bottom_justify() {
        justify_dot("[jp4].", 1740);
    }

    #[test]
    fn char_spacing() {
        justify_dot(" .", 423);
        justify_dot("[sc4] .[/sc]", 425);
        justify_dot("[sc5] .[/sc]", 426);
    }

    #[test]
    fn line_spacing() {
        justify_dot("[nl].", 1020);
        justify_dot("[nl1].", 960);
        justify_dot("[nl3].", 1080);
    }

    #[test]
    fn text_rectangles() {
        justify_dot("[tr2,1,10,10].", 421);
        justify_dot("[tr1,2,10,10].", 480);
        justify_dot("[tr2,2,10,10].", 481);
    }

    fn render_char(ms: &str) -> Result<Vec<Page>> {
        let dms = Dms::<128, 4, 0>::builder()
            .with_vms_cfg(VmsCfg {
                char_height_pixels: 7,
                char_width_pixels: 5,
                sign_height_pixels: 21,
                sign_width_pixels: 100,
                ..Default::default()
            })
            .with_font_definition(font_table())
            .with_multi_cfg(MultiCfg {
                default_justification_line: JustificationLine::Left,
                default_justification_page: JustificationPage::Top,
                default_font: 5,
                ..Default::default()
            })
            .build()
            .unwrap();
        Pages::new(&dms, ms).collect()
    }

    #[test]
    fn page_char_matrix() {
        match render_char("[tr1,1,12,12]") {
            Err(SyntaxError::UnsupportedTagValue(_)) => assert!(true),
            _ => assert!(false),
        }
        match render_char("[tr1,1,50,12]") {
            Err(SyntaxError::UnsupportedTagValue(_)) => assert!(true),
            _ => assert!(false),
        }
        match render_char("[tr1,1,12,14]") {
            Err(SyntaxError::UnsupportedTagValue(_)) => assert!(true),
            _ => assert!(false),
        }
        match render_char("[tr1,1,50,14]") {
            Ok(_) => assert!(true),
            _ => assert!(false),
        }
        match render_char("[pb9]") {
            Err(SyntaxError::UnsupportedTagValue(_)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn char_matrix_spacing() {
        match render_char("[sc1][/sc]") {
            Err(SyntaxError::UnsupportedTagValue(_)) => assert!(true),
            _ => assert!(false),
        }
        match render_char("[sc0][/sc]") {
            Ok(_) => assert!(true),
            _ => assert!(false),
        }
    }
}
