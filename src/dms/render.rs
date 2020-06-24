// render.rs
//
// Copyright (C) 2018-2020  Minnesota Department of Transportation
//
//! This module is for NTCIP 1203 DMS rendering.
//!
use crate::dms::multi::{
    ColorCtx, JustificationLine, JustificationPage, Parser, Rectangle,
    SyntaxError, Value,
};
use crate::dms::{Font, FontCache, GraphicCache, Result};
use log::debug;
use pix::{rgb::SRgb8, Raster, Region};

/// Maximum number of text rectangles per page
const MAX_TEXT_RECTANGLES: u32 = 50;

/// Page state
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PageState {
    /// Next page ON
    On(bool),
    /// Next page OFF
    Off(bool),
    /// All pages done
    Done,
}

/// Page render state
#[derive(Clone)]
pub struct State {
    /// Color context
    color_ctx: ColorCtx,
    /// Character width in pixels
    char_width: u8,
    /// Character height in pixels
    char_height: u8,
    /// Page-on time in deciseconds
    page_on_time_ds: u8,
    /// Page-off time in deciseconds
    page_off_time_ds: u8,
    /// Current text rectangle
    text_rectangle: Rectangle,
    /// Current page justification
    just_page: JustificationPage,
    /// Current line justification
    just_line: JustificationLine,
    /// Current line number
    line_number: u8,
    /// Current text span number
    span_number: u8,
    /// Current specified line spacing
    line_spacing: Option<u8>,
    /// Current specified char spacing
    char_spacing: Option<u8>,
    /// Font number
    font_num: u8,
    /// Font version_id
    font_version_id: Option<u16>,
}

/// Text span
#[derive(Clone)]
struct TextSpan {
    /// Render state at start of span
    state: State,
    /// Text string
    text: String,
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

/// MULTI renderer (iterator)
pub struct Renderer<'a> {
    /// Font cache
    fonts: &'a FontCache,
    /// Graphic cache
    graphics: &'a GraphicCache,
    /// Default rendering state
    default_state: State,
    /// Current render state
    state: State,
    /// Page state
    page_state: PageState,
    /// MULTI parser
    parser: Parser<'a>,
    /// Spans for current text rectangle
    spans: Vec<TextSpan>,
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

impl State {
    /// Create a new render state.
    pub fn new(
        color_ctx: ColorCtx,
        char_width: u8,
        char_height: u8,
        page_on_time_ds: u8,
        page_off_time_ds: u8,
        text_rectangle: Rectangle,
        just_page: JustificationPage,
        just_line: JustificationLine,
        font_num: u8,
        font_version_id: Option<u16>,
    ) -> Self {
        State {
            color_ctx,
            char_width,
            char_height,
            page_on_time_ds,
            page_off_time_ds,
            text_rectangle,
            just_page,
            just_line,
            line_number: 0,
            span_number: 0,
            line_spacing: None,
            char_spacing: None,
            font_num,
            font_version_id,
        }
    }

    /// Check if the sign is a character-matrix.
    fn is_char_matrix(&self) -> bool {
        self.char_width > 0
    }

    /// Check if the sign is a full-matrix.
    fn is_full_matrix(&self) -> bool {
        self.char_width == 0 && self.char_height == 0
    }

    /// Get the character width (1 for variable width).
    fn char_width(&self) -> u16 {
        if self.is_char_matrix() {
            self.char_width.into()
        } else {
            1
        }
    }

    /// Get the character height (1 for variable height).
    fn char_height(&self) -> u16 {
        if self.char_height > 0 {
            self.char_height.into()
        } else {
            1
        }
    }

    /// Update the text rectangle.
    fn update_text_rectangle(
        &mut self,
        default_state: &State,
        rect: Rectangle,
        val: &Value,
    ) -> Result<()> {
        let rect = rect.match_width_height(default_state.text_rectangle);
        if !default_state.text_rectangle.contains(rect) {
            return Err(SyntaxError::UnsupportedTagValue(val.into()));
        }
        let cw = self.char_width();
        if cw > 0 {
            // Check text rectangle matches character boundaries
            let x = rect.x - 1;
            if x % cw != 0 || rect.w % cw != 0 {
                return Err(SyntaxError::UnsupportedTagValue(val.into()));
            }
        }
        let lh = self.char_height();
        if lh > 0 {
            // Check text rectangle matches line boundaries
            let y = rect.y - 1;
            if y % lh != 0 || rect.h % lh != 0 {
                return Err(SyntaxError::UnsupportedTagValue(val.into()));
            }
        }
        self.text_rectangle = rect;
        Ok(())
    }

    /// Get the background RGB color.
    fn background_rgb(&self) -> SRgb8 {
        let (r, g, b) = self.color_ctx.background_rgb();
        SRgb8::new(r, g, b)
    }

    /// Get the foreground RGB color.
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
    fn font<'a>(&self, fonts: &'a FontCache) -> Result<&'a Font> {
        debug!("State::font {}", self.font_num);
        fonts.lookup(self.font_num, self.font_version_id)
    }
}

impl<'a> TextSpan {
    /// Create a new text span
    fn new(state: &State, text: String) -> Self {
        let state = state.clone();
        TextSpan { state, text }
    }

    /// Get the width of a text span
    fn width(&self, fonts: &FontCache) -> Result<u16> {
        let font = self.state.font(fonts)?;
        let cs = self.char_spacing_fonts(fonts)?;
        Ok(font.text_width(&self.text, Some(cs))?)
    }

    /// Get the char spacing
    fn char_spacing_fonts(&self, fonts: &FontCache) -> Result<u16> {
        match self.state.char_spacing {
            Some(sp) => Ok(sp.into()),
            None => Ok(self.state.font(fonts)?.char_spacing().into()),
        }
    }

    /// Get the char spacing
    fn char_spacing_font(&self, font: &Font) -> u8 {
        match self.state.char_spacing {
            Some(sp) => sp,
            None => font.char_spacing(),
        }
    }

    /// Get the char spacing from a previous span
    fn char_spacing_between(
        &self,
        prev: &TextSpan,
        fonts: &FontCache,
    ) -> Result<u16> {
        if let Some(c) = self.state.char_spacing {
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
    fn height(&self, fonts: &FontCache) -> Result<u16> {
        Ok(self.state.font(fonts)?.height().into())
    }

    /// Get the font line spacing
    fn font_spacing(&self, fonts: &FontCache) -> Result<u16> {
        Ok(self.state.font(fonts)?.line_spacing().into())
    }

    /// Get the line spacing
    fn line_spacing(&self) -> Option<u16> {
        match self.state.line_spacing {
            Some(sp) => Some(sp.into()),
            None => None,
        }
    }

    /// Render the text span
    fn render_text(
        &self,
        raster: &mut Raster<SRgb8>,
        font: &Font,
        x: i32,
        y: i32,
    ) -> Result<()> {
        let cs = self.char_spacing_font(font).into();
        let cf = self.state.foreground_rgb();
        Ok(font.render_text(raster, &self.text, x, y, cs, cf)?)
    }
}

impl TextLine {
    /// Create a new text line
    fn new(height: u16, font_spacing: u16, line_spacing: Option<u16>) -> Self {
        TextLine {
            height,
            font_spacing,
            line_spacing,
        }
    }

    /// Combine a text line with another
    fn combine(&mut self, rhs: &Self) {
        self.height = self.height.max(rhs.height);
        self.font_spacing = self.font_spacing.max(rhs.font_spacing);
        self.line_spacing = self.line_spacing.or(rhs.line_spacing);
    }

    /// Get the spacing between two text lines
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

impl<'a> Renderer<'a> {
    /// Create a new renderer.
    ///
    /// * `default_state` Default render state.
    /// * `ms` MULTI string to parse.
    pub fn new(
        fonts: &'a FontCache,
        graphics: &'a GraphicCache,
        default_state: State,
        ms: &'a str,
    ) -> Self {
        let state = default_state.clone();
        let parser = Parser::new(ms);
        let spans = vec![];
        Renderer {
            fonts,
            graphics,
            default_state,
            state,
            page_state: PageState::On(true),
            parser,
            spans,
        }
    }

    /// Get the page-on time (deciseconds)
    fn page_on_time_ds(&self) -> u16 {
        self.state.page_on_time_ds.into()
    }

    /// Get the page-off time (deciseconds)
    fn page_off_time_ds(&self) -> u16 {
        self.state.page_off_time_ds.into()
    }

    /// Render an OFF page.
    fn render_off_page(&mut self) -> (Raster<SRgb8>, u16) {
        self.page_state = self.page_state.next_state();
        (self.build_raster(), self.page_off_time_ds())
    }

    /// Build a raster
    fn build_raster(&self) -> Raster<SRgb8> {
        let width = self.state.text_rectangle.w.into();
        let height = self.state.text_rectangle.h.into();
        let clr = self.state.background_rgb();
        Raster::with_color(width, height, clr)
    }

    /// Render an ON page.
    fn render_on_page(&mut self) -> Result<(Raster<SRgb8>, u16)> {
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
        Ok((raster, self.page_on_time_ds()))
    }

    /// Check for unsupported MULTI tags
    fn check_unsupported(&self) -> Result<()> {
        let mut parser = self.parser.clone();
        while let Some(value) = parser.next() {
            let val = value?;
            match val {
                Value::Field(_, _)
                | Value::Flash(_, _, _)
                | Value::FlashEnd()
                | Value::ManufacturerSpecific(_, _)
                | Value::ManufacturerSpecificEnd(_, _)
                | Value::MovingText(_, _, _, _, _, _) => {
                    return Err(SyntaxError::UnsupportedTag(val.into()));
                }
                Value::NewPage() => break,
                _ => (),
            }
        }
        Ok(())
    }

    /// Iterate through all values in a page to update its state
    fn update_page_state(&mut self) -> Result<()> {
        let ds = &self.default_state;
        let rs = &mut self.state;
        // Set these back to default values
        rs.text_rectangle = ds.text_rectangle;
        rs.line_spacing = ds.line_spacing;
        rs.line_number = 0;
        rs.span_number = 0;
        let mut parser = self.parser.clone();
        while let Some(value) = parser.next() {
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
        let mut rs = self.state.clone();
        let mut parser = self.parser.clone();
        while let Some(value) = parser.next() {
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
                Value::Graphic(gn, None) => {
                    let g = self.graphics.lookup(gn, None)?;
                    g.render_graphic(raster, 1, 1, &rs.color_ctx)?;
                }
                Value::Graphic(gn, Some((x, y, gid))) => {
                    let g = self.graphics.lookup(gn, gid)?;
                    let x = x.into();
                    let y = y.into();
                    g.render_graphic(raster, x, y, &rs.color_ctx)?;
                }
                Value::NewPage() | Value::TextRectangle(_) => break,
                _ => (),
            }
        }
        Ok(())
    }

    /// Render one text rectangle
    fn render_text_rectangle(
        &mut self,
        raster: &mut Raster<SRgb8>,
    ) -> Result<()> {
        let page_off = self.page_off_time_ds() > 0;
        let ds = &self.default_state;
        let mut rs = &mut self.state;
        let mut line_blank = true;
        self.page_state = PageState::done(page_off);
        self.spans.clear();
        while let Some(value) = self.parser.next() {
            let val = value?;
            match val {
                Value::ColorForeground(clr) => {
                    rs.color_ctx.set_foreground(clr, &val)?;
                }
                Value::Font(f) => {
                    rs.font_num = f.map_or(ds.font_num, |t| t.0);
                    rs.font_version_id = f.map_or(ds.font_version_id, |t| t.1);
                }
                Value::JustificationLine(Some(JustificationLine::Other)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationLine(Some(JustificationLine::Full)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationLine(jl) => {
                    rs.just_line = jl.unwrap_or(ds.just_line);
                    rs.span_number = 0;
                }
                Value::JustificationPage(Some(JustificationPage::Other)) => {
                    return Err(SyntaxError::UnsupportedTagValue(val.into()));
                }
                Value::JustificationPage(jp) => {
                    rs.just_page = jp.unwrap_or(ds.just_page);
                    rs.line_number = 0;
                    rs.span_number = 0;
                }
                Value::NewLine(ls) => {
                    if !rs.is_full_matrix() && ls.is_some() {
                        return Err(SyntaxError::UnsupportedTagValue(
                            val.into(),
                        ));
                    }
                    // Insert an empty text span for blank lines.
                    if line_blank {
                        self.spans.push(TextSpan::new(&rs, "".into()));
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
                    if rs.is_char_matrix() && sc > 0 {
                        return Err(SyntaxError::UnsupportedTagValue(
                            val.into(),
                        ));
                    }
                    rs.char_spacing = Some(sc);
                }
                Value::SpacingCharacterEnd() => {
                    rs.char_spacing = None;
                }
                Value::TextRectangle(rect) => {
                    self.page_state = PageState::On(false);
                    rs.line_number = 0;
                    rs.span_number = 0;
                    rs.update_text_rectangle(ds, rect, &val)?;
                    break;
                }
                Value::Text(t) => {
                    self.spans.push(TextSpan::new(&rs, t));
                    rs.span_number += 1;
                    line_blank = false;
                }
                Value::HexadecimalCharacter(hc) => {
                    match std::char::from_u32(hc.into()) {
                        Some(c) => {
                            let mut t = String::new();
                            t.push(c);
                            self.spans.push(TextSpan::new(&rs, t));
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

    /// Render spans for current text rectangle
    fn render_text_spans(&self, raster: &mut Raster<SRgb8>) -> Result<()> {
        self.check_justification()?;
        for span in &self.spans {
            let x = self.span_x(span)?.into();
            let y = self.span_y(span)?.into();
            let font = span.state.font(self.fonts)?;
            span.render_text(raster, &font, x, y)?;
        }
        Ok(())
    }

    /// Check page and line justification ordering
    fn check_justification(&self) -> Result<()> {
        let mut jp = JustificationPage::Other;
        let mut jl = JustificationLine::Other;
        let mut ln = 0;
        for span in &self.spans {
            let just_page = span.state.just_page;
            let just_line = span.state.just_line;
            let line_number = span.state.line_number;
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

    /// Get the X position of a text span.
    fn span_x(&self, span: &TextSpan) -> Result<u16> {
        match span.state.just_line {
            JustificationLine::Left => self.span_x_left(span),
            JustificationLine::Center => self.span_x_center(span),
            JustificationLine::Right => self.span_x_right(span),
            _ => unreachable!(),
        }
    }

    /// Get the X position of a left-justified text span.
    fn span_x_left(&self, span: &TextSpan) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let (before, _) = self.offset_horiz(span)?;
        Ok(left + before)
    }

    /// Get the X position of a center-justified text span.
    fn span_x_center(&self, span: &TextSpan) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let w = span.state.text_rectangle.w;
        let (before, after) = self.offset_horiz(span)?;
        let offset = (w - before - after) / 2; // offset for centering
        let x = left + offset + before;
        let cw = self.default_state.char_width();
        // Truncate to character-width boundary
        Ok((x / cw) * cw)
    }

    /// Get the X position of a right-justified span
    fn span_x_right(&self, span: &TextSpan) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let w = span.state.text_rectangle.w;
        let (_, after) = self.offset_horiz(span)?;
        Ok(left + w - after)
    }

    /// Calculate horizontal offsets of a span.
    ///
    /// Returns a tuple of (before, after) widths of matching spans.
    fn offset_horiz(&self, text_span: &TextSpan) -> Result<(u16, u16)> {
        debug!("offset_horiz '{}'", text_span.text);
        let rs = &text_span.state;
        let mut before = 0;
        let mut after = 0;
        let mut pspan = None;
        for span in self.spans.iter().filter(|s| rs.matches_span(&s.state)) {
            if let Some(ps) = pspan {
                let w = span.char_spacing_between(ps, self.fonts)?;
                if span.state.span_number <= rs.span_number {
                    before += w
                } else {
                    after += w
                }
                debug!("  spacing {} before {} after {}", w, before, after);
            }
            let w = span.width(self.fonts)?;
            if span.state.span_number < rs.span_number {
                before += w
            } else {
                after += w
            }
            debug!("  span '{}'  before {} after {}", span.text, before, after);
            pspan = Some(span);
        }
        if before + after <= rs.text_rectangle.w {
            Ok((before, after))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }

    /// Get the Y position of a text span.
    fn span_y(&self, span: &TextSpan) -> Result<u16> {
        let b = self.baseline(span)?;
        let h = span.height(self.fonts)?;
        debug_assert!(b >= h);
        Ok(b - h)
    }

    /// Get the baseline of a text span.
    fn baseline(&self, span: &TextSpan) -> Result<u16> {
        match span.state.just_page {
            JustificationPage::Top => self.baseline_top(span),
            JustificationPage::Middle => self.baseline_middle(span),
            JustificationPage::Bottom => self.baseline_bottom(span),
            _ => unreachable!(),
        }
    }

    /// Get the baseline of a top-justified span
    fn baseline_top(&self, span: &TextSpan) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let (above, _) = self.offset_vert(span)?;
        Ok(top + above)
    }

    /// Get the baseline of a middle-justified span
    fn baseline_middle(&self, span: &TextSpan) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let h = span.state.text_rectangle.h;
        let (above, below) = self.offset_vert(span)?;
        let offset = (h - above - below) / 2; // offset for centering
        let y = top + offset + above;
        let ch = self.default_state.char_height();
        // Truncate to line-height boundary
        Ok((y / ch) * ch)
    }

    /// Get the baseline of a bottom-justified span
    fn baseline_bottom(&self, span: &TextSpan) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let h = span.state.text_rectangle.h;
        let (_, below) = self.offset_vert(span)?;
        Ok(top + h - below)
    }

    /// Calculate vertical offset of a span.
    ///
    /// Returns a tuple of (above, below) heights of matching lines.
    fn offset_vert(&self, text_span: &TextSpan) -> Result<(u16, u16)> {
        debug!("offset_vert '{}'", text_span.text);
        let rs = &text_span.state;
        let mut lines = vec![];
        for span in self.spans.iter().filter(|s| rs.matches_line(&s.state)) {
            let ln = usize::from(span.state.line_number);
            let h = span.height(self.fonts)?;
            let fs = span.font_spacing(self.fonts)?;
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
        if above + below <= rs.text_rectangle.h {
            Ok((above, below))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }
}

impl<'a> Iterator for Renderer<'a> {
    type Item = Result<(Raster<SRgb8>, u16)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.page_state {
            PageState::On(_) => Some(self.render_on_page()),
            PageState::Off(_) => Some(Ok(self.render_off_page())),
            _ => None,
        }
    }
}

/// Render a color rectangle
fn render_rect(
    raster: &mut Raster<SRgb8>,
    rect: Rectangle,
    clr: SRgb8,
    value: &Value,
) -> Result<()> {
    debug_assert!(rect.x > 0);
    debug_assert!(rect.y > 0);
    let rx = i32::from(rect.x) - 1;
    let ry = i32::from(rect.y) - 1;
    let rw = u32::from(rect.w);
    let rh = u32::from(rect.h);
    let region = Region::new(rx, ry, rw, rh);
    if raster.intersection(region) == region {
        raster.copy_color(region, clr);
        return Ok(());
    } else {
        Err(SyntaxError::UnsupportedTagValue(value.into()))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::multi::{ColorClassic, ColorCtx, ColorScheme};

    fn font_cache() -> FontCache {
        let mut fonts = FontCache::default();
        let fts: Vec<Font> =
            serde_json::from_str(include_str!("../../test/font.json")).unwrap();
        for font in fts {
            fonts.insert(font);
        }
        fonts
    }

    fn render_full(multi: &str) -> Result<Vec<(Raster<SRgb8>, u16)>> {
        let rs = State::new(
            ColorCtx::new(
                ColorScheme::Color24Bit,
                ColorClassic::White.rgb(),
                ColorClassic::Black.rgb(),
            ),
            0,
            0,
            20,
            0,
            Rectangle::new(1, 1, 60, 30),
            JustificationPage::Top,
            JustificationLine::Left,
            3,
            None,
        );
        let fonts = font_cache();
        let graphics = GraphicCache::default();
        Renderer::new(&fonts, &graphics, rs, multi).collect()
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
            "[fo3][jl2][cf255,255,255]RAMP A[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP B[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP C[jl4][cf255,255,0]FULL",
        )
        .unwrap();
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn page_times() {
        assert_eq!(render_full("").unwrap()[0].1, 20);
        assert_eq!(render_full("[pt25o10]").unwrap()[0].1, 25);
        assert_eq!(render_full("[pt20o10]").unwrap()[1].1, 10);
        assert_eq!(render_full("[pt30o5][np]").unwrap()[2].1, 30);
        assert_eq!(render_full("[pto15][np]").unwrap()[3].1, 15);
    }

    fn justify_dot(multi: &str, i: usize) {
        let mut raster = Raster::<SRgb8>::with_clear(60, 30);
        raster.pixels_mut()[i] = SRgb8::new(255, 255, 255);
        let pages = render_full(multi).unwrap();
        assert_eq!(pages.len(), 1);
        let page = &pages[0].0;
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

    fn render_char(multi: &str) -> Result<Vec<(Raster<SRgb8>, u16)>> {
        let rs = State::new(
            ColorCtx::new(
                ColorScheme::Monochrome1Bit,
                ColorClassic::White.rgb(),
                ColorClassic::Black.rgb(),
            ),
            5,
            7,
            20,
            0,
            Rectangle::new(1, 1, 100, 21),
            JustificationPage::Top,
            JustificationLine::Left,
            1,
            None,
        );
        let fonts = font_cache();
        let graphics = GraphicCache::default();
        Renderer::new(&fonts, &graphics, rs, multi).collect()
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
