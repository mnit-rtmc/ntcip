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
use pix::{rgb::SRgb8, Raster};
use std::convert::TryFrom;

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

/// Page renderer
#[derive(Clone)]
pub struct PageRenderer {
    /// Base page render state
    base_state: State,
    /// graphic / color rect, color context
    values: Vec<(Value, ColorCtx)>,
    /// text spans
    spans: Vec<TextSpan>,
    /// Flag indicating current line blank
    line_blank: bool,
}

/// Page splitter (iterator)
pub struct PageSplitter<'a> {
    /// Default rendering state
    default_state: State,
    /// Current state
    state: State,
    /// MULTI parser
    parser: Parser<'a>,
    /// Flag to indicate more pages
    more_pages: bool,
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
        r: Rectangle,
        v: &Value,
    ) -> Result<()> {
        let r = r.match_width_height(default_state.text_rectangle);
        if !default_state.text_rectangle.contains(r) {
            return Err(SyntaxError::UnsupportedTagValue(v.into()));
        }
        let cw = self.char_width();
        if cw > 0 {
            // Check text rectangle matches character boundaries
            let x = r.x - 1;
            if x % cw != 0 || r.w % cw != 0 {
                return Err(SyntaxError::UnsupportedTagValue(v.into()));
            }
        }
        let lh = self.char_height();
        if lh > 0 {
            // Check text rectangle matches line boundaries
            let y = r.y - 1;
            if y % lh != 0 || r.h % lh != 0 {
                return Err(SyntaxError::UnsupportedTagValue(v.into()));
            }
        }
        self.text_rectangle = r;
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
    fn matches_span(&self, other: &State) -> bool {
        self.text_rectangle == other.text_rectangle
            && self.just_page == other.just_page
            && self.line_number == other.line_number
            && self.just_line == other.just_line
    }

    /// Check if states match for lines
    fn matches_line(&self, other: &State) -> bool {
        self.text_rectangle == other.text_rectangle
            && self.just_page == other.just_page
    }

    /// Lookup current font in cache
    fn font<'a>(&self, fonts: &'a FontCache) -> Result<&'a Font> {
        debug!("State::font {}", self.font_num);
        fonts.lookup(self.font_num, self.font_version_id)
    }
}

impl<'a> TextSpan {
    /// Create a new text span
    fn new(state: State, text: String) -> Self {
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
            Some(s) => Ok(s.into()),
            None => Ok(self.state.font(fonts)?.char_spacing().into()),
        }
    }

    /// Get the char spacing
    fn char_spacing_font(&self, font: &Font) -> u8 {
        match self.state.char_spacing {
            Some(s) => s,
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
            Some(s) => Some(s.into()),
            None => None,
        }
    }

    /// Render the text span
    fn render_text(
        &self,
        page: &mut Raster<SRgb8>,
        font: &Font,
        x: i32,
        y: i32,
    ) -> Result<()> {
        let cs = self.char_spacing_font(font).into();
        let cf = self.state.foreground_rgb();
        Ok(font.render_text(page, &self.text, x, y, cs, cf)?)
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
    fn combine(&mut self, other: &TextLine) {
        self.height = self.height.max(other.height);
        self.font_spacing = self.font_spacing.max(other.font_spacing);
        self.line_spacing = self.line_spacing.or(other.line_spacing);
    }

    /// Get the spacing between two text lines
    fn spacing(&self, other: &TextLine) -> u16 {
        if let Some(ls) = self.line_spacing {
            ls
        } else {
            // NTCIP 1203 fontLineSpacing:
            // "The number of pixels between adjacent lines
            // is the average of the 2 line spacings of each
            // line, rounded up to the nearest whole pixel."
            let ps = other.font_spacing;
            let s = self.font_spacing;
            ((ps + s) >> 1) + ((ps + s) & 1)
        }
    }
}

impl PageRenderer {
    /// Create a new page renderer
    fn new(base_state: State) -> Self {
        let values = vec![];
        let spans = vec![];
        PageRenderer {
            base_state,
            values,
            spans,
            line_blank: true,
        }
    }

    /// Check page and line justification ordering
    fn check_justification(&self) -> Result<()> {
        let mut tr = Rectangle::new(0, 0, 0, 0);
        let mut jp = JustificationPage::Other;
        let mut jl = JustificationLine::Other;
        let mut ln = 0;
        for s in &self.spans {
            let text_rectangle = s.state.text_rectangle;
            let just_page = s.state.just_page;
            let just_line = s.state.just_line;
            let line_number = s.state.line_number;
            if text_rectangle == tr
                && (just_page < jp
                    || (just_page == jp && line_number == ln && just_line < jl))
            {
                return Err(SyntaxError::TagConflict);
            }
            tr = text_rectangle;
            jp = just_page;
            jl = just_line;
            ln = line_number;
        }
        Ok(())
    }

    /// Get the page-on time (deciseconds)
    pub fn page_on_time_ds(&self) -> u16 {
        self.base_state.page_on_time_ds.into()
    }

    /// Get the page-off time (deciseconds)
    pub fn page_off_time_ds(&self) -> u16 {
        self.base_state.page_off_time_ds.into()
    }

    /// Render a blank page.
    pub fn render_blank(&self) -> Raster<SRgb8> {
        let rs = &self.base_state;
        let w = rs.text_rectangle.w;
        let h = rs.text_rectangle.h;
        let clr = rs.background_rgb();
        Raster::with_color(w.into(), h.into(), clr)
    }

    /// Render the page.
    pub fn render_page(
        &self,
        fonts: &FontCache,
        graphics: &GraphicCache,
    ) -> Result<Raster<SRgb8>> {
        let rs = &self.base_state;
        let width = rs.text_rectangle.w.into();
        let height = rs.text_rectangle.h.into();
        debug!("render_page: {}x{}", width, height);
        let clr = rs.background_rgb();
        let mut page = Raster::with_color(width, height, clr);
        for (v, ctx) in &self.values {
            match v {
                Value::ColorRectangle(rect, _) => {
                    let (r, g, b) = ctx.foreground_rgb();
                    let clr = SRgb8::new(r, g, b);
                    self.render_rect(&mut page, *rect, clr, v)?;
                }
                Value::Graphic(gn, None) => {
                    let g = graphics.lookup(*gn, None)?;
                    g.render_graphic(&mut page, 1, 1, ctx)?;
                }
                Value::Graphic(gn, Some((x, y, gid))) => {
                    let g = graphics.lookup(*gn, *gid)?;
                    let x = (*x).into();
                    let y = (*y).into();
                    g.render_graphic(&mut page, x, y, ctx)?;
                }
                _ => unreachable!(),
            }
        }
        for s in &self.spans {
            let x = self.span_x(s, fonts)?.into();
            let y = self.span_y(s, fonts)?.into();
            let font = s.state.font(fonts)?;
            s.render_text(&mut page, &font, x, y)?;
        }
        Ok(page)
    }

    /// Render a color rectangle
    fn render_rect(
        &self,
        page: &mut Raster<SRgb8>,
        r: Rectangle,
        clr: SRgb8,
        v: &Value,
    ) -> Result<()> {
        debug_assert!(r.x > 0);
        debug_assert!(r.y > 0);
        let rx = i32::from(r.x) - 1;
        let ry = i32::from(r.y) - 1;
        let rw = i32::from(r.w);
        let rh = i32::from(r.h);
        let width = i32::try_from(page.width()).unwrap();
        let height = i32::try_from(page.height()).unwrap();
        if rx + rw <= width && ry + rh <= height {
            for y in 0..rh {
                for x in 0..rw {
                    *page.pixel_mut(rx + x, ry + y) = clr;
                }
            }
            return Ok(());
        }
        Err(SyntaxError::UnsupportedTagValue(v.into()))
    }

    /// Get the X position of a text span.
    fn span_x(&self, s: &TextSpan, fonts: &FontCache) -> Result<u16> {
        match s.state.just_line {
            JustificationLine::Left => self.span_x_left(s, fonts),
            JustificationLine::Center => self.span_x_center(s, fonts),
            JustificationLine::Right => self.span_x_right(s, fonts),
            _ => unreachable!(),
        }
    }

    /// Get the X position of a left-justified text span.
    fn span_x_left(&self, span: &TextSpan, fonts: &FontCache) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let (before, _) = self.offset_horiz(span, fonts)?;
        Ok(left + before)
    }

    /// Get the X position of a center-justified text span.
    fn span_x_center(&self, span: &TextSpan, fonts: &FontCache) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let w = span.state.text_rectangle.w;
        let (before, after) = self.offset_horiz(span, fonts)?;
        let offset = (w - before - after) / 2; // offset for centering
        let x = left + offset + before;
        let cw = self.base_state.char_width();
        // Truncate to character-width boundary
        Ok((x / cw) * cw)
    }

    /// Get the X position of a right-justified span
    fn span_x_right(&self, span: &TextSpan, fonts: &FontCache) -> Result<u16> {
        let left = span.state.text_rectangle.x - 1;
        let w = span.state.text_rectangle.w;
        let (_, after) = self.offset_horiz(span, fonts)?;
        Ok(left + w - after)
    }

    /// Calculate horizontal offsets of a span.
    ///
    /// Returns a tuple of (before, after) widths of matching spans.
    fn offset_horiz(
        &self,
        span: &TextSpan,
        fonts: &FontCache,
    ) -> Result<(u16, u16)> {
        debug!("offset_horiz '{}'", span.text);
        let rs = &span.state;
        let mut before = 0;
        let mut after = 0;
        let mut pspan = None;
        for s in self.spans.iter().filter(|s| rs.matches_span(&s.state)) {
            if let Some(ps) = pspan {
                let w = s.char_spacing_between(ps, fonts)?;
                if s.state.span_number <= rs.span_number {
                    before += w
                } else {
                    after += w
                }
                debug!("  spacing {} before {} after {}", w, before, after);
            }
            let w = s.width(fonts)?;
            if s.state.span_number < rs.span_number {
                before += w
            } else {
                after += w
            }
            debug!("  span '{}'  before {} after {}", s.text, before, after);
            pspan = Some(s);
        }
        if before + after <= rs.text_rectangle.w {
            Ok((before, after))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }

    /// Get the Y position of a text span.
    fn span_y(&self, s: &TextSpan, fonts: &FontCache) -> Result<u16> {
        let b = self.baseline(s, fonts)?;
        let h = s.height(fonts)?;
        debug_assert!(b >= h);
        Ok(b - h)
    }

    /// Get the baseline of a text span.
    fn baseline(&self, s: &TextSpan, fonts: &FontCache) -> Result<u16> {
        match s.state.just_page {
            JustificationPage::Top => self.baseline_top(s, fonts),
            JustificationPage::Middle => self.baseline_middle(s, fonts),
            JustificationPage::Bottom => self.baseline_bottom(s, fonts),
            _ => unreachable!(),
        }
    }

    /// Get the baseline of a top-justified span
    fn baseline_top(&self, span: &TextSpan, fonts: &FontCache) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let (above, _) = self.offset_vert(span, fonts)?;
        Ok(top + above)
    }

    /// Get the baseline of a middle-justified span
    fn baseline_middle(
        &self,
        span: &TextSpan,
        fonts: &FontCache,
    ) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let h = span.state.text_rectangle.h;
        let (above, below) = self.offset_vert(span, fonts)?;
        let offset = (h - above - below) / 2; // offset for centering
        let y = top + offset + above;
        let ch = self.base_state.char_height();
        // Truncate to line-height boundary
        Ok((y / ch) * ch)
    }

    /// Get the baseline of a bottom-justified span
    fn baseline_bottom(
        &self,
        span: &TextSpan,
        fonts: &FontCache,
    ) -> Result<u16> {
        let top = span.state.text_rectangle.y - 1;
        let h = span.state.text_rectangle.h;
        let (_, below) = self.offset_vert(span, fonts)?;
        Ok(top + h - below)
    }

    /// Calculate vertical offset of a span.
    ///
    /// Returns a tuple of (above, below) heights of matching lines.
    fn offset_vert(
        &self,
        span: &TextSpan,
        fonts: &FontCache,
    ) -> Result<(u16, u16)> {
        debug!("offset_vert '{}'", span.text);
        let rs = &span.state;
        let mut lines = vec![];
        for s in self.spans.iter().filter(|s| rs.matches_line(&s.state)) {
            let ln = usize::from(s.state.line_number);
            let h = s.height(fonts)?;
            let fs = s.font_spacing(fonts)?;
            let ls = s.line_spacing();
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
        if above + below <= span.state.text_rectangle.h {
            Ok((above, below))
        } else {
            Err(SyntaxError::TextTooBig)
        }
    }
}

impl<'a> PageSplitter<'a> {
    /// Create a new page splitter.
    ///
    /// * `default_state` Default render state.
    /// * `ms` MULTI string to parse.
    pub fn new(default_state: State, ms: &'a str) -> Self {
        let parser = Parser::new(ms);
        let state = default_state.clone();
        let more_pages = true;
        PageSplitter {
            default_state,
            state,
            parser,
            more_pages,
        }
    }

    /// Make the next page.
    fn make_page(&mut self) -> Result<PageRenderer> {
        self.more_pages = false;
        let mut page = PageRenderer::new(self.page_state());
        while let Some(v) = self.parser.next() {
            self.update_state(v?, &mut page)?;
            if self.more_pages {
                break;
            }
        }
        page.check_justification()?;
        Ok(page)
    }

    /// Get the current page state.
    fn page_state(&self) -> State {
        let mut rs = self.state.clone();
        // Set these back to default values
        rs.text_rectangle = self.default_state.text_rectangle;
        rs.line_spacing = self.default_state.line_spacing;
        rs
    }

    /// Update the render state with one MULTI value.
    ///
    /// * `v` MULTI value.
    /// * `page` Page renderer.
    fn update_state(
        &mut self,
        v: Value,
        page: &mut PageRenderer,
    ) -> Result<()> {
        let ds = &self.default_state;
        let mut rs = &mut self.state;
        match v {
            Value::ColorBackground(c) => {
                // This tag remains for backward compatibility with 1203v1
                rs.color_ctx.set_background(c, &v)?;
                page.base_state.color_ctx.set_background(c, &v)?;
            }
            Value::ColorForeground(c) => {
                rs.color_ctx.set_foreground(c, &v)?;
            }
            Value::ColorRectangle(_, c) => {
                let mut ctx = rs.color_ctx.clone();
                // only set foreground color in cloned context
                ctx.set_foreground(Some(c), &v)?;
                page.values.push((v, ctx));
            }
            Value::Font(None) => {
                rs.font_num = ds.font_num;
                rs.font_version_id = ds.font_version_id;
            }
            Value::Font(Some(f)) => {
                rs.font_num = f.0;
                rs.font_version_id = f.1;
            }
            Value::Graphic(_, _) => {
                page.values.push((v, rs.color_ctx.clone()));
            }
            Value::JustificationLine(Some(JustificationLine::Other)) => {
                return Err(SyntaxError::UnsupportedTagValue(v.into()));
            }
            Value::JustificationLine(Some(JustificationLine::Full)) => {
                return Err(SyntaxError::UnsupportedTagValue(v.into()));
            }
            Value::JustificationLine(jl) => {
                rs.just_line = jl.unwrap_or(ds.just_line);
                rs.span_number = 0;
            }
            Value::JustificationPage(Some(JustificationPage::Other)) => {
                return Err(SyntaxError::UnsupportedTagValue(v.into()));
            }
            Value::JustificationPage(jp) => {
                rs.just_page = jp.unwrap_or(ds.just_page);
                rs.line_number = 0;
                rs.span_number = 0;
            }
            Value::NewLine(ls) => {
                if !rs.is_full_matrix() && ls.is_some() {
                    return Err(SyntaxError::UnsupportedTagValue(v.into()));
                }
                // Insert an empty text span for blank lines.
                if page.line_blank {
                    page.spans.push(TextSpan::new(rs.clone(), "".into()));
                }
                page.line_blank = true;
                rs.line_spacing = ls;
                rs.line_number += 1;
                rs.span_number = 0;
            }
            Value::NewPage() => {
                rs.line_number = 0;
                rs.span_number = 0;
                self.more_pages = true;
            }
            Value::PageBackground(c) => {
                rs.color_ctx.set_background(c, &v)?;
                page.base_state.color_ctx.set_background(c, &v)?;
            }
            Value::PageTime(on, off) => {
                rs.page_on_time_ds = on.unwrap_or(ds.page_on_time_ds);
                rs.page_off_time_ds = off.unwrap_or(ds.page_off_time_ds);
                page.base_state.page_on_time_ds =
                    on.unwrap_or(ds.page_on_time_ds);
                page.base_state.page_off_time_ds =
                    off.unwrap_or(ds.page_off_time_ds);
            }
            Value::SpacingCharacter(sc) => {
                if rs.is_char_matrix() && sc > 0 {
                    return Err(SyntaxError::UnsupportedTagValue(v.into()));
                }
                rs.char_spacing = Some(sc);
            }
            Value::SpacingCharacterEnd() => {
                rs.char_spacing = None;
            }
            Value::TextRectangle(r) => {
                page.line_blank = true;
                rs.line_number = 0;
                rs.span_number = 0;
                rs.update_text_rectangle(ds, r, &v)?;
            }
            Value::Text(t) => {
                page.spans.push(TextSpan::new(rs.clone(), t));
                rs.span_number += 1;
                page.line_blank = false;
            }
            Value::HexadecimalCharacter(hc) => {
                match std::char::from_u32(hc.into()) {
                    Some(c) => {
                        let mut t = String::new();
                        t.push(c);
                        page.spans.push(TextSpan::new(rs.clone(), t));
                        rs.span_number += 1;
                        page.line_blank = false;
                    }
                    None => {
                        // Invalid code point (surrogate in D800-DFFF range)
                        return Err(SyntaxError::UnsupportedTagValue(v.into()));
                    }
                }
            }
            _ => {
                // Unsupported tags: [f], [fl], [ms], [mv]
                return Err(SyntaxError::UnsupportedTag(v.into()));
            }
        }
        Ok(())
    }
}

impl<'a> Iterator for PageSplitter<'a> {
    type Item = Result<PageRenderer>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.more_pages {
            Some(self.make_page())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::multi::{ColorClassic, ColorCtx, ColorScheme};

    fn make_full_matrix() -> State {
        State::new(
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
            1,
            None,
        )
    }

    #[test]
    fn page_count() {
        let rs = make_full_matrix();
        let pages: Vec<_> = PageSplitter::new(rs.clone(), "").collect();
        assert_eq!(pages.len(), 1);
        let pages: Vec<_> = PageSplitter::new(rs.clone(), "1").collect();
        assert_eq!(pages.len(), 1);
        let pages: Vec<_> = PageSplitter::new(rs.clone(), "[np]").collect();
        assert_eq!(pages.len(), 2);
        let pages: Vec<_> = PageSplitter::new(rs.clone(), "1[NP]").collect();
        assert_eq!(pages.len(), 2);
        let pages: Vec<_> = PageSplitter::new(rs.clone(), "1[Np]2").collect();
        assert_eq!(pages.len(), 2);
        let pages: Vec<_> =
            PageSplitter::new(rs.clone(), "1[np]2[nP]").collect();
        assert_eq!(pages.len(), 3);
        let pages: Vec<_> = PageSplitter::new(
            rs.clone(),
            "[fo6][nl]\
             [jl2][cf255,255,255]RAMP A[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP B[jl4][cf255,255,0]FULL[nl]\
             [jl2][cf255,255,255]RAMP C[jl4][cf255,255,0]FULL",
        )
        .collect();
        assert_eq!(pages.len(), 1);
    }

    #[test]
    fn line_count() {
        let rs = make_full_matrix();
        let mut pages = PageSplitter::new(rs.clone(), "[nl][nl]LINE 3[nl]");
        let p = pages.next().unwrap().unwrap();
        assert_eq!(
            p.spans
                .iter()
                .map(|s| s.text.clone())
                .collect::<Vec<String>>(),
            vec!["".to_string(), "".to_string(), "LINE 3".to_string()],
        );
    }

    #[test]
    fn page_full_matrix() {
        let rs = make_full_matrix();
        let mut pages = PageSplitter::new(rs.clone(), "");
        let p = pages.next().unwrap().unwrap();
        let rs = p.base_state;
        assert_eq!(rs.page_on_time_ds, 20);
        assert_eq!(rs.page_off_time_ds, 0);
        assert_eq!(rs.text_rectangle, Rectangle::new(1, 1, 60, 30));
        assert_eq!(rs.just_page, JustificationPage::Top);
        assert_eq!(rs.just_line, JustificationLine::Left);
        assert_eq!(rs.line_spacing, None);
        assert_eq!(rs.char_spacing, None);
        assert_eq!(rs.char_width, 0);
        assert_eq!(rs.char_height, 0);
        assert_eq!(rs.font_num, 1);
        assert_eq!(rs.font_version_id, None);
        let mut pages = PageSplitter::new(
            rs.clone(),
            "[pt10o2][cb9][pb5][cf3]\
             [jp3][jl4][tr1,1,10,10][nl4][fo3,1234][sc2][np][pb][pt][cb][/sc]",
        );
        let p = pages.next().unwrap().unwrap();
        let rs = p.base_state;
        assert_eq!(rs.page_on_time_ds, 10);
        assert_eq!(rs.page_off_time_ds, 2);
        assert_eq!(rs.text_rectangle, Rectangle::new(1, 1, 60, 30));
        assert_eq!(rs.just_page, JustificationPage::Top);
        assert_eq!(rs.just_line, JustificationLine::Left);
        assert_eq!(rs.line_spacing, None);
        assert_eq!(rs.char_spacing, None);
        assert_eq!(rs.font_num, 1);
        assert_eq!(rs.font_version_id, None);
        let p = pages.next().unwrap().unwrap();
        let rs = p.base_state;
        assert_eq!(rs.page_on_time_ds, 20);
        assert_eq!(rs.page_off_time_ds, 0);
        assert_eq!(rs.text_rectangle, Rectangle::new(1, 1, 60, 30));
        assert_eq!(rs.just_page, JustificationPage::Middle);
        assert_eq!(rs.just_line, JustificationLine::Right);
        assert_eq!(rs.line_spacing, None);
        assert_eq!(rs.char_spacing, Some(2));
        assert_eq!(rs.font_num, 3);
        assert_eq!(rs.font_version_id, Some(0x1234));
    }

    fn make_char_matrix() -> State {
        State::new(
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
        )
    }

    #[test]
    fn page_char_matrix() {
        let rs = make_char_matrix();
        let mut pages = PageSplitter::new(rs.clone(), "[tr1,1,12,12]");
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = pages.next() {
            assert!(true);
        } else {
            assert!(false)
        }
        let mut pages = PageSplitter::new(rs.clone(), "[tr1,1,50,12]");
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = pages.next() {
            assert!(true);
        } else {
            assert!(false)
        }
        let mut pages = PageSplitter::new(rs.clone(), "[tr1,1,12,14]");
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = pages.next() {
            assert!(true);
        } else {
            assert!(false)
        }
        let mut pages = PageSplitter::new(rs.clone(), "[tr1,1,50,14]");
        if let Some(Ok(_)) = pages.next() {
            assert!(true);
        } else {
            assert!(false)
        }
        let mut pages = PageSplitter::new(rs.clone(), "[pb9]");
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = pages.next() {
            assert!(true);
        } else {
            assert!(false)
        }
    }

    #[test]
    fn char_matrix_spacing() {
        let rs = make_char_matrix();
        let page = PageSplitter::new(rs.clone(), "[sc1][/sc]").next();
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = page {
            assert!(true)
        } else {
            assert!(false)
        }
        let page = PageSplitter::new(rs.clone(), "[sc0][/sc]").next();
        if let Some(Err(SyntaxError::UnsupportedTagValue(_))) = page {
            assert!(false)
        } else {
            assert!(true)
        }
    }
}
