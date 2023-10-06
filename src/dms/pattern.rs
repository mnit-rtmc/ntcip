// pattern.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
use crate::dms::multi::{MultiStr, Rectangle, SyntaxError, Value};
use crate::dms::sign::Dms;

/// Pattern values are MULTI values or "pseudo-values" from a pattern
#[derive(Clone, Debug, Eq, PartialEq)]
enum PatValue<'p> {
    /// MULTI value
    Value(Value<'p>),
    /// Fillable rectangle (w/font number)
    FillableRect(Rectangle, u8),
}

/// Pattern value iterator
struct PatIter<'p, const F: usize, const G: usize> {
    /// Sign
    dms: &'p Dms<F, G>,
    /// MULTI pattern
    pattern: MultiStr<'p>,
    /// Previous MULTI value
    value: Option<Value<'p>>,
    /// Current font number
    font_num: u8,
    /// Current rectangle + font number
    rect_font: Option<(Rectangle, u8)>,
    /// Have we reached the end?
    end: bool,
}

/// Message pattern
///
/// This is a message which can be composed with lines of text.  The *fillable*
/// parts are blank text rectangles or pages:
///
/// * `[tr…]` tag
/// * `[np]` tag
/// * Implicit page at start of message
///
/// To be fillable, a rectangle or page must not be followed by any text or
/// tags before the next `[tr…]` or `[np]` tag.  The only exception allowed
/// is the `[fo…]` tag, which applies to the **next** rectangle or page.
pub struct MessagePattern<'p, const F: usize, const G: usize> {
    /// Sign
    dms: &'p Dms<F, G>,
    /// MULTI string
    ms: &'p str,
}

impl<'p, const F: usize, const G: usize> PatIter<'p, F, G> {
    /// Create a new pattern iterator
    fn new(dms: &'p Dms<F, G>, ms: &'p str) -> Self {
        let rect = dms.full_rect();
        let font_num = dms.multi_cfg.default_font;
        PatIter {
            pattern: MultiStr::new(ms),
            value: None,
            dms,
            font_num,
            rect_font: Some((rect, font_num)),
            end: false,
        }
    }
}

impl<'p, const F: usize, const G: usize> MessagePattern<'p, F, G> {
    /// Create a new fillable pattern
    ///
    /// * `dms`: The sign
    /// * `ms`: MULTI string
    pub fn new(dms: &'p Dms<F, G>, ms: &'p str) -> Self {
        MessagePattern { dms, ms }
    }

    /// Find widths of fillable text lines
    ///
    /// Returns an iterator of tuples containing pixel width and font
    /// number for each fillable line in the pattern.
    pub fn widths(self) -> impl Iterator<Item = (u16, u8)> + 'p {
        let dms = self.dms;
        let mut lines = Vec::new();
        for (rect, font_num) in PatIter::new(self.dms, self.ms)
            .flatten()
            .filter_map(|v| match v {
                PatValue::FillableRect(r, f) => Some((r, f)),
                _ => None,
            })
        {
            for _ in 0..dms.rect_lines(rect, font_num) {
                lines.push((rect.width, font_num));
            }
        }
        lines.into_iter()
    }

    /// Fill lines into "fillable" parts of the pattern
    pub fn fill(self, mut lines: impl Iterator<Item = &'p str>) -> String {
        let mut font_val = None;
        let mut ms = String::new();
        for value in PatIter::new(self.dms, self.ms) {
            match value {
                Ok(PatValue::Value(Value::Font(fv))) => font_val = Some(fv),
                Ok(PatValue::Value(val)) => ms.push_str(&val.to_string()),
                Ok(PatValue::FillableRect(rect, font_num)) => {
                    for i in 0..self.dms.rect_lines(rect, font_num) {
                        match lines.next() {
                            Some(line) => {
                                if i > 0 {
                                    ms.push_str("[nl]");
                                }
                                ms.push_str(line);
                            }
                            None => break,
                        }
                    }
                    // defer font tag until lines are filled
                    if let Some(fv) = font_val {
                        ms.push_str(&Value::Font(fv).to_string());
                    }
                    font_val = None;
                }
                Err(_) => return String::new(),
            }
        }
        ms
    }

    /// Find text lines in a MULTI string matching pattern
    ///
    /// Returns an iterator of string slices matching each fillable line in
    /// the pattern.
    pub fn lines(self, mut ms: &str) -> impl Iterator<Item = &str> {
        let mut lines = Vec::new();
        for pval in PatIter::new(self.dms, self.ms).flatten() {
            match pval {
                PatValue::FillableRect(rect, font_num) => {
                    let mut n_lines = self.dms.rect_lines(rect, font_num);
                    let mut values = MultiStr::new(ms);
                    let (mut before, mut after) = values.split();
                    loop {
                        let value = values.next();
                        match value {
                            Some(Ok(
                                Value::Text(_)
                                | Value::ColorForeground(_)
                                | Value::JustificationLine(_)
                                | Value::SpacingCharacter(_)
                                | Value::SpacingCharacterEnd(),
                            )) => (before, after) = values.split(),
                            Some(Ok(Value::NewLine(_))) => {
                                if n_lines > 0 {
                                    lines.push(before);
                                    n_lines -= 1;
                                }
                                (_, ms) = values.split();
                                values = MultiStr::new(ms);
                                (before, after) = values.split();
                            }
                            _ => break,
                        }
                    }
                    // pad extra lines in rectangle
                    while n_lines > 0 {
                        lines.push(before);
                        n_lines -= 1;
                        before = "";
                    }
                    ms = after;
                }
                PatValue::Value(Value::Font(_)) => {
                    // ignore font tags in pattern
                    // since they are after fillable text
                }
                PatValue::Value(val) => {
                    let mut values = MultiStr::new(ms);
                    let mut value = values.next();
                    // ignore font tags in ms
                    while let Some(Ok(Value::Font(_))) = &value {
                        value = values.next();
                    }
                    match value {
                        Some(Ok(v)) if v == val => (),
                        _ => {
                            // abort!  ms does not match pattern
                            lines.clear();
                            break;
                        }
                    }
                    let (_before, after) = values.split();
                    ms = after;
                }
            }
        }
        lines.into_iter()
    }
}

impl<'p, const F: usize, const G: usize> Iterator for PatIter<'p, F, G> {
    type Item = Result<PatValue<'p>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }
        let value = self.value.take();
        if let Some(v) = value {
            return Some(Ok(PatValue::Value(v)));
        }
        let value = match self.pattern.next() {
            Some(Ok(v)) => v,
            Some(Err(e)) => return Some(Err(e)),
            None => {
                self.end = true;
                Value::NewPage()
            }
        };
        if let Some(tag) = value.tag() {
            if !self.dms.multi_cfg.supported_multi_tags.contains(tag) {
                return Some(Err(SyntaxError::UnsupportedTag(value.into())));
            }
        }
        match value {
            Value::Font(None) => {
                self.font_num = self.dms.multi_cfg.default_font;
            }
            Value::Font(Some((n, _))) => {
                self.font_num = n;
            }
            Value::Text(_) | Value::NewLine(_) => {
                self.rect_font = None;
            }
            Value::NewPage() => {
                let rect_font = self.rect_font;
                self.rect_font = Some((self.dms.full_rect(), self.font_num));
                if let Some((rect, font)) = rect_font {
                    self.value = Some(value);
                    return Some(Ok(PatValue::FillableRect(rect, font)));
                }
            }
            Value::TextRectangle(tr) => {
                let full_rect = self.dms.full_rect();
                let rect_font = self.rect_font;
                self.rect_font =
                    Some((tr.extend_width_height(full_rect), self.font_num));
                if let Some((rect, font)) = rect_font {
                    if rect != full_rect {
                        self.value = Some(value);
                        return Some(Ok(PatValue::FillableRect(rect, font)));
                    }
                }
            }
            _ => (),
        }
        (!self.end).then_some(Ok(PatValue::Value(value)))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::config::*;
    use crate::dms::font::*;

    fn font_table() -> FontTable<2> {
        let mut fonts = FontTable::default();
        let buf = include_bytes!("../../test/F07.ifnt");
        let f = fonts.lookup_mut(0).unwrap();
        *f = ifnt::read(&buf[..]).unwrap();
        let buf = include_bytes!("../../test/F08.ifnt");
        let f = fonts.lookup_mut(0).unwrap();
        *f = ifnt::read(&buf[..]).unwrap();
        fonts
    }

    fn make_dms() -> Dms<2, 0> {
        Dms::builder()
            .with_vms_cfg(VmsCfg {
                char_height_pixels: 0,
                char_width_pixels: 0,
                sign_height_pixels: 21,
                sign_width_pixels: 50,
                ..Default::default()
            })
            .with_font_definition(font_table())
            .with_multi_cfg(MultiCfg {
                default_font: 8,
                ..Default::default()
            })
            .build()
            .unwrap()
    }

    fn pattern_rects<'p>(
        dms: &'p Dms<2, 0>,
        ms: &'p str,
    ) -> impl Iterator<Item = (Rectangle, u8)> + 'p {
        PatIter::new(&dms, ms).flatten().filter_map(|v| match v {
            PatValue::FillableRect(r, f) => Some((r, f)),
            _ => None,
        })
    }

    #[test]
    fn fillable_rect1() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect2() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "TEXT");
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect3() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[np]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect4() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "FIRST[np]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect5() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[np]SECOND");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect6() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[np][np]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect7() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[tr1,1,50,24]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 24), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect8() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[tr1,1,50,24]TEXT");
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect9() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[tr1,1,50,12][tr1,14,50,12]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 12), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 14, 50, 12), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect10() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[tr1,1,50,12][fo7][tr1,14,50,12]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 12), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 14, 50, 12), 7)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect11() {
        let dms = make_dms();
        let mut r =
            pattern_rects(&dms, "[tr1,1,50,12][fo7][tr1,14,50,12][fo8][np]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 12), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 14, 50, 12), 7)));
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 50, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_rect12() {
        let dms = make_dms();
        let mut r = pattern_rects(&dms, "[tr1,1,25,0][tr26,1,0,0]");
        assert_eq!(r.next(), Some((Rectangle::new(1, 1, 25, 21), 8)));
        assert_eq!(r.next(), Some((Rectangle::new(26, 1, 25, 21), 8)));
        assert_eq!(r.next(), None);
    }

    #[test]
    fn fillable_width_1() {
        let dms = make_dms();
        let mut w = MessagePattern::new(&dms, "").widths();
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_width_2() {
        let dms = make_dms();
        let mut w = MessagePattern::new(&dms, "[np]").widths();
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_width_3() {
        let dms = make_dms();
        let mut w = MessagePattern::new(&dms, "FIRST[np]").widths();
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_width_4() {
        let dms = make_dms();
        let mut w =
            MessagePattern::new(&dms, "[tr1,1,50,12][tr1,14,50,12]").widths();
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_width_5() {
        let dms = make_dms();
        let mut w =
            MessagePattern::new(&dms, "[tr1,1,50,12][fo7][tr1,14,50,12]")
                .widths();
        assert_eq!(w.next(), Some((50, 8)));
        assert_eq!(w.next(), Some((50, 7)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_width_6() {
        let dms = make_dms();
        let mut w =
            MessagePattern::new(&dms, "[tr1,1,25,0][tr26,1,0,0]").widths();
        assert_eq!(w.next(), Some((25, 8)));
        assert_eq!(w.next(), Some((25, 8)));
        assert_eq!(w.next(), Some((25, 8)));
        assert_eq!(w.next(), Some((25, 8)));
        assert_eq!(w.next(), None);
    }

    #[test]
    fn fillable_lines_fail() {
        let dms = make_dms();
        let mut l = MessagePattern::new(&dms, "TEXT").lines("TEXT");
        assert_eq!(l.next(), None);
        l = MessagePattern::new(&dms, "[tr1,1,25,0]")
            .lines("[tr1,1,25,21]TEXT");
        assert_eq!(l.next(), None);
    }

    #[test]
    fn fill_1() {
        let dms = make_dms();
        let lines = ["ABC", "DEF", "GHI"].into_iter();
        let pattern = "LINE 1[nl]LINE 2[nl]LINE 3";
        let res = MessagePattern::new(&dms, pattern).fill(lines);
        assert_eq!(res, pattern);
    }

    fn roundtrip_1(pattern: &str, ms: &str) {
        let dms = make_dms();
        let lines = MessagePattern::new(&dms, pattern).lines(ms);
        let res = MessagePattern::new(&dms, pattern).fill(lines);
        assert_eq!(res, ms);
    }

    fn roundtrip_2(pattern: &str, ms: &str, ms2: &str) {
        let dms = make_dms();
        let lines = MessagePattern::new(&dms, pattern).lines(ms);
        let res = MessagePattern::new(&dms, pattern).fill(lines);
        assert_eq!(res, ms2);
    }

    #[test]
    fn roundtrip_fillable_1() {
        roundtrip_1("", "[nl]");
        roundtrip_1("", "LINE 1[nl]LINE 2");
        roundtrip_1("", "ABC[nl][jl2]D[jl3]E[jl4]F");
        roundtrip_1("", "ABC[sc3]DEF[/sc][nl]");
        roundtrip_1("[np]", "[nl][np][nl]");
        roundtrip_1("[np]", "PAGE 1[nl][np]PAGE 2[nl]");
        roundtrip_1("FIRST[np]", "FIRST[np]SECOND[nl]");
        roundtrip_1("[np][np]", "[nl][np][nl][np][nl]");
        roundtrip_1("", "CRASH[nl]AHEAD");
        roundtrip_1(
            "[tr1,1,50,12][tr1,14,50,12]",
            "[tr1,1,50,12]FIRST[tr1,14,50,12]SECOND",
        );
        roundtrip_1(
            "[tr1,1,50,12][fo7][tr1,14,50,12]",
            "[tr1,1,50,12]1ST[fo7][tr1,14,50,12]2ND",
        );
        roundtrip_1(
            "[tr1,1,50,8][fo7][tr1,10,50,7]",
            "[tr1,1,50,8]ABC[fo7][tr1,10,50,7]123",
        );
        roundtrip_1("[g1][tr1,10,50,8]", "[g1][tr1,10,50,8]TEXT");
    }

    #[test]
    fn roundtrip_fillable_2() {
        roundtrip_2("", "ABC[nl3]DEF", "ABC[nl]DEF");
        // check that non-line tags are stripped
        roundtrip_2("", "[cb8]ABC", "[nl]");
        roundtrip_2("", "[pb0,0,0]ABC", "[nl]");
        roundtrip_2("", "[cr255,0,0,0]ABC", "[nl]");
        roundtrip_2("", "[fo7]ABC", "[nl]");
        roundtrip_2("", "[g1,0,0]ABC", "[nl]");
        roundtrip_2("", "[jp3]ABC", "[nl]");
        roundtrip_2("", "[pt50o0]ABC", "[nl]");
        roundtrip_2("[np]", "PAGE 1[np]PAGE 2", "PAGE 1[nl][np]PAGE 2[nl]");
        roundtrip_2("FIRST[np]", "FIRST[np]SECOND", "FIRST[np]SECOND[nl]");
        roundtrip_2(
            "[tr1,1,25,0][tr26,1,0,0]",
            "[tr1,1,25,0]ONE[tr26,1,0,0]TWO",
            "[tr1,1,25,0]ONE[nl][tr26,1,0,0]TWO[nl]",
        );
    }
}
