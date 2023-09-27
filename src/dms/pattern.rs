// pattern.rs
//
// Copyright (C) 2023  Minnesota Department of Transportation
//
use crate::dms::multi::{Parser, Rectangle, SyntaxError, Value};
use crate::dms::sign::Dms;

/// Pattern values are MULTI values or "pseudo-values" from a pattern
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum PatternValue<'p> {
    /// MULTI value
    Value(Value<'p>),
    /// Fillable rectangle (w/font number)
    FillableRect(Rectangle, u8),
}

/// Find fillable text rectangles in a MULTI "pattern"
///
/// Returns an iterator of tuples containing a Rectangle and font
/// number for each fillable rectangle in the pattern.
pub(crate) struct PatternParser<'p> {
    /// Sign
    dms: &'p Dms,
    /// MULTI parser
    parser: Parser<'p>,
    /// Previous MULTI value
    value: Option<Value<'p>>,
    /// Current font number
    font_num: u8,
    /// Current rectangle + font number
    rect_font: Option<(Rectangle, u8)>,
    /// Have we reached the end?
    end: bool,
}

impl<'p> PatternParser<'p> {
    pub fn new(dms: &'p Dms, ms: &'p str) -> Self {
        let rect = dms.full_rect();
        let font_num = dms.multi_cfg.default_font;
        PatternParser {
            parser: Parser::new(ms),
            value: None,
            dms,
            font_num,
            rect_font: Some((rect, font_num)),
            end: false,
        }
    }
}

impl<'p> Iterator for PatternParser<'p> {
    type Item = Result<PatternValue<'p>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.value.take();
        if let Some(v) = value {
            return Some(Ok(PatternValue::Value(v)));
        }
        if self.end {
            return None;
        }
        let value = match self.parser.next() {
            Some(Ok(v)) => v,
            Some(Err(e)) => return Some(Err(e)),
            None => {
                self.end = true;
                Value::NewPage()
            }
        };
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
                    return Some(Ok(PatternValue::FillableRect(rect, font)));
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
                        return Some(Ok(PatternValue::FillableRect(
                            rect, font,
                        )));
                    }
                }
            }
            _ => (),
        }
        Some(Ok(PatternValue::Value(value)))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dms::config::*;
    use crate::dms::font::*;

    fn font_table() -> FontTable {
        let mut fonts = FontTable::default();
        let buf = include_bytes!("../../test/F07.ifnt");
        fonts.push(ifnt::read(&buf[..]).unwrap()).unwrap();
        let buf = include_bytes!("../../test/F08.ifnt");
        fonts.push(ifnt::read(&buf[..]).unwrap()).unwrap();
        fonts
    }

    fn make_dms() -> Dms {
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
    }

    fn pattern_rects<'p>(
        dms: &'p Dms,
        ms: &'p str,
    ) -> impl Iterator<Item = (Rectangle, u8)> + 'p {
        PatternParser::new(&dms, ms)
            .flatten()
            .filter_map(|v| match v {
                PatternValue::FillableRect(r, f) => Some((r, f)),
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
}
