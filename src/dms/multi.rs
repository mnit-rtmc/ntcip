// multi.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! MarkUp Language for Transportation Information
use log::{debug, warn};
use std::fmt;
use std::str::FromStr;

/// Classic color values
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ColorClassic {
    Black,
    Red,
    Yellow,
    Green,
    Cyan,
    Blue,
    Magenta,
    White,
    Orange,
    Amber,
}

/// Color scheme for dynamic message signs
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ColorScheme {
    /// Monochrome with 1-bit values
    Monochrome1Bit = 1,
    /// Monochrome with 8-bit values
    Monochrome8Bit,
    /// Classic color
    ColorClassic,
    /// 24-bit color
    Color24Bit,
}

/// Color value
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Color {
    /// Color for `Monochrome1Bit`, `Monochrome8Bit`, or `ColorClassic`
    Legacy(u8),
    /// Color for `Color24Bit`
    Rgb(u8, u8, u8),
}

/// A color context combines a scheme with foreground and background colors
#[derive(Clone)]
pub(crate) struct ColorCtx {
    /// Color scheme
    color_scheme: ColorScheme,
    /// Default foreground RGB color
    fg_default: (u8, u8, u8),
    /// Current foreground color
    fg_current: Color,
    /// Default background RGB color
    bg_default: (u8, u8, u8),
    /// Current background color
    bg_current: Color,
}

/// A rectangular area of a sign
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Rectangle {
    /// Left edge (starting from 1)
    pub x: u16,
    /// Top edge (starting from 1)
    pub y: u16,
    /// Width in pixels
    pub width: u16,
    /// Height in pixels
    pub height: u16,
}

/// Horizontal justification within a line
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum JustificationLine {
    /// Other line justification; deprecated in v2
    #[deprecated]
    Other = 1,
    /// Left line justification
    Left,
    /// Center line justification
    Center,
    /// Right line justification
    Right,
    /// Full line justification
    Full,
}

/// Vertical justification within a page
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum JustificationPage {
    /// Other page justification; deprecated in v2
    #[deprecated]
    Other = 1,
    /// Top page justification
    Top,
    /// Middle page justification
    Middle,
    /// Bottom page justification
    Bottom,
}

/// Order of flashing messages
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum FlashOrder {
    /// Flash on, then off
    OnOff,
    /// Flash off, then on
    OffOn,
}

/// Mode for moving text
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum MovingTextMode {
    /// Text wraps around, appearing circular
    Circular,
    /// No wrap around, delay in tenths of a second
    Linear(u8),
}

/// Direction for moving text
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum MovingTextDirection {
    /// Left-moving text
    Left,
    /// Right-moving text
    Right,
}

/// Values are tags or text from a parsed MULTI
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Value<'p> {
    /// Background color tag.
    ///
    /// This tag remains for backward compatibility with 1203v1.
    ///
    /// * Optional color (legacy only)
    ColorBackground(Option<Color>),

    /// Foreground color tag
    ///
    /// * Optional color
    ColorForeground(Option<Color>),

    /// Color rectangle tag
    ///
    /// * Dimensions to fill color
    /// * Rectangle color
    ColorRectangle(Rectangle, Color),

    /// Field tag
    ///
    /// * Field ID
    /// * Field width
    Field(u8, Option<u8>),

    /// Flash start tag
    ///
    /// * FlashOrder
    /// * Optional flash On or Off time
    /// * Optional flash Off or On time
    Flash(FlashOrder, Option<u8>, Option<u8>),

    /// Flash end tag
    FlashEnd(),

    /// Font tag
    ///
    /// * Tuple containing font number and optional version ID (hash)
    Font(Option<(u8, Option<u16>)>),

    /// Graphic tag
    ///
    /// * Graphic number
    /// * Optional tuple containing X and Y position and optional version ID
    Graphic(u8, Option<(u16, u16, Option<u16>)>),

    /// Hexadecimal character tag
    ///
    /// * Character number (code point)
    HexadecimalCharacter(u16),

    /// Line justification tag
    JustificationLine(Option<JustificationLine>),

    /// Page justification tag
    JustificationPage(Option<JustificationPage>),

    /// Manufacturer specific start tag
    ManufacturerSpecific(u32, Option<&'p str>),

    /// Manufacturer specific end tag
    ManufacturerSpecificEnd(u32, Option<&'p str>),

    /// Moving text tag
    ///
    /// * Moving text mode
    /// * Moving text direction
    /// * Width in pixels
    /// * Pixels offset at each time step
    /// * Deciseconds between time steps
    /// * Text to be moved
    MovingText(MovingTextMode, MovingTextDirection, u16, u8, u8, &'p str),

    /// New line tag
    ///
    /// * Optional line spacing
    NewLine(Option<u8>),

    /// New page tag
    NewPage(),

    /// Page background color tag
    ///
    /// * Optional color
    PageBackground(Option<Color>),

    /// Page time tag
    ///
    /// * Optional page-on time
    /// * Optional page-off time
    PageTime(Option<u8>, Option<u8>),

    /// Character spacing start tag
    ///
    /// * Pixel spacing between characters
    SpacingCharacter(u8),

    /// Character spacing end tag
    SpacingCharacterEnd(),

    /// Text value
    Text(&'p str),

    /// Text rectangle tag
    ///
    /// * Dimensions to restrict text
    TextRectangle(Rectangle),
}

/// Syntax errors from parsing MULTI
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    /// An unspecified error
    Other(&'static str),
    /// Specified tag not supported
    UnsupportedTag(String),
    /// Specified tag value not supported
    UnsupportedTagValue(String),
    /// Specified text does not fit within text rectangle
    TextTooBig,
    /// Specified font not defined
    FontNotDefined(u8),
    /// Specified character not defined in font
    CharacterNotDefined(char),
    /// Specified field device does not exist
    FieldDeviceNotExist,
    /// Field device communication error
    FieldDeviceError,
    /// Specified flash region not supported
    FlashRegionError,
    /// Specified tags conflict with each other
    TagConflict,
    /// Number of pages not supported
    TooManyPages,
    /// Specified font version ID does not match
    FontVersionID,
    /// Specified graphic version ID does not match
    GraphicID,
    /// Specified graphic number not defined
    GraphicNotDefined(u8),
}

/// Parser for MULTI values
#[derive(Clone)]
pub(crate) struct Parser<'p> {
    /// Remaining slice to parse
    ms: &'p str,
}

impl ColorClassic {
    /// Get RGB triplet for a classic color
    pub fn rgb(self) -> (u8, u8, u8) {
        match self {
            ColorClassic::Black => (0x00, 0x00, 0x00),
            ColorClassic::Red => (0xFF, 0x00, 0x00),
            ColorClassic::Yellow => (0xFF, 0xFF, 0x00),
            ColorClassic::Green => (0x00, 0xFF, 0x00),
            ColorClassic::Cyan => (0x00, 0xFF, 0xFF),
            ColorClassic::Blue => (0x00, 0x00, 0xFF),
            ColorClassic::Magenta => (0xFF, 0x00, 0xFF),
            ColorClassic::White => (0xFF, 0xFF, 0xFF),
            ColorClassic::Orange => (0xFF, 0xA5, 0x00),
            ColorClassic::Amber => (0xFF, 0xD0, 0x00),
        }
    }

    /// Maybe convert a u8 into a ColorClassic
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            v if v == ColorClassic::Black as u8 => Some(ColorClassic::Black),
            v if v == ColorClassic::Red as u8 => Some(ColorClassic::Red),
            v if v == ColorClassic::Yellow as u8 => Some(ColorClassic::Yellow),
            v if v == ColorClassic::Green as u8 => Some(ColorClassic::Green),
            v if v == ColorClassic::Cyan as u8 => Some(ColorClassic::Cyan),
            v if v == ColorClassic::Blue as u8 => Some(ColorClassic::Blue),
            v if v == ColorClassic::Magenta as u8 => {
                Some(ColorClassic::Magenta)
            }
            v if v == ColorClassic::White as u8 => Some(ColorClassic::White),
            v if v == ColorClassic::Orange as u8 => Some(ColorClassic::Orange),
            v if v == ColorClassic::Amber as u8 => Some(ColorClassic::Amber),
            _ => None,
        }
    }
}

impl From<ColorClassic> for u8 {
    fn from(c: ColorClassic) -> u8 {
        c as u8
    }
}

impl From<&str> for ColorScheme {
    /// Create a color scheme from a string
    fn from(s: &str) -> Self {
        match s {
            "monochrome1Bit" => ColorScheme::Monochrome1Bit,
            "monochrome8Bit" => ColorScheme::Monochrome8Bit,
            "colorClassic" => ColorScheme::ColorClassic,
            "color24Bit" => ColorScheme::Color24Bit,
            _ => {
                warn!("Unknown color scheme: {}", s);
                ColorScheme::Monochrome1Bit
            }
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Color::Legacy(v) => write!(f, "{v}"),
            Color::Rgb(r, g, b) => write!(f, "{r},{g},{b}"),
        }
    }
}

impl From<(u8, u8, u8)> for Color {
    fn from(rgb: (u8, u8, u8)) -> Self {
        Color::Rgb(rgb.0, rgb.1, rgb.2)
    }
}

impl Color {
    /// Get RGB triplet for a color
    pub fn rgb(self) -> (u8, u8, u8) {
        match self {
            Color::Legacy(v) => ColorCtx::rgb_classic(v).unwrap_or_default(),
            Color::Rgb(r, g, b) => (r, g, b),
        }
    }
}

impl ColorCtx {
    /// Create a new color context
    pub fn new(
        color_scheme: ColorScheme,
        fg_default: (u8, u8, u8),
        bg_default: (u8, u8, u8),
    ) -> Self {
        let fg_current = fg_default.into();
        let bg_current = bg_default.into();
        ColorCtx {
            color_scheme,
            fg_default,
            fg_current,
            bg_default,
            bg_current,
        }
    }

    /// Set the foreground color
    pub fn set_foreground(
        &mut self,
        c: Option<Color>,
        v: &Value,
    ) -> Result<(), SyntaxError> {
        self.fg_current = match c {
            Some(c) => match self.rgb(c) {
                Some(_) => c,
                None => return Err(SyntaxError::UnsupportedTagValue(v.into())),
            },
            None => self.fg_default.into(),
        };
        Ok(())
    }

    /// Get the foreground BGR color
    pub fn foreground(&self) -> Color {
        self.fg_current
    }

    /// Get the foreground RGB color
    pub fn foreground_rgb(&self) -> (u8, u8, u8) {
        match self.rgb(self.foreground()) {
            Some(rgb) => rgb,
            None => self.fg_default,
        }
    }

    /// Set the background color
    pub fn set_background(
        &mut self,
        c: Option<Color>,
        v: &Value,
    ) -> Result<(), SyntaxError> {
        self.bg_current = match c {
            Some(c) => match self.rgb(c) {
                Some(_) => c,
                None => return Err(SyntaxError::UnsupportedTagValue(v.into())),
            },
            None => self.bg_default.into(),
        };
        Ok(())
    }

    /// Get the background color
    pub fn background(&self) -> Color {
        self.bg_current
    }

    /// Get the background RGB color
    pub fn background_rgb(&self) -> (u8, u8, u8) {
        match self.rgb(self.background()) {
            Some(rgb) => rgb,
            None => self.bg_default,
        }
    }

    /// Get RGB for the specified color.
    pub fn rgb(&self, c: Color) -> Option<(u8, u8, u8)> {
        match (self.color_scheme, c) {
            (ColorScheme::Monochrome1Bit, Color::Legacy(v)) => {
                self.rgb_monochrome_1(v)
            }
            (ColorScheme::Monochrome1Bit, _) => None,
            (ColorScheme::Monochrome8Bit, Color::Legacy(v)) => {
                self.rgb_monochrome_8(v)
            }
            (ColorScheme::Monochrome8Bit, _) => None,
            (_, Color::Legacy(v)) => ColorCtx::rgb_classic(v),
            (ColorScheme::Color24Bit, Color::Rgb(r, g, b)) => Some((r, g, b)),
            _ => None,
        }
    }

    /// Get RGB for a monochrome 1-bit color.
    fn rgb_monochrome_1(&self, v: u8) -> Option<(u8, u8, u8)> {
        match v {
            0 => Some(self.bg_default),
            1 => Some(self.fg_default),
            _ => None,
        }
    }

    /// Get RGB for a monochrome 8-bit color.
    fn rgb_monochrome_8(&self, v: u8) -> Option<(u8, u8, u8)> {
        let bg = self.bg_default;
        let fg = self.fg_default;
        let r = ColorCtx::lerp(bg.0, fg.0, v);
        let g = ColorCtx::lerp(bg.1, fg.1, v);
        let b = ColorCtx::lerp(bg.2, fg.2, v);
        Some((r, g, b))
    }

    /// Get RGB for a classic color.
    fn rgb_classic(v: u8) -> Option<(u8, u8, u8)> {
        ColorClassic::from_u8(v).map(|c| c.rgb())
    }

    /// Interpolate between two color components
    fn lerp(bg: u8, fg: u8, v: u8) -> u8 {
        let d = bg.max(fg) - bg.min(fg);
        let c = d as u32 * v as u32;
        // cheap alternative to divide by 255
        let r = (((c + 1) + (c >> 8)) >> 8) as u8;
        bg.min(fg) + r
    }
}

impl fmt::Display for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{},{},{}", self.x, self.y, self.width, self.height)
    }
}

impl Default for Rectangle {
    fn default() -> Self {
        Rectangle::new(1, 1, 0, 0)
    }
}

impl Rectangle {
    /// Create a new rectangle
    pub fn new(x: u16, y: u16, width: u16, height: u16) -> Self {
        Rectangle {
            x,
            y,
            width,
            height,
        }
    }

    /// Extend 0 width and/or height to full rectangle edges
    pub(crate) fn extend_width_height(self, full: Self) -> Self {
        let width = if self.width > 0 {
            self.width
        } else {
            full.width.saturating_sub(self.x.saturating_sub(1))
        };
        let height = if self.height > 0 {
            self.height
        } else {
            full.height.saturating_sub(self.y.saturating_sub(1))
        };
        Rectangle::new(self.x, self.y, width, height)
    }

    /// Create intersection between this and another rectangle
    pub fn intersection(self, rhs: Self) -> Self {
        let left = self.x.max(rhs.x);
        let right = (self.x + self.width).min(rhs.x + rhs.width);
        let top = self.y.max(rhs.y);
        let bottom = (self.y + self.height).min(rhs.y + rhs.height);
        if right > left && bottom > top {
            let width = right - left;
            let height = bottom - top;
            Rectangle::new(left, top, width, height)
        } else {
            Rectangle::default()
        }
    }

    /// Check if a rectangle contains another rectangle
    pub fn contains(self, other: Self) -> bool {
        other.x >= self.x
            && other.x + other.width <= self.x + self.width
            && other.y >= self.y
            && other.y + other.height <= self.y + self.height
    }
}

impl fmt::Display for JustificationLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

impl JustificationLine {
    /// Create a line justification.
    pub fn new(v: &str) -> Option<Self> {
        #[allow(deprecated)]
        match v {
            "1" => Some(JustificationLine::Other),
            "2" => Some(JustificationLine::Left),
            "3" => Some(JustificationLine::Center),
            "4" => Some(JustificationLine::Right),
            "5" => Some(JustificationLine::Full),
            _ => None,
        }
    }
}

impl fmt::Display for JustificationPage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

impl JustificationPage {
    /// Create a page justification.
    pub fn new(v: &str) -> Option<Self> {
        #[allow(deprecated)]
        match v {
            "1" => Some(JustificationPage::Other),
            "2" => Some(JustificationPage::Top),
            "3" => Some(JustificationPage::Middle),
            "4" => Some(JustificationPage::Bottom),
            _ => None,
        }
    }
}

impl fmt::Display for MovingTextMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MovingTextMode::Circular => write!(f, "c"),
            MovingTextMode::Linear(x) => write!(f, "l{x}"),
        }
    }
}

impl fmt::Display for MovingTextDirection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MovingTextDirection::Left => write!(f, "l"),
            MovingTextDirection::Right => write!(f, "r"),
        }
    }
}

impl<'p> fmt::Display for Value<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::ColorBackground(None) => write!(f, "[cb]"),
            Value::ColorBackground(Some(c)) => write!(f, "[cb{c}]"),
            Value::ColorForeground(None) => write!(f, "[cf]"),
            Value::ColorForeground(Some(c)) => write!(f, "[cf{c}]"),
            Value::ColorRectangle(r, c) => write!(f, "[cr{r},{c}"),
            Value::Field(i, Some(w)) => write!(f, "[f{i},{w}]"),
            Value::Field(i, None) => write!(f, "[f{i}]"),
            Value::Flash(FlashOrder::OnOff, Some(a), Some(b)) => {
                write!(f, "[flt{a}o{b}]")
            }
            Value::Flash(FlashOrder::OnOff, Some(a), None) => {
                write!(f, "[flt{a}o]")
            }
            Value::Flash(FlashOrder::OnOff, None, Some(b)) => {
                write!(f, "[flto{b}]")
            }
            Value::Flash(FlashOrder::OnOff, None, None) => write!(f, "[flto]"),
            Value::Flash(FlashOrder::OffOn, Some(a), Some(b)) => {
                write!(f, "[flo{a}t{b}]")
            }
            Value::Flash(FlashOrder::OffOn, Some(a), None) => {
                write!(f, "[flo{a}t]")
            }
            Value::Flash(FlashOrder::OffOn, None, Some(b)) => {
                write!(f, "[flot{b}]")
            }
            Value::Flash(FlashOrder::OffOn, None, None) => write!(f, "[flot]"),
            Value::FlashEnd() => write!(f, "[/fl]"),
            Value::Font(None) => write!(f, "[fo]"),
            Value::Font(Some((num, None))) => write!(f, "[fo{num}]"),
            Value::Font(Some((num, Some(c)))) => {
                write!(f, "[fo{num},{c:04x}]")
            }
            Value::Graphic(num, None) => write!(f, "[g{num}]"),
            Value::Graphic(num, Some((x, y, None))) => {
                write!(f, "[g{num},{x},{y}]")
            }
            Value::Graphic(num, Some((x, y, Some(c)))) => {
                write!(f, "[g{num},{x},{y},{c:04x}]")
            }
            Value::HexadecimalCharacter(c) => write!(f, "[hc{c}]"),
            Value::JustificationLine(Some(j)) => write!(f, "[jl{j}]"),
            Value::JustificationLine(None) => write!(f, "[jl]"),
            Value::JustificationPage(Some(j)) => write!(f, "[jp{j}]"),
            Value::JustificationPage(None) => write!(f, "[jp]"),
            Value::ManufacturerSpecific(i, Some(s)) => {
                write!(f, "[ms{i},{s}]")
            }
            Value::ManufacturerSpecific(i, None) => write!(f, "[ms{i}]"),
            Value::ManufacturerSpecificEnd(i, Some(s)) => {
                write!(f, "[/ms{i},{s}]")
            }
            Value::ManufacturerSpecificEnd(i, None) => write!(f, "[/ms{i}]"),
            Value::MovingText(mode, dir, width, s, r, text) => {
                write!(f, "[mv{mode}{dir}{width},{s},{r},{text}]")
            }
            Value::NewLine(Some(x)) => write!(f, "[nl{x}]"),
            Value::NewLine(None) => write!(f, "[nl]"),
            Value::NewPage() => write!(f, "[np]"),
            Value::PageBackground(Some(c)) => write!(f, "[pb{c}]"),
            Value::PageBackground(None) => write!(f, "[pb]"),
            Value::PageTime(Some(x), Some(y)) => write!(f, "[pt{x}o{y}]"),
            Value::PageTime(Some(x), None) => write!(f, "[pt{x}o]"),
            Value::PageTime(None, Some(y)) => write!(f, "[pto{y}]"),
            Value::PageTime(None, None) => write!(f, "[pto]"),
            Value::SpacingCharacter(s) => write!(f, "[sc{s}]"),
            Value::SpacingCharacterEnd() => write!(f, "[/sc]"),
            Value::Text(t) => {
                write!(f, "{}", t.replace('[', "[[").replace(']', "]]"))
            }
            Value::TextRectangle(r) => write!(f, "[tr{r}]"),
        }
    }
}

impl<'p> From<Value<'p>> for String {
    fn from(v: Value<'p>) -> String {
        format!("{v}")
    }
}

impl<'p> From<&Value<'p>> for String {
    fn from(v: &Value<'p>) -> Self {
        format!("{v}")
    }
}

impl<'p> Value<'p> {
    /// Check if a `Value` is "blank"
    fn is_blank(&self) -> bool {
        match self {
            Value::ColorRectangle(_, _) => false,
            Value::Field(_, _) => false,
            Value::Graphic(_, _) => false,
            Value::HexadecimalCharacter(_) => false,
            Value::ManufacturerSpecific(_, _) => false,
            Value::ManufacturerSpecificEnd(_, _) => false,
            Value::MovingText(_, _, _, _, _, _) => false,
            Value::Text(txt) => txt.trim() == "",
            _ => true,
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "syntaxError: {self:?}")
    }
}

impl std::error::Error for SyntaxError {}

/// Parse a color from a tag
///
/// * `v` Iterator of color parameters.
fn parse_color<'a, I>(v: I) -> Option<Color>
where
    I: Iterator<Item = &'a str>,
{
    let mut rgb = v.map(|i| i.parse::<u8>());
    match (rgb.next(), rgb.next(), rgb.next(), rgb.next()) {
        (Some(Ok(r)), Some(Ok(g)), Some(Ok(b)), None) => {
            Some(Color::Rgb(r, g, b))
        }
        (Some(Ok(n)), None, _, _) => Some(Color::Legacy(n)),
        _ => None,
    }
}

/// Parse a rectangle from a tag
///
/// * `v` Iterator of rectangle parameters.
fn parse_rectangle<'a, I>(v: &mut I) -> Option<Rectangle>
where
    I: Iterator<Item = &'a str>,
{
    if let (Some(x), Some(y), Some(w), Some(h)) =
        (v.next(), v.next(), v.next(), v.next())
    {
        if let (Ok(x), Ok(y), Ok(w), Ok(h)) =
            (x.parse(), y.parse(), w.parse(), h.parse())
        {
            if x > 0 && y > 0 {
                return Some(Rectangle::new(x, y, w, h));
            }
        }
    }
    None
}

/// Parse an integer value
fn parse_int<'a, I, T>(v: &mut I) -> Option<T>
where
    I: Iterator<Item = &'a str>,
    T: FromStr,
{
    if let Some(s) = v.next() {
        if let Ok(i) = s.parse::<T>() {
            return Some(i);
        }
    }
    None
}

/// Parse an optional value
fn parse_optional<'a, I, T>(v: &mut I) -> Result<Option<T>, ()>
where
    I: Iterator<Item = &'a str>,
    T: FromStr,
{
    if let Some(s) = v.next() {
        if s.is_empty() {
            return Ok(None);
        }
        if let Ok(i) = s.parse::<T>() {
            Ok(Some(i))
        } else {
            Err(())
        }
    } else {
        Ok(None)
    }
}

/// Parse an optional value ranging from 0 to 99
fn parse_optional_99<'a, I>(v: &mut I) -> Result<Option<u8>, ()>
where
    I: Iterator<Item = &'a str>,
{
    if let Some(s) = v.next() {
        if s.is_empty() {
            return Ok(None);
        }
        if let Ok(i) = s.parse::<u8>() {
            if i <= 99 {
                return Ok(Some(i));
            }
        }
        return Err(());
    }
    Ok(None)
}

/// Parse a nonzero value
fn parse_nonzero<'a, I, T>(v: &mut I) -> Option<T>
where
    I: Iterator<Item = &'a str>,
    T: FromStr + PartialOrd + Default,
{
    if let Some(s) = v.next() {
        if let Ok(i) = s.parse::<T>() {
            // Use default to check for nonzero
            if i != T::default() {
                return Some(i);
            }
        }
    }
    None
}

/// Parse a version ID value
fn parse_version_id<'a, I>(v: &mut I) -> Result<Option<u16>, ()>
where
    I: Iterator<Item = &'a str>,
{
    match v.next() {
        Some(s) if s.len() == 4 => match u16::from_str_radix(s, 16) {
            Ok(i) => Ok(Some(i)),
            _ => Err(()),
        },
        Some(_) => Err(()),
        _ => Ok(None),
    }
}

/// Parse a Color -- Background tag (cb)
fn parse_color_background(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        // 1203 specifies a numeric value between 0 and 999,
        // but anything above 255 does not make sense
        match tag[2..].parse::<u8>() {
            Ok(n) => Some(Value::ColorBackground(Some(Color::Legacy(n)))),
            Err(_) => None,
        }
    } else {
        Some(Value::ColorBackground(None))
    }
}

/// Parse a Page -- Background tag [pb]
fn parse_page_background(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        parse_color(tag[2..].split(',')).map(|c| Value::PageBackground(Some(c)))
    } else {
        Some(Value::PageBackground(None))
    }
}

/// Parse a Color -- Foreground tag [cf]
fn parse_color_foreground(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        parse_color(tag[2..].split(','))
            .map(|c| Value::ColorForeground(Some(c)))
    } else {
        Some(Value::ColorForeground(None))
    }
}

/// Parse a Color Rectangle tag [cr]
fn parse_color_rectangle(tag: &str) -> Option<Value> {
    let mut vs = tag[2..].splitn(7, ',');
    match (parse_rectangle(&mut vs), parse_color(vs)) {
        (Some(r), Some(c)) => Some(Value::ColorRectangle(r, c)),
        _ => None,
    }
}

/// Parse a Field tag [f]
fn parse_field(tag: &str) -> Option<Value> {
    let mut vs = tag[1..].splitn(2, ',');
    match (parse_int(&mut vs), parse_optional(&mut vs)) {
        (Some(fid), Ok(w)) if fid < 100 => Some(Value::Field(fid, w)),
        _ => None,
    }
}

/// Parse a Flash time tag [fl]
fn parse_flash_time(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        let v = &tag[2..];
        match &v[..1] {
            "t" | "T" => parse_flash_on(&v[1..]),
            "o" | "O" => parse_flash_off(&v[1..]),
            _ => None,
        }
    } else {
        Some(Value::Flash(FlashOrder::OnOff, None, None))
    }
}

/// Parse a flash on -> off tag fragment
fn parse_flash_on(v: &str) -> Option<Value> {
    let mut vs = v.splitn(2, |c| c == 'o' || c == 'O');
    let t = parse_optional_99(&mut vs).ok()?;
    let o = parse_optional_99(&mut vs).ok()?;
    Some(Value::Flash(FlashOrder::OnOff, t, o))
}

/// Parse a flash off -> on tag fragment
fn parse_flash_off(v: &str) -> Option<Value> {
    let mut vs = v.splitn(2, |c| c == 't' || c == 'T');
    let o = parse_optional_99(&mut vs).ok()?;
    let t = parse_optional_99(&mut vs).ok()?;
    Some(Value::Flash(FlashOrder::OffOn, o, t))
}

/// Parse a flash end tag [/fl]
fn parse_flash_end(tag: &str) -> Option<Value> {
    if tag.len() == 3 {
        Some(Value::FlashEnd())
    } else {
        None
    }
}

/// Parse a Font tag [fo]
fn parse_font(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        let mut vs = tag[2..].splitn(2, ',');
        match (parse_nonzero(&mut vs), parse_version_id(&mut vs)) {
            (Some(n), Ok(vid)) => Some(Value::Font(Some((n, vid)))),
            _ => None,
        }
    } else {
        Some(Value::Font(None))
    }
}

/// Parse a Graphic tag [g]
fn parse_graphic(tag: &str) -> Option<Value> {
    let mut vs = tag[1..].splitn(4, ',');
    let n = parse_nonzero(&mut vs);
    let p = parse_point(&mut vs);
    let vid = parse_version_id(&mut vs);
    match (n, p, vid) {
        (Some(n), Ok(Some((x, y))), Ok(vid)) => {
            Some(Value::Graphic(n, Some((x, y, vid))))
        }
        (Some(n), Ok(None), Ok(None)) => Some(Value::Graphic(n, None)),
        _ => None,
    }
}

/// Parse a point value
fn parse_point<'a, I>(v: &mut I) -> Result<Option<(u16, u16)>, ()>
where
    I: Iterator<Item = &'a str>,
{
    match (v.next(), v.next()) {
        (Some(x), Some(y)) => Ok(Some(parse_xy(x, y)?)),
        (Some(_), None) => Err(()),
        (_, _) => Ok(None),
    }
}

/// Parse an x/y pair
fn parse_xy(x: &str, y: &str) -> Result<(u16, u16), ()> {
    if let (Ok(x), Ok(y)) = (x.parse(), y.parse()) {
        if x > 0 && y > 0 {
            return Ok((x, y));
        }
    }
    Err(())
}

/// Parse a hexadecimal character tag [hc]
fn parse_hexadecimal_character(tag: &str) -> Option<Value> {
    let mut vs = std::iter::once(&tag[2..]);
    match parse_hexadecimal(&mut vs) {
        Ok(hc) => Some(Value::HexadecimalCharacter(hc)),
        Err(_) => None,
    }
}

/// Parse a hexadecimal value
fn parse_hexadecimal<'a, I>(v: &mut I) -> Result<u16, ()>
where
    I: Iterator<Item = &'a str>,
{
    if let Some(s) = v.next() {
        // May be 1 to 4 hexadecimal digits
        if let Ok(i) = u16::from_str_radix(s, 16) {
            return Ok(i);
        }
    }
    Err(())
}

/// Parse a Justification -- Line tag [jl]
fn parse_justification_line(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        JustificationLine::new(&tag[2..])
            .map(|jl| Value::JustificationLine(Some(jl)))
    } else {
        Some(Value::JustificationLine(None))
    }
}

/// Parse a Justification -- Page tag [jp]
fn parse_justification_page(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        JustificationPage::new(&tag[2..])
            .map(|jl| Value::JustificationPage(Some(jl)))
    } else {
        Some(Value::JustificationPage(None))
    }
}

/// Parse a Manufacturer Specific tag [ms]
fn parse_manufacturer_specific(tag: &str) -> Option<Value> {
    let mut vs = tag[2..].splitn(2, ',');
    match (parse_int(&mut vs), vs.next()) {
        (Some(m), Some(t)) => Some(Value::ManufacturerSpecific(m, Some(t))),
        (Some(m), None) => Some(Value::ManufacturerSpecific(m, None)),
        _ => None,
    }
}

/// Parse a Manufacturer Specific end tag [/ms]
fn parse_manufacturer_specific_end(tag: &str) -> Option<Value> {
    let mut vs = tag[3..].splitn(2, ',');
    match (parse_int(&mut vs), vs.next()) {
        (Some(m), Some(t)) => Some(Value::ManufacturerSpecificEnd(m, Some(t))),
        (Some(m), None) => Some(Value::ManufacturerSpecificEnd(m, None)),
        _ => None,
    }
}

/// Parse a Moving text tag [mv]
fn parse_moving_text(tag: &str) -> Option<Value> {
    if tag.len() > 2 {
        let t = &tag[3..];
        match &tag[2..3] {
            "c" | "C" => parse_moving_text_mode(t, MovingTextMode::Circular),
            "l" | "L" => parse_moving_text_linear(t),
            _ => None,
        }
    } else {
        None
    }
}

/// Parse a moving text linear fragment
fn parse_moving_text_linear(tag: &str) -> Option<Value> {
    if !tag.is_empty() {
        let t = &tag[1..];
        if let Ok(i) = &tag[..1].parse::<u8>() {
            parse_moving_text_mode(t, MovingTextMode::Linear(*i))
        } else {
            parse_moving_text_mode(tag, MovingTextMode::Linear(0))
        }
    } else {
        None
    }
}

/// Parse a moving text mode fragment
fn parse_moving_text_mode(tag: &str, mode: MovingTextMode) -> Option<Value> {
    if !tag.is_empty() {
        let dir = parse_moving_text_dir(tag.chars().next());
        let mut vs = tag[1..].splitn(4, ',');
        let width = parse_int(&mut vs);
        let s = parse_int(&mut vs);
        let r = parse_int(&mut vs);
        let text = vs.next();
        if let (Some(dir), Some(width), Some(s), Some(r), Some(text)) =
            (dir, width, s, r, text)
        {
            return Some(Value::MovingText(mode, dir, width, s, r, text));
        }
    }
    None
}

/// Parse moving text direction
fn parse_moving_text_dir(d: Option<char>) -> Option<MovingTextDirection> {
    match d {
        Some('l') | Some('L') => Some(MovingTextDirection::Left),
        Some('r') | Some('R') => Some(MovingTextDirection::Right),
        _ => None,
    }
}

/// Parse a New Line tag [nl]
fn parse_new_line(tag: &str) -> Option<Value> {
    // 1203 only specifies a single digit parameter for "nl" tag (0-9)
    match tag.len() {
        2 => Some(Value::NewLine(None)),
        3 => match tag[2..].parse::<u8>() {
            Ok(n) => Some(Value::NewLine(Some(n))),
            Err(_) => None,
        },
        _ => None,
    }
}

/// Parse a New Page tag [np]
fn parse_new_page(tag: &str) -> Option<Value> {
    match tag.len() {
        2 => Some(Value::NewPage()),
        _ => None,
    }
}

/// Parse a Page Time tag [pt]
fn parse_page_time(tag: &str) -> Option<Value> {
    let mut vs = tag[2..].splitn(2, |c| c == 'o' || c == 'O');
    match (parse_optional(&mut vs), parse_optional(&mut vs)) {
        (Ok(t), Ok(o)) => Some(Value::PageTime(t, o)),
        _ => None,
    }
}

/// Parse a Spacing -- Character tag [sc]
fn parse_spacing_character(tag: &str) -> Option<Value> {
    let mut vs = std::iter::once(&tag[2..]);
    match parse_int(&mut vs) {
        Some(s) if s < 100 => Some(Value::SpacingCharacter(s)),
        _ => None,
    }
}

/// Parse a Spacing -- Character end tag [/sc]
fn parse_spacing_character_end(tag: &str) -> Option<Value> {
    if tag.len() == 3 {
        Some(Value::SpacingCharacterEnd())
    } else {
        None
    }
}

/// Parse a Text Rectangle tag [tr]
fn parse_text_rectangle(tag: &str) -> Option<Value> {
    let mut vs = tag[2..].splitn(4, ',');
    parse_rectangle(&mut vs).map(Value::TextRectangle)
}

/// Parse a tag (without brackets)
fn parse_tag(tag: &str) -> Result<Option<Value>, SyntaxError> {
    let mut chars = tag.chars().map(|c| c.to_ascii_lowercase());
    let (t0, t1, t2) = (chars.next(), chars.next(), chars.next());
    // Sorted by most likely occurrence
    let val = match (t0, t1, t2) {
        (Some('n'), Some('l'), _) => parse_new_line(tag),
        (Some('n'), Some('p'), _) => parse_new_page(tag),
        (Some('f'), Some('o'), _) => parse_font(tag),
        (Some('j'), Some('l'), _) => parse_justification_line(tag),
        (Some('j'), Some('p'), _) => parse_justification_page(tag),
        (Some('p'), Some('t'), _) => parse_page_time(tag),
        (Some('p'), Some('b'), _) => parse_page_background(tag),
        (Some('c'), Some('f'), _) => parse_color_foreground(tag),
        (Some('c'), Some('r'), _) => parse_color_rectangle(tag),
        (Some('t'), Some('r'), _) => parse_text_rectangle(tag),
        (Some('c'), Some('b'), _) => parse_color_background(tag),
        (Some('s'), Some('c'), _) => parse_spacing_character(tag),
        (Some('h'), Some('c'), _) => parse_hexadecimal_character(tag),
        (Some('f'), Some('l'), _) => parse_flash_time(tag),
        (Some('m'), Some('v'), _) => parse_moving_text(tag),
        (Some('m'), Some('s'), _) => parse_manufacturer_specific(tag),
        // Don't treat "fe" as a field tag -- this allows non-MULTI
        // tag (e.g. [feedx]) properly by returning UnsupportedTag.
        (Some('f'), Some('e'), _) => {
            return Err(SyntaxError::UnsupportedTag(tag.into()))
        }
        (Some('f'), _, _) => parse_field(tag),
        (Some('g'), _, _) => parse_graphic(tag),
        (Some('/'), Some('s'), Some('c')) => parse_spacing_character_end(tag),
        (Some('/'), Some('f'), Some('l')) => parse_flash_end(tag),
        (Some('/'), Some('m'), Some('s')) => {
            parse_manufacturer_specific_end(tag)
        }
        _ => return Err(SyntaxError::UnsupportedTag(tag.into())),
    };
    match val {
        Some(val) => Ok(Some(val)),
        None => Err(SyntaxError::UnsupportedTagValue(tag.into())),
    }
}

impl<'p> Parser<'p> {
    /// Create a new MULTI parser
    pub fn new(ms: &'p str) -> Self {
        debug!("Parser::new {}", ms);
        Parser { ms }
    }

    /// Parse a value at the current position
    fn parse_value(&mut self) -> Result<Option<Value<'p>>, SyntaxError> {
        if self.ms.starts_with("[[") || self.ms.starts_with("]]") {
            let span = &self.ms[..1];
            self.ms = &self.ms[2..];
            return Ok(Some(Value::Text(span)));
        }
        let tag = self.ms.starts_with('[');
        for (i, c) in self.ms.char_indices() {
            match c {
                ' '..='~' => (),
                _ => {
                    self.ms = self.ms.strip_prefix(c).unwrap_or("");
                    return Err(SyntaxError::CharacterNotDefined(c));
                }
            }
            if tag {
                if c == ']' {
                    let tag = &self.ms[1..i];
                    self.ms = &self.ms[i + 1..];
                    return parse_tag(tag);
                }
            } else if c == '[' {
                let span = &self.ms[..i];
                self.ms = &self.ms[i..];
                return Ok(Some(Value::Text(span)));
            } else if c == ']' {
                if i > 0 {
                    let span = &self.ms[..i];
                    self.ms = &self.ms[i..];
                    return Ok(Some(Value::Text(span)));
                } else {
                    self.ms = &self.ms[1..];
                    return Err(SyntaxError::UnsupportedTag("]".to_string()));
                }
            }
        }
        if tag {
            let tag = self.ms;
            self.ms = "";
            Err(SyntaxError::UnsupportedTag(tag.to_string()))
        } else {
            let span = self.ms;
            self.ms = "";
            if span.is_empty() {
                Ok(None)
            } else {
                Ok(Some(Value::Text(span)))
            }
        }
    }
}

impl<'p> Iterator for Parser<'p> {
    type Item = Result<Value<'p>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_value().transpose()
    }
}

/// Normalize a MULTI string
pub fn normalize(ms: &str) -> String {
    let mut s = String::with_capacity(ms.len());
    for t in Parser::new(ms).flatten() {
        s.push_str(&t.to_string());
    }
    s
}

/// Check if a MULTI string is a "blank" message
pub fn is_blank(ms: &str) -> bool {
    for val in Parser::new(ms) {
        match val {
            Ok(value) => {
                if !value.is_blank() {
                    return false;
                }
            }
            Err(_) => return false,
        }
    }
    true
}

/// Get an iterator of text spans in a MULTI string
pub fn text_spans(ms: &str) -> impl Iterator<Item = &str> + '_ {
    Parser::new(ms).flatten().filter_map(|v| match v {
        Value::Text(t) => Some(t),
        _ => None,
    })
}

/// Remove MULTI tags and join text spans
pub fn join_text(ms: &str, sep: &str) -> String {
    text_spans(ms).collect::<Vec<_>>().join(sep)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn value_size() {
        assert_eq!(std::mem::size_of::<Value>(), 24);
    }

    #[test]
    fn color_component() {
        assert_eq!(ColorCtx::lerp(0, 255, 0), 0);
        assert_eq!(ColorCtx::lerp(0, 255, 128), 128);
        assert_eq!(ColorCtx::lerp(0, 255, 255), 255);
        assert_eq!(ColorCtx::lerp(0, 128, 0), 0);
        assert_eq!(ColorCtx::lerp(0, 128, 128), 64);
        assert_eq!(ColorCtx::lerp(0, 128, 255), 128);
        assert_eq!(ColorCtx::lerp(128, 255, 0), 128);
        assert_eq!(ColorCtx::lerp(128, 255, 128), 191);
        assert_eq!(ColorCtx::lerp(128, 255, 255), 255);
    }

    #[test]
    fn color_mono_1() {
        let mut ctx = ColorCtx::new(
            ColorScheme::Monochrome1Bit,
            ColorClassic::Amber.rgb(),
            ColorClassic::Black.rgb(),
        );
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xD0, 0x00));
        assert_eq!(ctx.background_rgb(), (0x00, 0x00, 0x00));
        let v = Value::ColorForeground(Some(Color::Legacy(2)));
        assert_eq!(
            ctx.set_foreground(Some(Color::Legacy(2)), &v),
            Err(SyntaxError::UnsupportedTagValue("[cf2]".into()))
        );
        let v = Value::ColorForeground(Some(Color::Rgb(0, 0, 0)));
        assert_eq!(
            ctx.set_foreground(Some(Color::Rgb(0, 0, 0)), &v),
            Err(SyntaxError::UnsupportedTagValue("[cf0,0,0]".into()))
        );
        let v = Value::ColorForeground(Some(Color::Legacy(0)));
        assert_eq!(ctx.set_foreground(Some(Color::Legacy(0)), &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0x00, 0x00, 0x00));
        let v = Value::PageBackground(Some(Color::Legacy(1)));
        assert_eq!(ctx.set_background(Some(Color::Legacy(1)), &v), Ok(()));
        assert_eq!(ctx.background_rgb(), (0xFF, 0xD0, 0x00));
        assert_eq!(ctx.set_foreground(None, &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xD0, 0x00));
    }

    #[test]
    fn color_mono_8() {
        let mut ctx = ColorCtx::new(
            ColorScheme::Monochrome8Bit,
            ColorClassic::White.rgb(),
            ColorClassic::Black.rgb(),
        );
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0xFF));
        assert_eq!(ctx.background_rgb(), (0x00, 0x00, 0x00));
        let v = Value::ColorForeground(Some(Color::Legacy(128)));
        assert_eq!(ctx.set_foreground(Some(Color::Legacy(128)), &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0x80, 0x80, 0x80));
        let v = Value::ColorForeground(Some(Color::Rgb(128, 128, 128)));
        assert_eq!(
            ctx.set_foreground(Some(Color::Rgb(128, 128, 128)), &v),
            Err(SyntaxError::UnsupportedTagValue("[cf128,128,128]".into()))
        );
        assert_eq!(ctx.set_foreground(None, &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0xFF));
    }

    #[test]
    fn color_classic() {
        let mut ctx = ColorCtx::new(
            ColorScheme::ColorClassic,
            ColorClassic::White.rgb(),
            ColorClassic::Green.rgb(),
        );
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0xFF));
        assert_eq!(ctx.background_rgb(), (0x00, 0xFF, 0x00));
        let v = Value::ColorForeground(Some(Color::Legacy(10)));
        assert_eq!(
            ctx.set_foreground(Some(Color::Legacy(10)), &v),
            Err(SyntaxError::UnsupportedTagValue("[cf10]".into()))
        );
        let v = Value::ColorForeground(Some(Color::Legacy(5)));
        assert_eq!(ctx.set_foreground(Some(Color::Legacy(5)), &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0x00, 0x00, 0xFF));
        let v = Value::PageBackground(Some(Color::Rgb(255, 0, 255)));
        assert_eq!(
            ctx.set_background(Some(Color::Rgb(255, 0, 255)), &v),
            Err(SyntaxError::UnsupportedTagValue("[pb255,0,255]".into()))
        );
        assert_eq!(ctx.set_foreground(None, &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0xFF));
    }

    #[test]
    fn color_24() {
        let mut ctx = ColorCtx::new(
            ColorScheme::Color24Bit,
            ColorClassic::Yellow.rgb(),
            ColorClassic::Red.rgb(),
        );
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0x00));
        assert_eq!(ctx.background_rgb(), (0xFF, 0x00, 0x00));
        let v = Value::ColorForeground(Some(Color::Legacy(10)));
        assert_eq!(
            ctx.set_foreground(Some(Color::Legacy(10)), &v),
            Err(SyntaxError::UnsupportedTagValue("[cf10]".into()))
        );
        let v = Value::ColorForeground(Some(Color::Legacy(6)));
        assert_eq!(ctx.set_foreground(Some(Color::Legacy(6)), &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0x00, 0xFF));
        let v = Value::PageBackground(Some(Color::Rgb(121, 0, 212)));
        assert_eq!(
            ctx.set_background(Some(Color::Rgb(121, 0, 212)), &v),
            Ok(())
        );
        assert_eq!(ctx.background_rgb(), (0x79, 0x00, 0xD4));
        assert_eq!(ctx.set_foreground(None, &v), Ok(()));
        assert_eq!(ctx.foreground_rgb(), (0xFF, 0xFF, 0x00));
    }

    fn single_text(v: &str) {
        let mut m = Parser::new(v);
        assert_eq!(m.next(), Some(Ok(Value::Text(v.into()))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_text() {
        single_text("THIS IS A TEST");
    }

    #[test]
    fn parse_lower() {
        single_text("this is lower case");
    }

    #[test]
    fn parse_bracket() {
        let mut m = Parser::new("[[a]]b[[[[c]][[]]]]d");
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("a".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("]".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("b".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("c".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("]".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("]".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("]".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("d".into()))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_bracket2() {
        let mut m = Parser::new("[[[[[[[[");
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn norm_cb() {
        assert_eq!(normalize("[cb1][CB255]"), "[cb1][cb255]");
        assert_eq!(normalize("[cb][cb256]"), "[cb]");
    }

    #[test]
    fn parse_cb1() {
        let mut m = Parser::new("[cb0][CB1][cB255][cb256][cb]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorBackground(Some(Color::Legacy(0)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorBackground(Some(Color::Legacy(1)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorBackground(Some(Color::Legacy(255)))))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cb256".into())))
        );
        assert_eq!(m.next(), Some(Ok(Value::ColorBackground(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cb2() {
        let mut m = Parser::new("[cbX][cb0,0,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cbX".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cb0,0,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn norm_pb() {
        assert_eq!(normalize("[pb0][PB255]"), "[pb0][pb255]");
        assert_eq!(normalize("[pb][pb256]"), "[pb]");
        assert_eq!(
            normalize("[pb0,0,0][PB255,255,255]"),
            "[pb0,0,0][pb255,255,255]"
        );
        assert_eq!(normalize("[pb256,0,0][PBx]"), "");
    }

    #[test]
    fn parse_pb1() {
        let mut m = Parser::new("[pb0][PB1][pB255][pb256][pb]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::PageBackground(Some(Color::Legacy(0)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::PageBackground(Some(Color::Legacy(1)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::PageBackground(Some(Color::Legacy(255)))))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pb256".into())))
        );
        assert_eq!(m.next(), Some(Ok(Value::PageBackground(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pb2() {
        let mut m = Parser::new("[pb0,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pb0,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pb3() {
        let mut m = Parser::new("[pb50,150,200]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::PageBackground(Some(Color::Rgb(50, 150, 200)))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pb4() {
        let mut m = Parser::new("[pb0,0,255,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pb0,0,255,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pb5() {
        let mut m = Parser::new("[pb0,0.5,255]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pb0,0.5,255".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn norm_cf() {
        assert_eq!(normalize("[cf0][CF255]"), "[cf0][cf255]");
        assert_eq!(normalize("[cf][cf256]"), "[cf]");
        assert_eq!(
            normalize("[cf0,0,0][CF255,255,255]"),
            "[cf0,0,0][cf255,255,255]"
        );
        assert_eq!(normalize("[cf256,0,0][CFx]"), "");
    }

    #[test]
    fn parse_cf1() {
        let mut m = Parser::new("[cf0][CF1][cF255][cf256][cf]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorForeground(Some(Color::Legacy(0)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorForeground(Some(Color::Legacy(1)))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorForeground(Some(Color::Legacy(255)))))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cf256".into())))
        );
        assert_eq!(m.next(), Some(Ok(Value::ColorForeground(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cf2() {
        let mut m = Parser::new("[cf0,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cf0,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cf3() {
        let mut m = Parser::new("[cf255,0,208][CF0,a,0]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorForeground(Some(Color::Rgb(255, 0, 208)))))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("CF0,a,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cf4() {
        let mut m = Parser::new("[cf0,0,255,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cf0,0,255,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cf5() {
        let mut m = Parser::new("[cf0,0.5,255]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("cf0,0.5,255".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr() {
        let mut m = Parser::new("[cr1,1,10,10,0]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorRectangle(
                Rectangle::new(1, 1, 10, 10),
                Color::Legacy(0)
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr2() {
        let mut m = Parser::new("[CR1,0,10,10,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "CR1,0,10,10,0".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr3() {
        let mut m = Parser::new("[cR1,1,100,100,0,1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "cR1,1,100,100,0,1".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr4() {
        let mut m = Parser::new("[Cr5,7,100,80,100,150,200]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorRectangle(
                Rectangle::new(5, 7, 100, 80),
                Color::Rgb(100, 150, 200)
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr5() {
        let mut m = Parser::new("[cr1,1,100,100,0,1,2,3]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "cr1,1,100,100,0,1,2,3".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_cr6() {
        let mut m = Parser::new("[cr100,200,1000,2000,255,208,0]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ColorRectangle(
                Rectangle::new(100, 200, 1000, 2000),
                Color::Rgb(255, 208, 0)
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_f() {
        let mut m = Parser::new("[F]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("F".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_f1() {
        let mut m = Parser::new("[f1]");
        assert_eq!(m.next(), Some(Ok(Value::Field(1, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_f2() {
        let mut m = Parser::new("[f99]");
        assert_eq!(m.next(), Some(Ok(Value::Field(99, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_f3() {
        let mut m = Parser::new("[f100]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("f100".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_f4() {
        let mut m = Parser::new("[F4,1]");
        assert_eq!(m.next(), Some(Ok(Value::Field(4, Some(1)))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl() {
        let mut m = Parser::new("[flto]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Flash(FlashOrder::OnOff, None, None)))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl2() {
        let mut m = Parser::new("[FLOT]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Flash(FlashOrder::OffOn, None, None)))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl3() {
        let mut m = Parser::new("[Flt10o5]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Flash(FlashOrder::OnOff, Some(10), Some(5))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl4() {
        let mut m = Parser::new("[fLo0t99]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Flash(FlashOrder::OffOn, Some(0), Some(99))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl5() {
        let mut m = Parser::new("[flt10o5x]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("flt10o5x".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl6() {
        let mut m = Parser::new("[flt10o100]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("flt10o100".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fl7() {
        let mut m = Parser::new("[flt10o10o10]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("flt10o10o10".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fle() {
        let mut m = Parser::new("[/fl]");
        assert_eq!(m.next(), Some(Ok(Value::FlashEnd())));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fle1() {
        let mut m = Parser::new("[/fl1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("/fl1".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo() {
        let mut m = Parser::new("[fo]");
        assert_eq!(m.next(), Some(Ok(Value::Font(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo1() {
        let mut m = Parser::new("[fo1]");
        assert_eq!(m.next(), Some(Ok(Value::Font(Some((1, None))))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo2() {
        let mut m = Parser::new("[fO2,0000]");
        assert_eq!(m.next(), Some(Ok(Value::Font(Some((2, Some(0)))))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo3() {
        let mut m = Parser::new("[Fo3,FFFF]");
        assert_eq!(m.next(), Some(Ok(Value::Font(Some((3, Some(0xFFFF)))))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo4() {
        let mut m = Parser::new("[FO4,FFFFF]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("FO4,FFFFF".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo5() {
        let mut m = Parser::new("[fo5,xxxx]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("fo5,xxxx".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo6() {
        let mut m = Parser::new("[fo6,0000,0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("fo6,0000,0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo7() {
        let mut m = Parser::new("[Fo7,abcd]");
        assert_eq!(m.next(), Some(Ok(Value::Font(Some((7, Some(0xabcd)))))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_fo8() {
        let mut m = Parser::new("[fo0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("fo0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g() {
        let mut m = Parser::new("[G]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("G".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g1() {
        let mut m = Parser::new("[g1]");
        assert_eq!(m.next(), Some(Ok(Value::Graphic(1, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g2() {
        let mut m = Parser::new("[g2,1,1]");
        assert_eq!(m.next(), Some(Ok(Value::Graphic(2, Some((1, 1, None))))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g3() {
        let mut m = Parser::new("[g3,1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("g3,1".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g4() {
        let mut m = Parser::new("[g4,1,1,0123]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Graphic(4, Some((1, 1, Some(0x0123))))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g5() {
        let mut m = Parser::new("[g5,1,0,0123]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("g5,1,0,0123".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g6() {
        let mut m = Parser::new("[g6,300,300,12345]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "g6,300,300,12345".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g7() {
        let mut m = Parser::new("[g7,30,30,1245,]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "g7,30,30,1245,".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_g8() {
        let mut m = Parser::new("[G8,50,50,Beef]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::Graphic(8, Some((50, 50, Some(0xbeef))))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc() {
        let mut m = Parser::new("[hc]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("hc".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc1() {
        let mut m = Parser::new("[HC1]");
        assert_eq!(m.next(), Some(Ok(Value::HexadecimalCharacter(0x0001))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc2() {
        let mut m = Parser::new("[hcFFFF]");
        assert_eq!(m.next(), Some(Ok(Value::HexadecimalCharacter(0xFFFF))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc3() {
        let mut m = Parser::new("[hc1FFFF]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("hc1FFFF".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc4() {
        let mut m = Parser::new("[hcXXxx]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("hcXXxx".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_hc5() {
        let mut m = Parser::new("[hc7f]");
        assert_eq!(m.next(), Some(Ok(Value::HexadecimalCharacter(0x7F))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_jl() {
        let mut m = Parser::new("[jl]");
        assert_eq!(m.next(), Some(Ok(Value::JustificationLine(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_jl0() {
        let mut m = Parser::new("[JL0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("JL0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    #[allow(deprecated)]
    fn parse_jl15() {
        let mut m = Parser::new("[jL1][Jl2][JL3][jl4][JL5]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationLine(Some(JustificationLine::Other))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationLine(Some(JustificationLine::Left))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationLine(Some(
                JustificationLine::Center
            ))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationLine(Some(JustificationLine::Right))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationLine(Some(JustificationLine::Full))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_jp() {
        let mut m = Parser::new("[jp]");
        assert_eq!(m.next(), Some(Ok(Value::JustificationPage(None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_jp0() {
        let mut m = Parser::new("[JP0]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("JP0".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    #[allow(deprecated)]
    fn parse_jp14() {
        let mut m = Parser::new("[jP1][Jp2][JP3][jp4]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationPage(Some(JustificationPage::Other))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationPage(Some(JustificationPage::Top))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationPage(Some(
                JustificationPage::Middle
            ))))
        );
        assert_eq!(
            m.next(),
            Some(Ok(Value::JustificationPage(Some(
                JustificationPage::Bottom
            ))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_ms() {
        let mut m = Parser::new("[ms0]");
        assert_eq!(m.next(), Some(Ok(Value::ManufacturerSpecific(0, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_ms1() {
        let mut m = Parser::new("[Ms1,Test]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ManufacturerSpecific(1, Some("Test".into()))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_ms2() {
        let mut m = Parser::new("[Ms999,RANDOM junk]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ManufacturerSpecific(
                999,
                Some("RANDOM junk".into())
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_ms3() {
        let mut m = Parser::new("[Ms9x9]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("Ms9x9".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mse() {
        let mut m = Parser::new("[/ms0]");
        assert_eq!(m.next(), Some(Ok(Value::ManufacturerSpecificEnd(0, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mse1() {
        let mut m = Parser::new("[/Ms1,Test]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ManufacturerSpecificEnd(1, Some("Test".into()))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mse2() {
        let mut m = Parser::new("[/Ms999,RANDOM junk]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::ManufacturerSpecificEnd(
                999,
                Some("RANDOM junk".into())
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mse3() {
        let mut m = Parser::new("[/Ms9x9]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("/Ms9x9".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv() {
        let mut m = Parser::new("[mv]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mv".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv1() {
        let mut m = Parser::new("[mvc]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mvc".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv2() {
        let mut m = Parser::new("[mvcl]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mvcl".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv3() {
        let mut m = Parser::new("[mvcl100]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mvcl100".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv4() {
        let mut m = Parser::new("[mvcl100,1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mvcl100,1".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv5() {
        let mut m = Parser::new("[mvcl100,1,10]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("mvcl100,1,10".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv6() {
        let mut m = Parser::new("[mvcl100,1,10,Text]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::MovingText(
                MovingTextMode::Circular,
                MovingTextDirection::Left,
                100,
                1,
                10,
                "Text".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv7() {
        let mut m = Parser::new("[mvcr150,2,5,*MOVING*]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::MovingText(
                MovingTextMode::Circular,
                MovingTextDirection::Right,
                150,
                2,
                5,
                "*MOVING*".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv8() {
        let mut m = Parser::new("[mvll75,3,4,Linear]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::MovingText(
                MovingTextMode::Linear(0),
                MovingTextDirection::Left,
                75,
                3,
                4,
                "Linear".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv9() {
        let mut m = Parser::new("[mvlr1000,4,5,right]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::MovingText(
                MovingTextMode::Linear(0),
                MovingTextDirection::Right,
                1000,
                4,
                5,
                "right".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv10() {
        let mut m = Parser::new("[mvl2l100,5,1,left]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::MovingText(
                MovingTextMode::Linear(2),
                MovingTextDirection::Left,
                100,
                5,
                1,
                "left".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv11() {
        let mut m = Parser::new("[mvl4x100,5,1,left]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "mvl4x100,5,1,left".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_mv12() {
        let mut m = Parser::new("[mvl4r100,5,300,left]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "mvl4r100,5,300,left".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt() {
        let mut m = Parser::new("[pt]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(None, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt1() {
        let mut m = Parser::new("[pt10]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(Some(10), None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt2() {
        let mut m = Parser::new("[pt10o]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(Some(10), None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt3() {
        let mut m = Parser::new("[pt10o2]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(Some(10), Some(2)))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt4() {
        let mut m = Parser::new("[pt10o2o]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pt10o2o".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt5() {
        let mut m = Parser::new("[pt255O255]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(Some(255), Some(255)))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt6() {
        let mut m = Parser::new("[PTO]");
        assert_eq!(m.next(), Some(Ok(Value::PageTime(None, None))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt7() {
        let mut m = Parser::new("[pt256o256]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pt256o256".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_pt8() {
        let mut m = Parser::new("[pt%%%]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("pt%%%".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sc() {
        let mut m = Parser::new("[sc]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("sc".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sc1() {
        let mut m = Parser::new("[SC1]");
        assert_eq!(m.next(), Some(Ok(Value::SpacingCharacter(1))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sc2() {
        let mut m = Parser::new("[Sc99]");
        assert_eq!(m.next(), Some(Ok(Value::SpacingCharacter(99))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sc3() {
        let mut m = Parser::new("[sc100]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("sc100".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sc4() {
        let mut m = Parser::new("[sc2,1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("sc2,1".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sce() {
        let mut m = Parser::new("[/sc]");
        assert_eq!(m.next(), Some(Ok(Value::SpacingCharacterEnd())));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_sce1() {
        let mut m = Parser::new("[/sc1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("/sc1".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr() {
        let mut m = Parser::new("[tr1,1,10,10]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::TextRectangle(Rectangle::new(1, 1, 10, 10))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr2() {
        let mut m = Parser::new("[TR1,0,10,10]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("TR1,0,10,10".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr3() {
        let mut m = Parser::new("[tR1,1,100,100,1]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue(
                "tR1,1,100,100,1".into()
            )))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr4() {
        let mut m = Parser::new("[Tr5,7,100,80]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::TextRectangle(Rectangle::new(5, 7, 100, 80))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr5() {
        let mut m = Parser::new("[tr1,1,,100]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("tr1,1,,100".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tr6() {
        let mut m = Parser::new("[tr1,1,0,0]");
        assert_eq!(
            m.next(),
            Some(Ok(Value::TextRectangle(Rectangle::new(1, 1, 0, 0))))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_new_line() {
        let mut m = Parser::new("[nl][NL0][Nl1][nL9][nl10]");
        assert_eq!(m.next(), Some(Ok(Value::NewLine(None))));
        assert_eq!(m.next(), Some(Ok(Value::NewLine(Some(0)))));
        assert_eq!(m.next(), Some(Ok(Value::NewLine(Some(1)))));
        assert_eq!(m.next(), Some(Ok(Value::NewLine(Some(9)))));
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTagValue("nl10".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_multi() {
        let mut m = Parser::new("[[TEST[nl]TEST 2[np]TEST 3XX[NL]TEST 4]]");
        assert_eq!(m.next(), Some(Ok(Value::Text("[".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("TEST".into()))));
        assert_eq!(m.next(), Some(Ok(Value::NewLine(None))));
        assert_eq!(m.next(), Some(Ok(Value::Text("TEST 2".into()))));
        assert_eq!(m.next(), Some(Ok(Value::NewPage())));
        assert_eq!(m.next(), Some(Ok(Value::Text("TEST 3XX".into()))));
        assert_eq!(m.next(), Some(Ok(Value::NewLine(None))));
        assert_eq!(m.next(), Some(Ok(Value::Text("TEST 4".into()))));
        assert_eq!(m.next(), Some(Ok(Value::Text("]".into()))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_control_char() {
        let mut m = Parser::new("\n");
        assert_eq!(m.next(), Some(Err(SyntaxError::CharacterNotDefined('\n'))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_rustacean() {
        let mut m = Parser::new("🦀🦀");
        assert_eq!(m.next(), Some(Err(SyntaxError::CharacterNotDefined('🦀'))));
        assert_eq!(m.next(), Some(Err(SyntaxError::CharacterNotDefined('🦀'))));
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag() {
        let mut m = Parser::new("[x[x]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("x[x".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag2() {
        let mut m = Parser::new("]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("]".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag3() {
        let mut m = Parser::new("[nl");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("[nl".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag4() {
        let mut m = Parser::new("[");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("[".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag5() {
        let mut m = Parser::new("[x]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("x".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag6() {
        let mut m = Parser::new("bad]");
        assert_eq!(m.next(), Some(Ok(Value::Text("bad"))));
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("]".into())))
        );
    }

    #[test]
    fn parse_tag7() {
        let mut m = Parser::new("[ttS123][vsa][slow45,10][feedL123][tz1,2,3]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("ttS123".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("vsa".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("slow45,10".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("feedL123".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("tz1,2,3".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn parse_tag8() {
        let mut m = Parser::new("[pa1,LOW,CLOSED][loca,b,c,d]");
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("pa1,LOW,CLOSED".into())))
        );
        assert_eq!(
            m.next(),
            Some(Err(SyntaxError::UnsupportedTag("loca,b,c,d".into())))
        );
        assert_eq!(m.next(), None);
    }

    #[test]
    fn norm() {
        assert_eq!(normalize("01234567890"), "01234567890");
        assert_eq!(normalize("ABC"), "ABC");
        assert_eq!(normalize("ABC_DEF"), "ABC_DEF");
        assert_eq!(normalize("abc"), "abc");
        assert_eq!(normalize("DON'T"), "DON'T");
        assert_eq!(normalize("SPACE SPACE"), "SPACE SPACE");
        assert_eq!(normalize("AB|C"), "AB|C");
        assert_eq!(normalize("AB|{}{}C{}"), "AB|{}{}C{}");
        assert_eq!(normalize("!\"#$%&\'()*+,-./"), "!\"#$%&\'()*+,-./");
        assert_eq!(normalize(":;<=>?@\\^_`{|}~"), ":;<=>?@\\^_`{|}~");
        assert_eq!(normalize("[["), "[[");
        assert_eq!(normalize("]]"), "]]");
        assert_eq!(normalize("[[NOT TAG]]"), "[[NOT TAG]]");
        assert_eq!(normalize("\t\n\rTAIL"), "TAIL");
    }

    #[test]
    fn norm_2() {
        assert_eq!(normalize("ABC[NL]DEF"), "ABC[nl]DEF");
        assert_eq!(normalize("ABC[nl3]DEF"), "ABC[nl3]DEF");
        assert_eq!(normalize("ABC[np]DEF"), "ABC[np]DEF");
        assert_eq!(normalize("ABC[jl4]DEF"), "ABC[jl4]DEF");
        assert_eq!(normalize("ABC[jl6]DEF"), "ABCDEF");
        assert_eq!(normalize("ABC[jp4]DEF"), "ABC[jp4]DEF");
        assert_eq!(normalize("[fo3]ABC DEF"), "[fo3]ABC DEF");
        assert_eq!(normalize("[fo3,beef]ABC DEF"), "[fo3,beef]ABC DEF");
        assert_eq!(normalize("[g1]"), "[g1]");
        assert_eq!(normalize("[g1_]"), "");
        assert_eq!(normalize("[g1,5,5]"), "[g1,5,5]");
        assert_eq!(normalize("[g1,5,5,beef]"), "[g1,5,5,beef]");
        assert_eq!(normalize("[g1,4,4,BEEF]"), "[g1,4,4,beef]");
        assert_eq!(normalize("[cf255,255,255]"), "[cf255,255,255]");
        assert_eq!(normalize("[cf0,255,255]"), "[cf0,255,255]");
        assert_eq!(normalize("[cf0,255,0]"), "[cf0,255,0]");
        assert_eq!(normalize("[pto]"), "[pto]");
        assert_eq!(normalize("[pt10o]"), "[pt10o]");
        assert_eq!(normalize("[pt10o5]"), "[pt10o5]");
        assert_eq!(normalize("[pto5]"), "[pto5]");
        assert_eq!(normalize("ABC[sc3]DEF"), "ABC[sc3]DEF");
        assert_eq!(normalize("ABC[sc3]DEF[/sc]GHI"), "ABC[sc3]DEF[/sc]GHI");
        assert_eq!(normalize("[tr1,1,40,20]"), "[tr1,1,40,20]");
        assert_eq!(normalize("[tr1,1,0,0]"), "[tr1,1,0,0]");
        assert_eq!(normalize("[pb0,128,255]"), "[pb0,128,255]");
    }

    #[test]
    fn norm_3() {
        assert_eq!(normalize("["), "");
        assert_eq!(normalize("]"), "");
        assert_eq!(normalize("[bad tag"), "");
        assert_eq!(normalize("bad tag]"), "bad tag");
        assert_eq!(normalize("bad[tag"), "bad");
        assert_eq!(normalize("bad]tag"), "badtag");
        assert_eq!(normalize("bad[ [nl] tag"), "bad tag");
        assert_eq!(normalize("bad ]tag [nl]"), "bad tag [nl]");
        assert_eq!(normalize("[ttS123]"), "");
    }

    #[test]
    fn blank() {
        assert!(is_blank(""));
        assert!(is_blank(" "));
        assert!(is_blank("[nl]"));
        assert!(is_blank("[np]"));
        assert!(is_blank("[pt1o1]"));
        assert!(is_blank("[jp2]"));
        assert!(is_blank("[jl3]"));
        assert!(is_blank("[fo2]"));
        assert!(is_blank("[sc2]"));
        assert!(is_blank("[tr1,1,20,20]"));
        assert!(is_blank("[pb0,1,2]"));
    }

    #[test]
    fn not_blank() {
        assert!(!is_blank("A"));
        assert!(!is_blank("[cr255,0,0]"));
        assert!(!is_blank("[f1]"));
        assert!(!is_blank("[g1]"));
        assert!(!is_blank("[hc41]"));
        assert!(!is_blank("[ms1]"));
        assert!(!is_blank("[/ms1]"));
        assert!(!is_blank("[mvcl100,1,10,Text]"));
    }

    #[test]
    fn text() {
        assert_eq!(join_text("ABC[nl]DEF", " "), "ABC DEF");
        assert_eq!(join_text("ABC[np]DEF", " "), "ABC DEF");
        assert_eq!(join_text("ABC[jl4]DEF", " "), "ABC DEF");
        assert_eq!(join_text("[fo3]ABC DEF", " "), "ABC DEF");
        assert_eq!(join_text("[cf255,255,255]ABC", " "), "ABC");
        assert_eq!(join_text("ABC[sc2]DEF", " "), "ABC DEF");
        assert_eq!(join_text("DEF[tr1,1,40,20]ABC", " "), "DEF ABC");
        assert_eq!(join_text("ABC[x]DEF", "_"), "ABC_DEF");
    }

    #[test]
    fn rectangles() {
        let r1 = Rectangle::new(1, 1, 2, 2);
        let r2 = Rectangle::new(2, 2, 2, 2);
        assert_eq!(r1.intersection(r2), Rectangle::new(2, 2, 1, 1));
        let r3 = Rectangle::new(3, 3, 2, 2);
        assert_eq!(r1.intersection(r3), Rectangle::default());
    }
}
