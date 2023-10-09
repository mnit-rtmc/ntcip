// config.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Configuration information
use crate::dms::multi::{
    Color, ColorClassic, ColorScheme, JustificationLine, JustificationPage, Tag,
};
use enumflags2::BitFlags;

/// Configuration error
#[derive(Debug, thiserror::Error)]
pub enum CfgError {
    #[error("Invalid page config")]
    InvalidPageCfg,
}

/// Sign access
#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmsSignAccess {
    Other = 1 << 0,
    WalkIn = 1 << 1,
    Rear = 1 << 2,
    Front = 1 << 3,
}

/// Sign type
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmsSignType {
    Other = 1,
    Bos,
    Cms,
    VmsChar,
    VmsLine,
    VmsFull,
    PortableOther = 129,
    PortableBos,
    PortableCms,
    PortableVmsChar,
    PortableVmsLine,
    PortableVmsFull,
}

/// DMS Legend
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmsLegend {
    #[deprecated]
    Other = 1,
    NoLegend,
    LegendExists,
}

/// Beacon type
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmsBeaconType {
    Other = 1,
    None,
    OneBeacon,
    TwoBeaconSyncFlash,
    TwoBeaconsOppFlash,
    FourBeaconSyncFlash,
    FourBeaconAltRowFlash,
    FourBeaconAltColumnFlash,
    FourBeaconAltDiagonalFlash,
    FourBeaconNoSyncFlash,
    OneBeaconStrobe,
    TwoBeaconStrobe,
    FourBeaconStrobe,
}

/// Sign technology
#[enumflags2::bitflags]
#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmsSignTechnology {
    Other = 1 << 0,
    Led = 1 << 1,
    FlipDisk = 1 << 2,
    FiberOptics = 1 << 3,
    Shuttered = 1 << 4,
    Bulb = 1 << 5,
    Drum = 1 << 6,
}

/// Character set for MULTI messages
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CharacterSet {
    Other = 1,
    EightBit,
}

/// Sign configuration — `dmsSignCfg`
///
/// Configuration common to all signs.
#[derive(Clone)]
pub struct SignCfg {
    /// Access bits — `dmsSignAccess`
    pub sign_access: BitFlags<DmsSignAccess>,
    /// Sign type — `dmsSignType`
    pub sign_type: DmsSignType,
    /// Height of sign face (mm) — `dmsSignHeight`
    pub sign_height: u16,
    /// Width of sign face (mm) — `dmsSignWidth`
    pub sign_width: u16,
    /// Horizontal border (mm) — `dmsHorizontalBorder`
    pub horizontal_border: u16,
    /// Vertical border (mm) — `dmsVerticalBorder`
    pub vertical_border: u16,
    /// Sign legend — `dmsLegend`
    pub legend: DmsLegend,
    /// Beacon type — `dmsBeaconType`
    pub beacon_type: DmsBeaconType,
    /// Sign technology — `dmsSignTechnology`
    pub sign_technology: BitFlags<DmsSignTechnology>,
}

/// VMS configuration — `vmsCfg`
///
/// Configuration for variable message signs.
#[derive(Clone)]
pub struct VmsCfg {
    /// Character height in pixels — `vmsCharacterHeightPixels`
    pub char_height_pixels: u8,
    /// Character width in pixels — `vmsCharacterWidthPixels`
    pub char_width_pixels: u8,
    /// Sign height in pixels — `vmsSignHeightPixels`
    pub sign_height_pixels: u16,
    /// Sign width in pixels — `vmsSignWidthPixels`
    pub sign_width_pixels: u16,
    /// Horizontal pixel pitch (mm) — `vmsHorizontalPitch`
    pub horizontal_pitch: u8,
    /// Vertical pixel pitch (mm) — `vmsVerticalPitch`
    pub vertical_pitch: u8,
    /// Monochrome sign colors — `monochromeColor`
    pub monochrome_color: [u8; 6],
}

/// MULTI configuration — `multiCfg`
///
/// Default values for MarkUp Language for Transportation Information.
#[derive(Clone)]
pub struct MultiCfg {
    /// Serves as `dmsDefaultBackgroundRGB` and `dmsDefaultBackgroundColor`
    pub default_background_rgb: Color,
    /// Serves as `dmsDefaultForegroundRGB` and `dmsDefaultForegroundColor`
    pub default_foreground_rgb: Color,
    /// Default flash on time (1/10 s) — `defaultFlashOn`
    pub default_flash_on: u8,
    /// Default flash off time (1/10 s) — `defaultFlashOff`
    pub default_flash_off: u8,
    /// Default font number — `defaultFont`
    pub default_font: u8,
    /// Default line justification — `defaultJustificationLine`
    pub default_justification_line: JustificationLine,
    /// Default page justification — `defaultJustificationPage`
    pub default_justification_page: JustificationPage,
    /// Default page on time (1/10 s) — `defaultPageOnTime`
    pub default_page_on_time: u8,
    /// Default page off time (1/10 s) — `defaultPageOffTime`
    pub default_page_off_time: u8,
    /// Default character set — `defaultCharacterSet`
    pub default_character_set: CharacterSet,
    /// Color scheme — `dmsColorScheme`
    pub color_scheme: ColorScheme,
    /// Supported MULTI tags — `dmsSupportedMultiTags`
    pub supported_multi_tags: BitFlags<Tag>,
    /// Maximum number of pages — `dmsMaxNumberPages`
    pub max_number_pages: u8,
    /// Maximum MULTI string length — `dmsMaxMultiStringLength`
    pub max_multi_string_length: u16,
}

impl Default for SignCfg {
    fn default() -> Self {
        SignCfg {
            sign_access: BitFlags::empty(),
            sign_type: DmsSignType::VmsChar,
            sign_height: 1800,
            sign_width: 3500,
            horizontal_border: 80,
            vertical_border: 80,
            legend: DmsLegend::NoLegend,
            beacon_type: DmsBeaconType::None,
            sign_technology: DmsSignTechnology::Led.into(),
        }
    }
}

impl Default for VmsCfg {
    fn default() -> Self {
        VmsCfg {
            char_height_pixels: 7,
            char_width_pixels: 5,
            sign_height_pixels: 21,
            sign_width_pixels: 40,
            horizontal_pitch: 66,
            vertical_pitch: 66,
            monochrome_color: [0xFF, 0x70, 0xE0, 0, 0, 0],
        }
    }
}

impl Default for MultiCfg {
    fn default() -> Self {
        // Unsupported tags: Fl, Ms, Mv, F1-F13
        let supported_multi_tags = Tag::Cb
            | Tag::Cf
            | Tag::Fo
            | Tag::G
            | Tag::Hc
            | Tag::Jl
            | Tag::Jp
            | Tag::Nl
            | Tag::Np
            | Tag::Pt
            | Tag::Sc
            | Tag::Tr
            | Tag::Cr
            | Tag::Pb;
        MultiCfg {
            default_background_rgb: Color::Legacy(ColorClassic::Black.into()),
            default_foreground_rgb: Color::Legacy(ColorClassic::Amber.into()),
            default_flash_on: 5,
            default_flash_off: 5,
            default_font: 1,
            default_justification_line: JustificationLine::Center,
            default_justification_page: JustificationPage::Middle,
            default_page_on_time: 30,
            default_page_off_time: 0,
            default_character_set: CharacterSet::EightBit,
            color_scheme: ColorScheme::Monochrome1Bit,
            supported_multi_tags,
            max_number_pages: 4,
            max_multi_string_length: 65535,
        }
    }
}

impl MultiCfg {
    /// Validate the MULTI configuration
    pub fn validate(&self) -> Result<(), CfgError> {
        if self.max_number_pages == 0
            || self.max_number_pages > 1
                && !self.supported_multi_tags.contains(Tag::Np)
        {
            return Err(CfgError::InvalidPageCfg);
        }
        Ok(())
    }
}
