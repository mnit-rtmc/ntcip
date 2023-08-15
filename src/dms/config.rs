// config.rs
//
// Copyright (C) 2018-2023  Minnesota Department of Transportation
//
//! Configuration information
use crate::dms::multi::{
    Color, ColorClassic, ColorScheme, JustificationLine, JustificationPage,
};

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

/// Sign configuration
#[derive(Clone)]
pub struct SignCfg {
    pub sign_access: u8,
    pub sign_type: DmsSignType,
    pub sign_height: u16,
    pub sign_width: u16,
    pub horizontal_border: u16,
    pub vertical_border: u16,
    pub legend: DmsLegend,
    pub beacon_type: DmsBeaconType,
    pub sign_technology: u16,
}

/// VMS configuration
#[derive(Clone)]
pub struct VmsCfg {
    pub char_height_pixels: u8,
    pub char_width_pixels: u8,
    pub sign_height_pixels: u16,
    pub sign_width_pixels: u16,
    pub horizontal_pitch: u8,
    pub vertical_pitch: u8,
    pub monochrome_color: [u8; 6],
}

/// MULTI configuration
#[derive(Clone)]
pub struct MultiCfg {
    /// Serves as dmsDefaultBackgroundRGB and dmsDefaultBackgroundColor
    pub default_background_rgb: Color,
    /// Serves as dmsDefaultForegroundRGB and dmsDefaultForegroundColor
    pub default_foreground_rgb: Color,
    pub default_flash_on: u8,
    pub default_flash_off: u8,
    pub default_font: u8,
    pub default_justification_line: JustificationLine,
    pub default_justification_page: JustificationPage,
    pub default_page_on_time: u8,
    pub default_page_off_time: u8,
    pub default_character_set: u8,
    pub color_scheme: ColorScheme,
    pub supported_multi_tags: [u8; 4],
    pub max_number_pages: u8,
    pub max_multi_string_length: u16,
}

impl Default for SignCfg {
    fn default() -> Self {
        SignCfg {
            sign_access: 0,
            sign_type: DmsSignType::VmsChar,
            sign_height: 1800,
            sign_width: 3500,
            horizontal_border: 80,
            vertical_border: 80,
            legend: DmsLegend::NoLegend,
            beacon_type: DmsBeaconType::None,
            sign_technology: 1,
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
            default_character_set: 2,
            color_scheme: ColorScheme::Monochrome1Bit,
            supported_multi_tags: [0, 0, 0, 0],
            max_number_pages: 4,
            max_multi_string_length: 65535,
        }
    }
}
