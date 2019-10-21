// ntcip::dms
//
// Copyright (C) 2019  Minnesota Department of Transportation
//
//! DMS module for NTCIP 1203 dynamic message signs
//!
mod base64;
mod font;
mod graphic;
pub mod multi;
mod render;

/// Result type
pub type Result<T> = std::result::Result<T, multi::SyntaxError>;

pub use font::{Character, Font, FontCache};
pub use graphic::{Graphic, GraphicCache};
pub use render::{PageSplitter, State};
