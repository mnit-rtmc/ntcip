// ntcip::dms
//
// Copyright (C) 2019-2020  Minnesota Department of Transportation
//
//! Dynamic message signs specified by NTCIP 1203.
mod base64;
mod font;
mod graphic;
pub mod multi;
mod render;

/// Result type
pub type Result<T> = std::result::Result<T, multi::SyntaxError>;

pub use font::{Character, Font, FontCache};
pub use graphic::{Graphic, GraphicCache};
pub use render::{PageBuilder, Pages};
