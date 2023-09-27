// ntcip::dms
//
// Copyright (C) 2019-2023  Minnesota Department of Transportation
//
//! 1203 \([PDF]\) — Dynamic message signs
//!
//! [PDF]: https://www.ntcip.org/file/2018/11/NTCIP1203v03f.pdf
pub mod config;
pub mod font;
pub mod graphic;
pub mod multi;
mod pattern;
mod render;
mod sign;

/// Result type
pub type Result<T> = std::result::Result<T, multi::SyntaxError>;

pub use render::{Page, Pages};
pub use sign::{Dms, DmsBuilder};
