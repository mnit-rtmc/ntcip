// ntcip::dms
//
// Copyright (C) 2019-2023  Minnesota Department of Transportation
//
//! NTCIP [1203] \(PDF\) — Dynamic message signs
//!
//! [1203]: https://www.ntcip.org/file/2018/11/NTCIP1203v03f.pdf
pub mod config;
pub mod font;
pub mod graphic;
pub mod multi;
mod render;
mod sign;

/// Result type
pub type Result<T> = std::result::Result<T, multi::SyntaxError>;

pub use render::Page;
pub use sign::{Dms, DmsBuilder};
