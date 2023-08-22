// ntcip::dms
//
// Copyright (C) 2019-2023  Minnesota Department of Transportation
//
//! Dynamic message signs; NTCIP 1203
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
