/// Example rendering to a .gif file
use gift::{Encoder, Step};
use ntcip::dms::multi::{
    ColorClassic, ColorCtx, ColorScheme, JustificationLine, JustificationPage,
};
use ntcip::dms::{Font, FontCache, Pages, Result};
use pix::{gray::Gray8, rgb::SRgb8, Palette, Raster};
use std::fs::File;
use std::io::BufWriter;

fn font_cache() -> FontCache {
    let mut fonts = FontCache::default();
    let fts: Vec<Font> =
        serde_json::from_str(include_str!("../test/font.json")).unwrap();
    for font in fts {
        fonts.insert(font);
    }
    fonts
}

fn render_full(multi: &str) -> Result<Vec<(Raster<SRgb8>, u16)>> {
    let fonts = font_cache();
    Pages::builder(140, 28)
        .with_color_ctx(ColorCtx::new(
            ColorScheme::Color24Bit,
            ColorClassic::White.rgb(),
            ColorClassic::Black.rgb(),
        ))
        .with_page_on_time_ds(20)
        .with_justification_page(JustificationPage::Top)
        .with_justification_line(JustificationLine::Left)
        .with_font_num(3)
        .with_fonts(Some(&fonts))
        .build(multi)
        .collect()
}

fn make_indexed(page: Raster<SRgb8>) -> (Raster<Gray8>, Palette) {
    let mut palette = Palette::new(256);
    let mut indexed = Raster::with_clear(page.width(), page.height());
    for (dst, src) in indexed.pixels_mut().iter_mut().zip(page.pixels()) {
        if let Some(d) = palette.set_entry(*src) {
            *dst = Gray8::new(d as u8);
        } else {
            panic!("Palette full!");
        }
    }
    (indexed, palette)
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let mut writer = BufWriter::new(File::create("pages.gif")?);
    let mut enc = Encoder::new(&mut writer).into_step_enc().with_loop_count(0);
    let pages = render_full("[cf1]LEFT[jl3][cf2]MIDDLE[jl4][cf3]RIGHT[nl][cf4][jl2]THE[cf5][jl3]CENTER[cf6][jl4]LINE[nl][jl3][cf7]THE BOTTOM LINE[np][cf8][jp3][jl3]SECOND PAGE[np][cr1,1,20,8,255,128,128][cr121,1,20,8,128,255,128][cr1,21,20,8,128,128,255][cr121,21,20,8,128,128,128][pb32,0,64][pt40o10]PAGE 3")?;
    for (page, delay_ds) in pages {
        let (pg, palette) = make_indexed(page);
        let delay = delay_ds * 10;
        let step =
            Step::with_indexed(pg, palette).with_delay_time_cs(Some(delay));
        enc.encode_step(&step)?;
    }
    Ok(())
}
