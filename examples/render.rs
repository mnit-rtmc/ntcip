/// Example DMS rendering
mod bmp;
use ntcip::dms::config::{MultiCfg, VmsCfg};
use ntcip::dms::multi::{ColorScheme, JustificationLine, JustificationPage};
use ntcip::dms::{ifnt, Dms, FontTable, Pages};

fn font_table() -> FontTable<1> {
    let mut fonts = FontTable::default();
    let buf = include_bytes!("../test/F08.ifnt");
    let f = fonts.lookup_mut(0).unwrap();
    *f = ifnt::read(&buf[..]).unwrap();
    fonts
}

fn make_dms() -> Dms<1, 0> {
    Dms::builder()
        .with_vms_cfg(VmsCfg {
            char_height_pixels: 0,
            char_width_pixels: 0,
            sign_height_pixels: 28,
            sign_width_pixels: 140,
            ..Default::default()
        })
        .with_font_definition(font_table())
        .with_multi_cfg(MultiCfg {
            default_justification_line: JustificationLine::Left,
            default_justification_page: JustificationPage::Top,
            default_font: 8,
            color_scheme: ColorScheme::Color24Bit,
            ..Default::default()
        })
        .build()
        .unwrap()
}

const MULTI: &str = "[cf1]LEFT[jl3][cf2]MIDDLE[jl4][cf3]RIGHT[nl][cf4][jl2]THE[cf5][jl3]CENTER[cf6][jl4]LINE[nl][jl3][cf7]THE BOTTOM LINE[np][cf8][jp3][jl3]SECOND PAGE[np][cr1,1,20,8,255,128,128][cr121,1,20,8,128,255,128][cr1,21,20,8,128,128,255][cr121,21,20,8,128,128,128][pb32,0,64][pt40o10]PAGE 3";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let dms = make_dms();
    let pages = Pages::new(&dms, MULTI);
    for (i, page) in pages.enumerate() {
        let raster = page?.raster;
        bmp::write(&raster, &format!("render{i}.bmp"))?;
    }
    Ok(())
}
