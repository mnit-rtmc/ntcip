/// Graphic example
mod bmp;
use fstr::FStr;
use ntcip::dms::multi::ColorScheme;
use ntcip::dms::Graphic;

fn main() {
    let bitmap = vec![
        0x04, 0x00, 0x80, 0x10, 0x42, 0x14, 0x44, 0x49, 0x05, 0x40, 0x70, 0x04,
        0x00,
    ];
    let graphic = Graphic {
        number: 3,
        name: FStr::from_str_lossy("arrow", b'\0'),
        height: 9,
        width: 11,
        gtype: ColorScheme::Monochrome1Bit,
        transparent_color: None,
        bitmap,
    };
    let raster = graphic.to_raster();
    bmp::write(&raster, &format!("graphic.bmp")).unwrap();
}
