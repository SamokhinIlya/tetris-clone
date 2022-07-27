pub fn update(raw_canvas: &mut impl RawCanvas, input: &Input) {
    let width = raw_canvas.width();
    let height = raw_canvas.height();

    for y in 0..height {
        for x in 0..width {
            let pixel = &mut raw_canvas[(y*width + x) as usize];
            *pixel = (x as u32) << 8 | (y as u32);
        }
    }

    let mouse = &input.mouse;
    for y in mouse.y..(mouse.y + 4) {
        for x in mouse.x..(mouse.x + 4) {
            let pixel = &mut raw_canvas[(y as usize)*width + (x as usize)];
            *pixel = 0x00FFFFFF;
        }
    }
}

pub trait RawCanvas: std::ops::IndexMut<usize, Output=u32> {
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}

#[derive(Default)]
pub struct Input {
    pub mouse: Mouse,
}

#[derive(Default, Debug)]
pub struct Mouse {
    pub x: i32,
    pub y: i32,
    pub left: bool,
    pub right: bool,
}