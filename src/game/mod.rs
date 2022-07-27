use std::ops::IndexMut;
use std::convert::{TryInto};
use crate::num_cast::NumCast;

pub fn update(raw_canvas: &mut dyn RawCanvas, input: &Input) {
    update_impl(Canvas::from_raw(raw_canvas), input)
}

pub trait RawCanvas: IndexMut<usize, Output=u32> {
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

fn update_impl(mut c: Canvas, input: &Input) {
    for y in 0..c.height() {
        for x in 0..c.width() {
            if let Some(pixel) = c.get_mut(x, y) {
                *pixel = x.num_cast::<u32>() << 8 | y.num_cast::<u32>();
            }
        }
    }

    let mouse = &input.mouse;
    for y in mouse.y..(mouse.y + 4) {
        for x in mouse.x..(mouse.x + 4) {
            if let Some(pixel) = c.get_mut(x, y) {
                *pixel = 0x00FF_FFFF;
            }
        }
    }
}

struct Canvas<'a> {
    inner: &'a mut dyn RawCanvas,
}

impl<'a> Canvas<'a> {
    pub fn from_raw(raw_canvas: &'a mut dyn RawCanvas) -> Self {
        let _: i32 = raw_canvas.width().try_into().expect("failed to convert RawCanvas::width() to i32");
        let _: i32 = raw_canvas.height().try_into().expect("failed to convert RawCanvas::height() to i32");
        Self {
            inner: raw_canvas,
        }
    }

    pub fn width(&self) -> i32 {
        self.inner.width().num_cast()
    }

    pub fn height(&self) -> i32 {
        self.inner.height().num_cast()
    }

    pub fn get_mut(&mut self, x: i32, y: i32) -> Option<&mut u32> {
        let width = self.width();
        let height = self.height();

        if x > 0 && x < width && y > 0 && y < height {
            Some(&mut self.inner[(y*width + x).num_cast()])
        } else {
            None
        }
    }
}