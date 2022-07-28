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

mod color {
    pub const BLACK: u32 = 0x0000_0000;
    pub const WHITE: u32 = 0x00FF_FFFF;
}

type Bucket = [[bool; 10]; 20];
const SIZE: i32 = 30;

fn update_impl(mut c: Canvas, input: &Input) {
    clear(&mut c);

    let cells = [[true; 10]; 20];
    draw_bucket(&mut c, &cells);

    let mouse = &input.mouse;
    draw_cell(&mut c, mouse.x, mouse.y);
}

fn draw_bucket(c: &mut Canvas, bucket: &Bucket) {
    let bucket_width = bucket[0].len().num_cast::<i32>() * SIZE;
    let bucket_height = bucket.len().num_cast::<i32>() * SIZE;

    let x0 = c.width() / 2 - bucket_width / 2;
    let y0 = c.height() / 2 - bucket_height / 2;

    for (y, line) in bucket.iter().enumerate() {
        for (x, cell) in line.iter().enumerate() {
            if *cell {
                draw_cell(c, x0 + x.num_cast::<i32>() * SIZE, y0 + y.num_cast::<i32>() * SIZE);
            }
        }
    }
}

fn draw_cell(c: &mut Canvas, x0: i32, y0: i32) {
    const COLOR_SWITCH: [i32; 2] = [SIZE - 2, SIZE - 8];

    fn current(foreground: bool) -> u32 {
        if foreground {
            color::WHITE
        } else {
            color::BLACK
        }
    }

    let mut foreground = false;
    fill_square(c, current(foreground), x0, y0, x0 + SIZE, y0 + SIZE);

    for n in COLOR_SWITCH {
        foreground = !foreground;
        fill_square(c, current(foreground), x0 + SIZE - n, y0 + SIZE - n, x0 + n, y0 + n);
    }
}

fn fill_square(c: &mut Canvas, color: u32, x0: i32, y0: i32, x1: i32, y1: i32) {
    for y in y0..y1 {
        for x in x0..x1 {
            if let Some(pixel) = c.get_mut(x, y) {
                *pixel = color;
            }
        }
    }
}

fn clear(c: &mut Canvas) {
    let width = c.width();
    let height = c.height();
    fill_square(c, color::BLACK, 0, 0, width, height);
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