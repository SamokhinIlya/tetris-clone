use crate::game::RawCanvas;
use crate::num_cast::NumCast;

use super::bucket::Cell;
use super::grid::Grid;

pub fn grid<const N: usize, const M: usize>(
    canvas: &mut Canvas,
    bucket: &Grid<Cell, N, M>,
    cell_px: i32,
    [y0, x0]: [i32; 2],
    show_disappearing: bool,
) {
    let bucket_width_px = bucket.width().num_cast::<i32>() * cell_px;
    let bucket_height_px = bucket.height().num_cast::<i32>() * cell_px;

    let [y1, x1] = [y0 + bucket_height_px, x0 + bucket_width_px];

    for y in y0..y1 {
        for x in x0..x1 {
            if y == y0 || y == (y1 - 1) || x == x0 || x == (x1 - 1) {
                if let Some(pixel) = canvas.get_mut(x, y) {
                    *pixel = color::WHITE;
                }
            }
        }
    }

    for (y, row) in bucket.rows().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if *cell == Cell::Falling || *cell == Cell::Frozen || (*cell == Cell::Disappearing && show_disappearing) {
                let color = if *cell == Cell::Falling { color::BLUE } else { color::WHITE };
                self::cell(canvas, cell_px, [y0 + y.num_cast::<i32>() * cell_px, x0 + x.num_cast::<i32>() * cell_px], color);
            }
        }
    }
}

pub fn cell(canvas: &mut Canvas, cell_px: i32, [y0, x0]: [i32; 2], color: u32) {
    let color_switch = [cell_px - 2, cell_px - 8];

    fn current(foreground: bool, color: u32) -> u32 {
        if foreground {
            color
        } else {
            color::BLACK
        }
    }

    let mut foreground = false;
    fill_square(canvas, current(foreground, color), [y0, x0], [y0 + cell_px, x0 + cell_px]);

    for n in color_switch {
        foreground = !foreground;
        fill_square(canvas, current(foreground, color), [y0 + cell_px - n, x0 + cell_px - n], [y0 + n, x0 + n]);
    }
}

pub mod color {
    pub const BLACK: u32 = 0x0000_0000;
    pub const WHITE: u32 = 0x00FF_FFFF;
    pub const GREEN: u32 = 0x0000_FF00;
    pub const BLUE: u32 = 0x0000_00FF;
}

fn fill_square(canvas: &mut Canvas, color: u32, [y0, x0]: [i32; 2], [y1, x1]: [i32; 2]) {
    for y in y0..y1 {
        for x in x0..x1 {
            if let Some(pixel) = canvas.get_mut(x, y) {
                *pixel = color;
            }
        }
    }
}

pub fn clear(canvas: &mut Canvas) {
    let width = canvas.width();
    let height = canvas.height();
    fill_square(canvas, color::BLACK, [0, 0], [height, width]);
}

pub struct Canvas<'a> {
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