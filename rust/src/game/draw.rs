use std::iter::zip;

use itertools::{chain, iproduct};

use crate::game::RawCanvas;

use super::field::Cell;
use super::grid::Grid;

pub fn grid<const N: usize, const M: usize>(
    canvas: &mut Canvas,
    bucket: &Grid<Cell, N, M>,
    cell_px: usize,
    [y0, x0]: [usize; 2],
    show_disappearing: bool,
) {
    // draw border
    {
        let y1 = y0 + bucket.height() * cell_px - 1;
        let x1 = x0 + bucket.width()  * cell_px - 1;

        for (y, x) in chain(iproduct!(y0..=y1, [x0, x1]), iproduct!([y0, y1], (x0..=x1))) {
            if let Some(pixel) = canvas.get_mut(x, y) {
                *pixel = color::WHITE;
            }
        }
    }

    let is_drawable = |c| c == Cell::Falling || c == Cell::Frozen || (c == Cell::Disappearing && show_disappearing);
    let to_color = |c| if c == Cell::Falling { color::BLUE } else { color::WHITE };
    bucket.iter_with_idx()
        .filter(|(_, &c)| is_drawable(c))
        .map(|(i, &c)| (i, to_color(c)))
        .for_each(|([y, x], color)| {
            self::cell(canvas, cell_px, [y0 + y * cell_px, x0 + x * cell_px], color);
        });
}

pub fn cell(canvas: &mut Canvas, cell_px: usize, [y0, x0]: [usize; 2], color: u32) {
    let borders = [cell_px, cell_px - 2, cell_px - 8];
    let colors = [color::BLACK, color].into_iter().cycle();

    for (px, color) in zip(borders, colors) {
        fill_square(canvas, color, [y0 + cell_px - px, x0 + cell_px - px], [y0 + px, x0 + px]);
    }
}

pub mod color {
    pub const BLACK: u32 = 0x0000_0000;
    pub const WHITE: u32 = 0x00FF_FFFF;
    pub const GREEN: u32 = 0x0000_FF00;
    pub const BLUE: u32 = 0x0000_00FF;
}

fn fill_square(canvas: &mut Canvas, color: u32, [y0, x0]: [usize; 2], [y1, x1]: [usize; 2]) {
    for y in y0..y1 {
        for x in x0..x1 {
            if let Some(pixel) = canvas.get_mut(x, y) {
                *pixel = color;
            }
        }
    }
}

pub fn clear(canvas: &mut Canvas) {
    fill_square(canvas, color::BLACK, [0, 0], [canvas.height(), canvas.width()]);
}

pub struct Canvas<'a> {
    inner: &'a mut dyn RawCanvas,
}

impl<'a> Canvas<'a> {
    pub fn from_raw(inner: &'a mut dyn RawCanvas) -> Self {
        Self {
            inner,
        }
    }
}

impl Canvas<'_> {
    pub fn width(&self) -> usize {
        self.inner.width()
    }

    pub fn height(&self) -> usize {
        self.inner.height()
    }

    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut u32> {
        let width = self.inner.width();
        let height = self.inner.height();

        if x < width && y < height {
            Some(&mut self.inner[y*width + x])
        } else {
            None
        }
    }
}