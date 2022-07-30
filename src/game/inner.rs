use crate::game::RawCanvas;
use crate::num_cast::NumCast;

type Bucket = [[bool; 10]; 20];
const SIZE: i32 = 30;

const TICK: f64 = 0.5;

pub struct State {
    spawned: bool,
    cells: Bucket,
    tick: f64,
}

impl State {
    pub fn new() -> Self {
        let cells = [[false; 10]; 20];

        Self {
            spawned: false,
            cells,
            tick: TICK,
        }
    }
}

pub fn update(state: &mut State, mut canvas: Canvas, input: &super::Input, dt: f64) {
    clear(&mut canvas);

    let mut tick = false;
    state.tick -= dt;
    if state.tick < 0.0 {
        state.tick = TICK;
        tick = true;
    }

    if state.spawned {
        if tick {
            let mut falling_cells = Vec::new();
            for (y, line) in state.cells.iter().enumerate().rev() {
                for (x, cell) in line.iter().enumerate() {
                    if *cell {
                        falling_cells.push([x, y]);
                    }
                }
            }

            for [x, y] in falling_cells {
                if y < state.cells.len() - 1 && !state.cells[y + 1][x] {
                    state.cells[y][x] = false;
                    state.cells[y + 1][x] = true;
                }
            }
        }
    } else {
        let width = state.cells[0].len();
        for i in 0..5 {
            state.cells[i][i] = true;
            state.cells[i][width - 1 - i] = true;
        }
        state.spawned = true;
    }
    draw_bucket(&mut canvas, &state.cells);

    let mouse = &input.mouse;
    draw_cell(&mut canvas, mouse.x, mouse.y);
}

fn draw_bucket(canvas: &mut Canvas, bucket: &Bucket) {
    let bucket_width = bucket[0].len().num_cast::<i32>() * SIZE;
    let bucket_height = bucket.len().num_cast::<i32>() * SIZE;

    let x0 = canvas.width() / 2 - bucket_width / 2;
    let y0 = canvas.height() / 2 - bucket_height / 2;

    for y in y0..(y0 + bucket_height) {
        for x in x0..(x0 + bucket_width) {
            if y == y0 || y == (y0 + bucket_height - 1) || x == x0 || x == (x0 + bucket_width - 1) {
                if let Some(pixel) = canvas.get_mut(x, y) {
                    *pixel = color::WHITE;
                }
            }
        }
    }

    for (y, line) in bucket.iter().enumerate() {
        for (x, cell) in line.iter().enumerate() {
            if *cell {
                draw_cell(canvas, x0 + x.num_cast::<i32>() * SIZE, y0 + y.num_cast::<i32>() * SIZE);
            }
        }
    }
}

fn draw_cell(canvas: &mut Canvas, x0: i32, y0: i32) {
    const COLOR_SWITCH: [i32; 2] = [SIZE - 2, SIZE - 8];

    fn current(foreground: bool) -> u32 {
        if foreground {
            color::WHITE
        } else {
            color::BLACK
        }
    }

    let mut foreground = false;
    fill_square(canvas, current(foreground), x0, y0, x0 + SIZE, y0 + SIZE);

    for n in COLOR_SWITCH {
        foreground = !foreground;
        fill_square(canvas, current(foreground), x0 + SIZE - n, y0 + SIZE - n, x0 + n, y0 + n);
    }
}

mod color {
    pub const BLACK: u32 = 0x0000_0000;
    pub const WHITE: u32 = 0x00FF_FFFF;
}

fn fill_square(canvas: &mut Canvas, color: u32, x0: i32, y0: i32, x1: i32, y1: i32) {
    for y in y0..y1 {
        for x in x0..x1 {
            if let Some(pixel) = canvas.get_mut(x, y) {
                *pixel = color;
            }
        }
    }
}

fn clear(canvas: &mut Canvas) {
    let width = canvas.width();
    let height = canvas.height();
    fill_square(canvas, color::BLACK, 0, 0, width, height);
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