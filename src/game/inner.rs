use crate::game::{Input, RawCanvas};
use crate::num_cast::NumCast;

type Bucket = [[Cell; BUCKET_WIDTH]; BUCKET_HEIGHT];

#[derive(PartialEq, Clone, Copy)]
enum Cell { Empty, Falling, Frozen, Disappearing }

const BUCKET_WIDTH: usize = 10;
const BUCKET_HEIGHT: usize = 20;
const CELL_PX: i32 = 30;

const TICK: f64 = 0.5;
const CLEAR_ROW_FLASH_TICK: f64 = TICK / 2.0;

pub struct Data {
    state: State,
    cells: Bucket,
    gravity_tick: f64,
    clear_row_flash_tick: f64,
}

#[derive(PartialEq, Debug)]
enum State {
    NotSpawned,
    Spawned,
    ClearRow { show: bool, blinks: i32 },
    FallAfterClear,
}

impl Data {
    pub fn new() -> Self {
        let cells = [[Cell::Empty; BUCKET_WIDTH]; BUCKET_HEIGHT];

        Self {
            state: State::NotSpawned,
            cells,
            gravity_tick: TICK,
            clear_row_flash_tick: CLEAR_ROW_FLASH_TICK,
        }
    }
}

#[derive(PartialEq)]
enum Move {
    Left,
    Right,
    Down,
}

impl From<&Input> for Option<Move> {
    fn from(input: &Input) -> Self {
        let move_down = input.keyboard.down.is_pressed();
        if move_down {
            Some(Move::Down)
        } else {
            let move_left = input.keyboard.left.just_pressed();
            let move_right = input.keyboard.right.just_pressed();
            match (move_left, move_right) {
                (true, true) | (false, false) => None,
                (true, false) => Some(Move::Left),
                (false, true) => Some(Move::Right),
            }
        }
    }
}

pub fn update(data: &mut Data, mut canvas: Canvas, input: &Input, dt: f64) {
    clear(&mut canvas);

    let mov = Option::<Move>::from(input); 

    let mut tick = false;
    data.gravity_tick -= dt;
    if data.gravity_tick < 0.0 {
        data.gravity_tick = TICK;
        tick = true;
    }

    let mut show_disappearing = true;
    match data.state {
        State::NotSpawned => {
            if tick {
                data.cells[0][0] = Cell::Falling;
                data.state = State::Spawned;
            }
        },
        State::Spawned => {
            let mut gravity_applied = false;
            match mov {
                Some(Move::Left) => {
                    for y in 0..BUCKET_HEIGHT {
                        for x in 0..BUCKET_WIDTH {
                            if data.cells[y][x] == Cell::Falling && x > 0 && data.cells[y][x - 1] == Cell::Empty {
                                data.cells[y].swap(x, x - 1);
                            }
                        }
                    }
                },
                Some(Move::Right) => {
                    for y in 0..BUCKET_HEIGHT {
                        for x in (0..BUCKET_WIDTH).rev() {
                            if data.cells[y][x] == Cell::Falling && x + 1 < BUCKET_WIDTH && data.cells[y][x + 1] == Cell::Empty {
                                data.cells[y].swap(x, x + 1);
                            }
                        }
                    }
                },
                Some(Move::Down) => {
                    apply_gravity(data);
                    gravity_applied = true;
                },
                None => (),
            }

            if !gravity_applied && tick {
                apply_gravity(data);
                gravity_applied = true;
            }

            if gravity_applied {
                let mut disappearing = false;
                for y in (0..BUCKET_HEIGHT).rev() {
                    if (0..BUCKET_WIDTH).all(|x| data.cells[y][x] == Cell::Frozen) {
                        disappearing = true;
                        for x in 0..BUCKET_WIDTH {
                            data.cells[y][x] = Cell::Disappearing;
                        }
                    }
                }

                if disappearing {
                    data.state = State::ClearRow { show: false, blinks: 4 };
                }
            }
        },
        State::ClearRow { show, blinks } => {
            if blinks > 0 {
                show_disappearing = show;

                data.clear_row_flash_tick -= dt;
                if data.clear_row_flash_tick < 0.0 {
                    data.clear_row_flash_tick = CLEAR_ROW_FLASH_TICK;
                    data.state = State::ClearRow { show: !show, blinks: blinks - 1 };
                }
            } else {
                show_disappearing = false;
                for row in &mut data.cells {
                    for cell in row {
                        if *cell == Cell::Disappearing {
                            *cell = Cell::Empty;
                        } else if *cell == Cell::Frozen {
                            *cell = Cell::Falling;
                        }
                    }
                }
                data.state = State::FallAfterClear;
            }  
        },
        State::FallAfterClear => {
            if tick {
                apply_gravity(data);
            }
        },
    }
    draw_bucket(&mut canvas, &data.cells, show_disappearing);

    let mouse = &input.mouse;
    draw_cell(&mut canvas, mouse.x, mouse.y);
}

fn apply_gravity(data: &mut Data) {
    for y in (0..BUCKET_HEIGHT).rev() {
        for x in 0..BUCKET_WIDTH {
            if data.cells[y][x] == Cell::Falling {
                if y + 1 < BUCKET_HEIGHT && (data.cells[y + 1][x] == Cell::Empty) {
                    data.cells[y][x] = Cell::Empty;
                    data.cells[y + 1][x] = Cell::Falling;
                } else {
                    data.cells[y][x] = Cell::Frozen;
                    data.state = State::NotSpawned;
                }
            }
        }
    }
}

fn draw_bucket(canvas: &mut Canvas, bucket: &Bucket, show_disappearing: bool) {
    let bucket_width_px = BUCKET_WIDTH.num_cast::<i32>() * CELL_PX;
    let bucket_height_px = BUCKET_HEIGHT.num_cast::<i32>() * CELL_PX;

    let x0 = canvas.width() / 2 - bucket_width_px / 2;
    let y0 = canvas.height() / 2 - bucket_height_px / 2;

    for y in y0..(y0 + bucket_height_px) {
        for x in x0..(x0 + bucket_width_px) {
            if y == y0 || y == (y0 + bucket_height_px - 1) || x == x0 || x == (x0 + bucket_width_px - 1) {
                if let Some(pixel) = canvas.get_mut(x, y) {
                    *pixel = color::WHITE;
                }
            }
        }
    }

    for (y, line) in bucket.iter().enumerate() {
        for (x, cell) in line.iter().enumerate() {
            if *cell == Cell::Falling || *cell == Cell::Frozen || (*cell == Cell::Disappearing && show_disappearing) {
                draw_cell(canvas, x0 + x.num_cast::<i32>() * CELL_PX, y0 + y.num_cast::<i32>() * CELL_PX);
            }
        }
    }
}

fn draw_cell(canvas: &mut Canvas, x0: i32, y0: i32) {
    const COLOR_SWITCH: [i32; 2] = [CELL_PX - 2, CELL_PX - 8];

    fn current(foreground: bool) -> u32 {
        if foreground {
            color::WHITE
        } else {
            color::BLACK
        }
    }

    let mut foreground = false;
    fill_square(canvas, current(foreground), x0, y0, x0 + CELL_PX, y0 + CELL_PX);

    for n in COLOR_SWITCH {
        foreground = !foreground;
        fill_square(canvas, current(foreground), x0 + CELL_PX - n, y0 + CELL_PX - n, x0 + n, y0 + n);
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