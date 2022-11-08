use crate::game::{Input, RawCanvas};
use crate::num_cast::NumCast;

type Bucket = [[Cell; BUCKET_WIDTH]; BUCKET_HEIGHT];

#[derive(PartialEq, Clone, Copy, Debug)]
enum Cell {
    Empty,
    Falling,
    Frozen,
    Disappearing,
}

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
    piece: Piece,
}

#[derive(PartialEq, Debug)]
enum State {
    NotSpawned,
    Spawned,
    ClearRows {
        range: [usize; 2],
        show: bool,
        blinks: i32,
    },
    FallAfterClear,
}

impl Data {
    pub fn new() -> Self {
        let mut cells = [[Cell::Empty; BUCKET_WIDTH]; BUCKET_HEIGHT];
        for y in (BUCKET_HEIGHT - 4)..(BUCKET_HEIGHT) {
            for x in 0..BUCKET_WIDTH {
                cells[y][x] = Cell::Frozen;
            }
        }
        cells[BUCKET_HEIGHT - 1][5] = Cell::Empty;
        cells[BUCKET_HEIGHT - 2][5] = Cell::Empty;
        cells[BUCKET_HEIGHT - 3][5] = Cell::Empty;
        cells[BUCKET_HEIGHT - 4][5] = Cell::Empty;
        cells[BUCKET_HEIGHT - 4][3] = Cell::Empty;
        cells[BUCKET_HEIGHT - 4][4] = Cell::Empty;
        cells[BUCKET_HEIGHT - 4][6] = Cell::Empty;

        Self {
            state: State::NotSpawned,
            cells,
            gravity_tick: TICK,
            clear_row_flash_tick: CLEAR_ROW_FLASH_TICK,
            piece: Piece::Square,
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

#[derive(Debug, PartialEq)]
enum Turn {
    Left,
    Right,
}

impl From<&Input> for Option<Turn> {
    fn from(input: &Input) -> Self {
        let left = input.mouse.left.just_pressed();
        let right = input.mouse.right.just_pressed();
        match (left, right) {
            (true, true) | (false, false) => None,
            (true, false) => Some(Turn::Left),
            (false, true) => Some(Turn::Right),
        }
    }
}

pub fn update(data: &mut Data, mut canvas: Canvas, input: &Input, dt: f64) {
    clear(&mut canvas);

    let mov = Option::<Move>::from(input);
    let turn = Option::<Turn>::from(input);

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
                let n_piece = (data.piece as usize).wrapping_add(1) % 7;
                data.piece = match n_piece {
                    0 => Piece::Square,
                    1 => Piece::Stick,
                    2 => Piece::L,
                    3 => Piece::ReverseL,
                    4 => Piece::T,
                    5 => Piece::S,
                    6 => Piece::ReverseS,
                    _ => unreachable!(),
                };
                spawn(&mut data.cells, data.piece);
                data.state = State::Spawned;
            }
        }
        State::Spawned => {
            if let Some(t) = turn {
                try_turn_piece(&mut data.cells, t, data.piece);
            }

            if let Some(m) = mov {
                try_move_piece(&mut data.cells, m);
            }

            if tick && !try_move_piece(&mut data.cells, Move::Down) {
                for y in 0..BUCKET_HEIGHT {
                    for x in 0..BUCKET_WIDTH {
                        if data.cells[y][x] == Cell::Falling {
                            data.cells[y][x] = Cell::Frozen;
                        }
                    }
                }
                data.state = State::NotSpawned;
            }

            let mut full_rows_range = Option::<[usize; 2]>::None;
            for y in (0..BUCKET_HEIGHT).rev() {
                if (0..BUCKET_WIDTH).all(|x| data.cells[y][x] == Cell::Frozen) {
                    if let Some(range) = full_rows_range {
                        full_rows_range = Some([y, range[1]]);
                    } else {
                        full_rows_range = Some([y, y]);
                    }

                    for x in 0..BUCKET_WIDTH {
                        data.cells[y][x] = Cell::Disappearing;
                    }
                }
            }

            if let Some(range) = full_rows_range {
                data.state = State::ClearRows { range, show: false, blinks: 4 };
            }
        },
        State::ClearRows { range, show, blinks } => {
            if blinks > 0 {
                show_disappearing = show;

                data.clear_row_flash_tick -= dt;
                if data.clear_row_flash_tick < 0.0 {
                    data.clear_row_flash_tick = CLEAR_ROW_FLASH_TICK;
                    data.state = State::ClearRows { range, show: !show, blinks: blinks - 1 };
                }
            } else {
                show_disappearing = false;
                for y in 0..range[0] {
                    for x in 0..BUCKET_WIDTH {
                        let cell = &mut data.cells[y][x];
                        if *cell == Cell::Frozen {
                            *cell = Cell::Falling;
                        }
                    }
                }
                for y in range[0]..=range[1] {
                    for x in 0..BUCKET_WIDTH {
                        let cell = &mut data.cells[y][x];
                        if *cell == Cell::Disappearing {
                            *cell = Cell::Empty;
                        }                    
                    }
                }
                data.state = State::FallAfterClear;
            }  
        },
        State::FallAfterClear => {
            if tick && !try_move_piece(&mut data.cells, Move::Down) {
                for row in &mut data.cells {
                    for cell in row {
                        if *cell == Cell::Falling {
                            *cell = Cell::Frozen;
                        }
                    }
                }
                data.state = State::NotSpawned;
            }
        },
    }
    draw_bucket(&mut canvas, &data.cells, show_disappearing);

    let mouse = &input.mouse;
    draw_cell(&mut canvas, mouse.x, mouse.y);
}

#[derive(Clone, Copy, Debug)]
enum Piece {
    Square,
    Stick,
    L,
    ReverseL,
    T,
    S,
    ReverseS,
}

const fn bool_to_cell(b: bool) -> Cell {
    if b {
        Cell::Falling
    } else {
        Cell::Empty
    }
}

const fn blueprint_row(binary: u8) -> [Cell; 4] {
    [
        bool_to_cell(binary & 0b_0001 != 0),
        bool_to_cell(binary & 0b_0010 != 0),
        bool_to_cell(binary & 0b_0100 != 0),
        bool_to_cell(binary & 0b_1000 != 0),
    ]
}

const fn blueprint(bs: [u8; 4]) -> [[Cell; 4]; 4] {
    [
        blueprint_row(bs[0]),
        blueprint_row(bs[1]),
        blueprint_row(bs[2]),
        blueprint_row(bs[3]),
    ]
}

impl Piece {
    fn get_blueprint(self) -> [[Cell; 4]; 4] {
        match self {
            Piece::Square => blueprint([
                0b_1100,
                0b_1100,
                0b_0000,
                0b_0000,
            ]),
            Piece::Stick => blueprint([
                0b_1111,
                0b_0000,
                0b_0000,
                0b_0000,
            ]),
            Piece::L => blueprint([
                0b_1000,
                0b_1000,
                0b_1100,
                0b_0000,
            ]),
            Piece::ReverseL => blueprint([
                0b_0100,
                0b_0100,
                0b_1100,
                0b_0000,
            ]),
            Piece::T => blueprint([
                0b_0100,
                0b_1110,
                0b_0000,
                0b_0000,
            ]),
            Piece::S => blueprint([
                0b_0110,
                0b_1100,
                0b_0000,
                0b_0000,
            ]),
            Piece::ReverseS => blueprint([
                0b_1100,
                0b_0110,
                0b_0000,
                0b_0000,
            ]),
        }
    }
}

fn spawn(cells: &mut Bucket, piece: Piece) {
    let x = 5;
    match piece {
        Piece::Square => {
            cells[0][0 + x] = Cell::Falling;
            cells[0][1 + x] = Cell::Falling;
            cells[1][0 + x] = Cell::Falling;
            cells[1][1 + x] = Cell::Falling;
        }
        Piece::Stick => {
            cells[0][x - 1] = Cell::Falling;
            cells[0][x] = Cell::Falling;
            cells[0][x + 1] = Cell::Falling;
            cells[0][x + 2] = Cell::Falling;
        }
        Piece::L => {
            cells[0][x] = Cell::Falling;
            cells[1][x] = Cell::Falling;
            cells[2][x] = Cell::Falling;
            cells[2][x + 1] = Cell::Falling;
        }
        Piece::ReverseL => {
            cells[0][x] = Cell::Falling;
            cells[1][x] = Cell::Falling;
            cells[2][x] = Cell::Falling;
            cells[2][x - 1] = Cell::Falling;
        }
        Piece::T => {
            cells[0][x] = Cell::Falling;
            cells[1][x] = Cell::Falling;
            cells[1][x + 1] = Cell::Falling;
            cells[1][x - 1] = Cell::Falling;
        }
        Piece::S => {
            cells[1][x] = Cell::Falling;
            cells[1][x + 1] = Cell::Falling;
            cells[0][x + 1] = Cell::Falling;
            cells[0][x + 2] = Cell::Falling;
        }
        Piece::ReverseS => {
            cells[0][x] = Cell::Falling;
            cells[0][x + 1] = Cell::Falling;
            cells[1][x + 1] = Cell::Falling;
            cells[1][x + 2] = Cell::Falling;
        }
        _ => ()
    }
}

fn try_move_piece(cells: &mut Bucket, mov: Move) -> bool {
    let dx: fn(usize) -> usize;
    let dy: fn(usize) -> usize;
    let is_boundary: fn([usize; 2]) -> bool;
    match mov {
        Move::Left  => {
            dx = |x| x - 1;
            dy = |y| y;
            is_boundary = |[_y, x]| x == 0;
        }
        Move::Right => {
            dx = |x| x + 1;
            dy = |y| y;
            is_boundary = |[_y, x]| x == BUCKET_WIDTH - 1;
        }
        Move::Down => {
            dx = |x| x;
            dy = |y| y + 1;
            is_boundary = |[y, _x]| y == BUCKET_HEIGHT - 1;
        }
    };

    let mut prev = Vec::new();
    let can = 'can: {
        for y in 0..BUCKET_HEIGHT {
            for x in 0..BUCKET_WIDTH {
                if cells[y][x] == Cell::Falling {
                    if is_boundary([y, x]) || cells[dy(y)][dx(x)] == Cell::Frozen {
                        break 'can false
                    }
                    prev.push([y, x]);
                }
            }
        }
        !prev.is_empty()
    };
    if can {
        for &[y, x] in &prev {
            cells[y][x] = Cell::Empty;
        }
        for &[y, x] in &prev {
            cells[dy(y)][dx(x)] = Cell::Falling;
        }
    }
    can
}

fn try_turn_piece(cells: &mut Bucket, turn: Turn, piece: Piece) -> bool {
    let y0 = 'y0: {
        for y in 0..BUCKET_HEIGHT {
            for x in 0..BUCKET_WIDTH {
                if cells[y][x] == Cell::Falling {
                    break 'y0 y;
                }
            }
        }
        return false;
    };
    let x0 = 'x0: {
        for x in 0..BUCKET_WIDTH {
            for y in 0..BUCKET_HEIGHT {
                if cells[y][x] == Cell::Falling {
                    break 'x0 x;
                }
            }
        }
        return false;
    };

    match piece {
        Piece::Square => true,
        Piece::Stick => {
            assert!(cells[y0][x0] == Cell::Falling);
            if cells[y0][x0 + 1] == Cell::Falling {
                let x = x0 + 1;
                if y0 > 0 && y0 < BUCKET_WIDTH - 2 {
                    for y in (y0 - 1)..(y0 + 3) {
                        if cells[y][x] != Cell::Empty && cells[y][x] != Cell::Falling {
                            return false;
                        }
                    }

                    for x in x0..(x0 + 4) {
                        cells[y0][x] = Cell::Empty;
                    }
                    for y in (y0 - 1)..(y0 + 3) {
                        cells[y][x] = Cell::Falling;
                    }
                    true
                } else {
                    false
                }
            } else if cells[y0 + 1][x0] == Cell::Falling {
                false
            } else {
                unreachable!();
            }
        },
        Piece::L => todo!(),
        Piece::ReverseL => todo!(),
        Piece::T => todo!(),
        Piece::S => todo!(),
        Piece::ReverseS => todo!(),
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