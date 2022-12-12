mod grid;

use crate::game::{Input, RawCanvas};
use crate::num_cast::NumCast;

use grid::Grid;

type Bucket = Grid<Cell, 10, 20>;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Cell {
    Empty,
    Falling,
    Frozen,
    Disappearing,
}

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
    Spawned {
        pos: [usize; 2],
    },
    ClearRows {
        range: [usize; 2],
        show: bool,
        blinks: i32,
    },
    FallAfterClear,
}

impl Data {
    pub fn new() -> Self {
        let mut cells = Grid::filled(Cell::Empty);
        for y in (cells.height() - 4)..(cells.height()) {
            for x in 0..cells.width() {
                cells[[y, x]] = Cell::Frozen;
            }
        }
        let h = cells.height();
        cells[[h - 1, 5]] = Cell::Empty;
        cells[[h - 2, 5]] = Cell::Empty;
        cells[[h - 3, 5]] = Cell::Empty;
        cells[[h - 4, 5]] = Cell::Empty;
        cells[[h - 4, 3]] = Cell::Empty;
        cells[[h - 4, 4]] = Cell::Empty;
        cells[[h - 4, 6]] = Cell::Empty;

        Self {
            state: State::NotSpawned,
            cells,
            gravity_tick: TICK,
            clear_row_flash_tick: CLEAR_ROW_FLASH_TICK,
            piece: Piece { ty: PieceType::Square, rotations: 0 },
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

    if tick {
        dbg!(&data.state);
        dbg!(&data.piece);
    }

    match data.state {
        State::NotSpawned => {
            if tick {
                let n_piece = (data.piece.ty as usize).wrapping_add(1) % 7;
                let piece_type = match n_piece {
                    0 => PieceType::Square,
                    1 => PieceType::Stick,
                    2 => PieceType::L,
                    3 => PieceType::ReverseL,
                    4 => PieceType::T,
                    5 => PieceType::S,
                    6 => PieceType::ReverseS,
                    _ => unreachable!(),
                };

                data.piece.ty = piece_type;

                let pos = [0, 5];
                spawn(&mut data.cells, data.piece, pos);
                data.state = State::Spawned { pos };
            }
        }
        State::Spawned { pos } => {
            let mut new_pos = pos;

            if let Some(t) = turn {
                try_turn_piece(data, t, pos);
            }

            if let Some(m) = mov {
                new_pos = try_move_piece(data, pos, m).unwrap_or(pos);
            }

            if tick {
                if let Some(p) = try_move_piece(data, pos, Move::Down) {
                    new_pos = p
                } else {
                    data.cells.iter_mut()
                        .filter(|c| **c == Cell::Falling)
                        .for_each(|c| *c = Cell::Frozen);
                }
            }

            let mut full_rows_range = Option::<[usize; 2]>::None;
            for (y, row) in data.cells.rows_mut().enumerate().rev() {
                if row.iter().all(|c| *c == Cell::Frozen) {
                    if let Some([_, end]) = full_rows_range {
                        full_rows_range = Some([y, end]);
                    } else {
                        full_rows_range = Some([y, y]);
                    }

                    for cell in row {
                        *cell = Cell::Disappearing;
                    }
                }
            }

            if let Some(range) = full_rows_range {
                data.state = State::ClearRows { range, show: false, blinks: 4 };
            } else if tick && new_pos == pos {
                data.state = State::NotSpawned;
            } else {
                data.state = State::Spawned { pos: new_pos };
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
                    for x in 0..data.cells.width() {
                        let cell = &mut data.cells[[y, x]];
                        if *cell == Cell::Frozen {
                            *cell = Cell::Falling;
                        }
                    }
                }
                for y in range[0]..=range[1] {
                    for x in 0..data.cells.width() {
                        let cell = &mut data.cells[[y, x]];
                        if *cell == Cell::Disappearing {
                            *cell = Cell::Empty;
                        }                    
                    }
                }
                data.state = State::FallAfterClear;
            }  
        },
        State::FallAfterClear => {
            if tick && !try_fall(&mut data.cells) {
                data.cells.iter_mut()
                    .filter(|c| **c == Cell::Falling)
                    .for_each(|c| *c = Cell::Frozen);
                data.state = State::NotSpawned;
            }
        },
    }
    draw_bucket(&mut canvas, &data.cells, show_disappearing);

    let mouse = &input.mouse;
    draw_cell(&mut canvas, mouse.x, mouse.y, color::GREEN);
}

fn try_fall(cells: &mut Bucket) -> bool {
    let mut fell = false;

    let width = cells.width();
    let height = cells.height();
    for y in (0..height).rev().skip(1) {
        let free_dst = (0..width).map(|x| cells[[y + 1, x]]).all(|cell| cell == Cell::Empty);
        let src_has_falling = (0..width).map(|x| cells[[y, x]]).any(|cell| cell == Cell::Falling);

        if free_dst && src_has_falling {
            for x in 0..width {
                cells[[y + 1, x]] = cells[[y, x]];
                cells[[y, x]] = Cell::Empty;
            }
            fell = true;
        }
    }

    fell
}

#[derive(Clone, Copy, Debug)]
struct Piece {
    ty: PieceType,
    rotations: u8,
}

#[derive(Clone, Copy, Debug)]
enum PieceType {
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

const fn blueprint_row(bin: u8) -> [Cell; 4] {
    [
        bool_to_cell(bin & 0b_1000 != 0),
        bool_to_cell(bin & 0b_0100 != 0),
        bool_to_cell(bin & 0b_0010 != 0),
        bool_to_cell(bin & 0b_0001 != 0),
    ]
}

fn blueprint(bs: [u8; 4]) -> Grid<Cell, 4, 4> {
    [
        blueprint_row(bs[0]),
        blueprint_row(bs[1]),
        blueprint_row(bs[2]),
        blueprint_row(bs[3]),
    ].into()
}

impl Piece {
    fn turn(&mut self, turn: Turn) {
        self.rotations = match turn {
            Turn::Left => self.rotations.wrapping_sub(1) % 4,
            Turn::Right => self.rotations.wrapping_add(1) % 4,
        };
    }

    fn dims(&self) -> [usize; 2] {
        let blueprint = self.get_blueprint();

        let [y0, x0] = [0, 0];
        let [y1, x1] = [blueprint.height(), blueprint.width()];

        let mut piece_width = blueprint.width();
        let mut piece_height = blueprint.height();
        for y in (y0..y1).rev() {
            if (x0..x1).any(|x| blueprint[[y, x]] == Cell::Falling) {
                piece_height = y + 1;
                break
            }
        }
        for x in (x0..x1).rev() {
            if (y0..y1).any(|y| blueprint[[y, x]] == Cell::Falling) {
                piece_width = x + 1;
                break
            }
        }
        [piece_height, piece_width]
    }

    fn get_blueprint(&self) -> Grid<Cell, 4, 4> {
        let mut blueprint = match self.ty {
            PieceType::Square => blueprint([
                0b_1100,
                0b_1100,
                0b_0000,
                0b_0000,
            ]),
            PieceType::Stick => blueprint([
                0b_1111,
                0b_0000,
                0b_0000,
                0b_0000,
            ]),
            PieceType::L => blueprint([
                0b_1000,
                0b_1000,
                0b_1100,
                0b_0000,
            ]),
            PieceType::ReverseL => blueprint([
                0b_0100,
                0b_0100,
                0b_1100,
                0b_0000,
            ]),
            PieceType::T => blueprint([
                0b_0100,
                0b_1110,
                0b_0000,
                0b_0000,
            ]),
            PieceType::S => blueprint([
                0b_0110,
                0b_1100,
                0b_0000,
                0b_0000,
            ]),
            PieceType::ReverseS => blueprint([
                0b_1100,
                0b_0110,
                0b_0000,
                0b_0000,
            ]),
        };

        for _ in 0..self.rotations {
            blueprint = blueprint.rotate(false);
        }

        {
            let [width, height] = [blueprint.width(), blueprint.height()];

            let mut y0 = 0;
            for y in 0..height {
                if (0..width).any(|x| blueprint[[y, x]] == Cell::Falling) {
                    y0 = y;
                    break
                }
            }
            let mut x0 = 0;
            for x in 0..width {
                if (0..height).any(|y| blueprint[[y, x]] == Cell::Falling) {
                    x0 = x;
                    break
                }
            }

            let mut result = blueprint;
            result.iter_mut().for_each(|c| *c = Cell::Empty);
            for (dst_y, src_y) in (y0..height).enumerate() {
                for (dst_x, src_x) in (x0..width).enumerate() {
                    result[[dst_y, dst_x]] = blueprint[[src_y, src_x]];
                }
            }
            blueprint = result;
        }

        blueprint
    }
}

fn spawn(cells: &mut Bucket, piece: Piece, pos: [usize; 2]) {
    Grid::copy_if(cells, &piece.get_blueprint(), pos, piece.dims(), |c| *c == Cell::Falling);
}

fn try_move_piece(data: &mut Data, pos: [usize; 2], mov: Move) -> Option<[usize; 2]> {
    let blueprint = data.piece.get_blueprint();
    let [piece_height, piece_width] = data.piece.dims();

    let new_pos = {
        let [y, x] = pos;
        match mov {
            Move::Left => {
                if x == 0 {
                    return None;
                }
                [y, x - 1]
            }
            Move::Right => {
                if x + piece_width > data.cells.width() - 1 {
                    return None;
                }
                [y, x + 1]
            }
            Move::Down => {
                if y + piece_height > data.cells.height() - 1 {
                    return None;
                }
                [y + 1, x]
            }
        }
    };

    {

        let [y0, x0] = new_pos;
        let [y1, x1] = [y0 + piece_height, x0 + piece_width];

        for (bp_y, y) in (y0..y1).enumerate() {
            for (bp_x, x) in (x0..x1).enumerate() {
                let src = blueprint[[bp_y, bp_x]];
                let dst = data.cells[[y, x]];

                if src == Cell::Falling && !(dst == Cell::Falling || dst == Cell::Empty) {
                    return None
                }
            }
        }
    }

    data.cells.iter_mut()
        .filter(|c| **c == Cell::Falling)
        .for_each(|c| *c = Cell::Empty);

    data.cells.copy_if(&blueprint, new_pos, [piece_height, piece_width], |c| *c == Cell::Falling);

    Some(new_pos)
}

fn try_turn_piece(data: &mut Data, turn: Turn, pos: [usize; 2]) -> bool {
    let [piece_height, piece_width] = data.piece.dims();
    {
        let blueprint = data.piece.get_blueprint();

        let [y0, x0] = pos;
        let [y1, x1] = [y0 + piece_height, x0 + piece_width];

        for (bp_y, y) in (y0..y1).enumerate() {
            for (bp_x, x) in (x0..x1).enumerate() {
                let src = blueprint[[bp_y, bp_x]];
                let dst = data.cells[[y, x]];

                if src == Cell::Falling && !(dst == Cell::Falling || dst == Cell::Empty) {
                    return false
                }
            }
        }
    }

    data.cells.iter_mut()
        .filter(|c| **c == Cell::Falling)
        .for_each(|c| *c = Cell::Empty);

    data.piece.turn(turn);
    data.cells.copy_if(&data.piece.get_blueprint(), pos, data.piece.dims(), |c| *c == Cell::Falling);

    true
}

fn draw_bucket(canvas: &mut Canvas, bucket: &Bucket, show_disappearing: bool) {
    let bucket_width_px = bucket.width().num_cast::<i32>() * CELL_PX;
    let bucket_height_px = bucket.height().num_cast::<i32>() * CELL_PX;

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

    for (y, row) in bucket.rows().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if *cell == Cell::Falling || *cell == Cell::Frozen || (*cell == Cell::Disappearing && show_disappearing) {
                let color = if *cell == Cell::Falling { color::BLUE } else { color::WHITE };
                draw_cell(canvas, x0 + x.num_cast::<i32>() * CELL_PX, y0 + y.num_cast::<i32>() * CELL_PX, color);
            }
        }
    }
}

fn draw_cell(canvas: &mut Canvas, x0: i32, y0: i32, color: u32) {
    const COLOR_SWITCH: [i32; 2] = [CELL_PX - 2, CELL_PX - 8];

    fn current(foreground: bool, color: u32) -> u32 {
        if foreground {
            color
        } else {
            color::BLACK
        }
    }

    let mut foreground = false;
    fill_square(canvas, current(foreground, color), x0, y0, x0 + CELL_PX, y0 + CELL_PX);

    for n in COLOR_SWITCH {
        foreground = !foreground;
        fill_square(canvas, current(foreground, color), x0 + CELL_PX - n, y0 + CELL_PX - n, x0 + n, y0 + n);
    }
}

mod color {
    pub const BLACK: u32 = 0x0000_0000;
    pub const WHITE: u32 = 0x00FF_FFFF;
    pub const GREEN: u32 = 0x0000_FF00;
    pub const BLUE: u32 = 0x0000_00FF;
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