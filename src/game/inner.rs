mod bucket;
mod draw;
mod grid;
mod input;
mod timer;

use bucket::{Bucket, Cell};
use draw::Canvas;
use grid::Grid;
use input::{Move, Turn};
use timer::Timer;

use super::{Input, RawCanvas};

pub struct Data {
    state: State,
    cells: Bucket,
    gravity_timer: Timer,
    clear_row_flash_timer: Timer,
    piece: Piece,
}

#[derive(PartialEq, Debug)]
enum State {
    NotSpawned,
    Spawned {
        pos: [usize; 2],
    },
    BlinkDisappearingRows {
        range: [usize; 2],
        show: bool,
        blinks: i32,
    },
    ClearDisappearingRows {
        range: [usize; 2],
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

        const TICK: f64 = 0.5;
        const CLEAR_ROW_FLASH_TICK: f64 = TICK / 2.0;

        Self {
            state: State::NotSpawned,
            cells,
            gravity_timer: Timer::new(TICK),
            clear_row_flash_timer: Timer::new(CLEAR_ROW_FLASH_TICK),
            piece: Piece { ty: PieceType::Square, rotations: 0 },
        }
    }
}

pub fn update(data: &mut Data, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    let mut canvas = Canvas::from_raw(raw_canvas);
    draw::clear(&mut canvas);

    let mov = Option::<Move>::from(input);
    let turn = Option::<Turn>::from(input);
    let tick = data.gravity_timer.tick(dt);

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
            if let Some(t) = turn {
                try_turn_piece(data, t, pos);
            }

            let new_pos = if tick {
                if let Some(p) = try_move_piece(data, pos, Move::Down) {
                    p
                } else {
                    data.cells.iter_mut()
                        .filter(|c| **c == Cell::Falling)
                        .for_each(|c| *c = Cell::Frozen);
                    pos
                }
            } else {
                mov.and_then(|m| try_move_piece(data, pos, m)).unwrap_or(pos)
            };

            let mut full_rows_range = Option::<[usize; 2]>::None;
            data.cells.rows_mut().enumerate().rev()
                .filter(|(_, row)| row.iter().all(|c| *c == Cell::Frozen))
                .for_each(|(y, row)| {
                    for cell in row {
                        *cell = Cell::Disappearing;
                    }

                    full_rows_range = full_rows_range.map(|[_, end]| [y, end]).or(Some([y, y + 1]));
                });

            data.state = if let Some(range) = full_rows_range {
                State::BlinkDisappearingRows { range, show: false, blinks: 4 }
            } else if tick && new_pos == pos {
                State::NotSpawned
            } else {
                State::Spawned { pos: new_pos }
            };
        }
        State::BlinkDisappearingRows { range, show, blinks } => {
            if blinks > 0 {
                show_disappearing = show;

                if data.clear_row_flash_timer.tick(dt) {
                    data.state = State::BlinkDisappearingRows { range, show: !show, blinks: blinks - 1 };
                }
            } else {
                show_disappearing = false;

                data.state = State::ClearDisappearingRows { range };
            }  
        }
        State::ClearDisappearingRows { range: [start, end] } => {
            data.cells.rows_mut().take(start)
                .for_each(|row| row.iter_mut()
                    .filter(|cell| **cell == Cell::Frozen)
                    .for_each(|cell| *cell = Cell::Falling));

            data.cells.rows_mut().take(end).skip(start)
                .for_each(|row| row.iter_mut()
                    .filter(|cell| **cell == Cell::Disappearing)
                    .for_each(|cell| *cell = Cell::Empty));

            data.state = State::FallAfterClear;
        }
        State::FallAfterClear => {
            if tick && !try_fall(&mut data.cells) {
                data.cells.iter_mut()
                    .filter(|c| **c == Cell::Falling)
                    .for_each(|c| *c = Cell::Frozen);

                data.state = State::NotSpawned;
            }
        }
    }
    draw::bucket(&mut canvas, &data.cells, show_disappearing);

    let mouse = &input.mouse;
    draw::cell(&mut canvas, mouse.x, mouse.y, draw::color::GREEN);
}

fn try_fall(cells: &mut Bucket) -> bool {
    let mut fell = false;

    let width = cells.width();
    let height = cells.height();
    for y in (0..height).rev().skip(1) {
        let dst_is_free = (0..width).map(|x| cells[[y + 1, x]]).all(|cell| cell == Cell::Empty);
        let src_has_falling = (0..width).map(|x| cells[[y, x]]).any(|cell| cell == Cell::Falling);

        if dst_is_free && src_has_falling {
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

const fn from_bool(b: bool) -> Cell {
    if b {
        Cell::Falling
    } else {
        Cell::Empty
    }
}

const fn from_bin(bin: u8) -> [Cell; 4] {
    [
        from_bool(bin & 0b_1000 != 0),
        from_bool(bin & 0b_0100 != 0),
        from_bool(bin & 0b_0010 != 0),
        from_bool(bin & 0b_0001 != 0),
    ]
}

fn blueprint(bs: [u8; 4]) -> Grid<Cell, 4, 4> {
    [
        from_bin(bs[0]),
        from_bin(bs[1]),
        from_bin(bs[2]),
        from_bin(bs[3]),
    ]
    .into()
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