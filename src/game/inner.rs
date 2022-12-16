mod field;
mod draw;
mod grid;
mod input;
mod piece;
mod timer;

use field::{Field, Cell};
use draw::Canvas;
use grid::Grid;
use input::{Move, Turn};
use piece::Piece;
use timer::Timer;

use super::{Input, RawCanvas};

pub struct Data {
    state: State,
    field: Field,
    piece: Piece,
    gravity_timer: Timer,
    clear_row_flash_timer: Timer,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum State {
    SpawnPiece,
    Spawned {
        pos: [usize; 2],
    },
    BlinkDisappearingRows {
        show: bool,
        blinks: i32,
    },
    ClearDisappearingRows,
    FallAfterClear,
    ChangePiece,
}

impl Data {
    pub fn new() -> Self {
        const GRAVITY_TICK: f64 = 0.5;

        Self {
            state: State::SpawnPiece,
            field: Grid::filled(Cell::Empty),
            piece: Piece::new(),
            gravity_timer: Timer::new(GRAVITY_TICK),
            clear_row_flash_timer: Timer::new(GRAVITY_TICK / 2.0),
        }
    }
}

pub fn update(data: &mut Data, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    let mut canvas = Canvas::from_raw(raw_canvas);

    let mov = Option::<Move>::from(input);
    let turn = Option::<Turn>::from(input);
    let tick = data.gravity_timer.tick(dt);

    let mut show_disappearing = true;

    data.state = match data.state {
        State::SpawnPiece => {
            let pos = [0, 5];
            spawn(&mut data.field, data.piece, pos);

            State::Spawned { pos }
        }
        State::Spawned { pos } => {
            let mut new_pos = turn.and_then(|t| try_turn_piece(data, t, pos)).unwrap_or(pos);
            new_pos = if tick {
                if let Some(p) = try_move_piece(data, Move::Down, new_pos) {
                    p
                } else {
                    data.field.iter_mut()
                        .filter(|c| **c == Cell::Falling)
                        .for_each(|c| *c = Cell::Frozen);
                    new_pos
                }
            } else {
                mov.and_then(|m| try_move_piece(data, m, new_pos)).unwrap_or(new_pos)
            };

            let mut has_full_rows = false;
            data.field.rows_mut()
                .filter(|row| row.iter().all(|c| *c == Cell::Frozen))
                .for_each(|row| {
                    for cell in row {
                        *cell = Cell::Disappearing;
                    }

                    has_full_rows = true;
                });

            if has_full_rows {
                State::BlinkDisappearingRows { show: false, blinks: 4 }
            } else if tick && new_pos == pos {
                State::ChangePiece
            } else {
                State::Spawned { pos: new_pos }
            }
        }
        same @ State::BlinkDisappearingRows { show, blinks } => {
            if blinks > 0 {
                show_disappearing = show;

                if data.clear_row_flash_timer.tick(dt) {
                    State::BlinkDisappearingRows { show: !show, blinks: blinks - 1 }
                } else {
                    same
                }
            } else {
                show_disappearing = false;

                State::ClearDisappearingRows
            }  
        }
        State::ClearDisappearingRows => {
            for cell in data.field.iter_mut() {
                *cell = match *cell {
                    Cell::Frozen => Cell::Falling,
                    Cell::Disappearing => Cell::Empty,
                    c => c,
                }
            }

            State::FallAfterClear
        }
        same @ State::FallAfterClear => {
            if tick && !try_fall(&mut data.field) {
                data.field.iter_mut()
                    .filter(|c| **c == Cell::Falling)
                    .for_each(|c| *c = Cell::Frozen);

                State::ChangePiece
            } else {
                same
            }
        }
        State::ChangePiece => {
            data.piece = data.piece.next();

            State::SpawnPiece
        }
    };

    // draw
    {
        const CELL_PX: usize = 30;

        draw::clear(&mut canvas);

        let bucket_width_px = data.field.width() * CELL_PX;
        let bucket_height_px = data.field.height() * CELL_PX;

        let x0 = canvas.width() / 2 - bucket_width_px / 2;
        let y0 = canvas.height() / 2 - bucket_height_px / 2;
        draw::grid(&mut canvas, &data.field, CELL_PX, [y0, x0], show_disappearing);

        draw::grid(&mut canvas, &data.piece.next().blueprint(), CELL_PX, [y0, x0 + bucket_width_px + CELL_PX], false);

        let mouse = &input.mouse;
        draw::cell(&mut canvas, CELL_PX, [mouse.y, mouse.x], draw::color::GREEN);
    }
}

fn spawn(cells: &mut Field, piece: Piece, pos: [usize; 2]) {
    Grid::copy_if(cells, &piece.blueprint(), pos, piece.dims(), |c| *c == Cell::Falling);
}

fn try_fall(cells: &mut Field) -> bool {
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

fn try_move_piece(data: &mut Data, mov: Move, pos: [usize; 2]) -> Option<[usize; 2]> {
    let [piece_height, piece_width] = data.piece.dims();
    let new_pos = {
        let [height, width] = [data.field.height(), data.field.width()];

        let [y, x] = pos;
        let [y, x] = match mov {
            Move::Left => [y, x.wrapping_sub(1)],
            Move::Right => [y, x + 1],
            Move::Down => [y + 1, x],
        };
        if y >= height || x >= width || y + piece_height > height || x.wrapping_add(piece_width) > width {
            return None
        }

        [y, x]
    };

    let blueprint = data.piece.blueprint();
    {
        let [y0, x0] = new_pos;
        let [y1, x1] = [y0 + piece_height, x0 + piece_width];

        for (bp_y, y) in (y0..y1).enumerate() {
            for (bp_x, x) in (x0..x1).enumerate() {
                let src = blueprint[[bp_y, bp_x]];
                let dst = data.field[[y, x]];

                if src == Cell::Falling && !(dst == Cell::Falling || dst == Cell::Empty) {
                    return None
                }
            }
        }
    }

    data.field.iter_mut()
        .filter(|c| **c == Cell::Falling)
        .for_each(|c| *c = Cell::Empty);

    data.field.copy_if(&blueprint, new_pos, [piece_height, piece_width], |c| *c == Cell::Falling);

    Some(new_pos)
}

fn try_turn_piece(data: &mut Data, turn: Turn, pos: [usize; 2]) -> Option<[usize; 2]> {
    let new_pos;

    let mut turned = data.piece;
    turned.turn(turn);
    {
        let [piece_height, piece_width] = turned.dims();
        let blueprint = turned.blueprint();

        let [y0, x0] = {
            let [height, width] = [data.field.height(), data.field.width()];

            let [mut y, mut x] = pos;
            if y + piece_height > height {
                y = height - piece_height;
            }
            if x + piece_width > width {
                x = height - piece_width;
            }
            new_pos = [y, x];
            new_pos
        };
        let [y1, x1] = [y0 + piece_height, x0 + piece_width];

        for (bp_y, y) in (y0..y1).enumerate() {
            for (bp_x, x) in (x0..x1).enumerate() {
                let src = blueprint[[bp_y, bp_x]];
                let dst = data.field[[y, x]];

                if src == Cell::Falling && !(dst == Cell::Falling || dst == Cell::Empty) {
                    return None
                }
            }
        }
    }

    data.field.iter_mut()
        .filter(|c| **c == Cell::Falling)
        .for_each(|c| *c = Cell::Empty);

    data.piece = turned;
    data.field.copy_if(&data.piece.blueprint(), new_pos, data.piece.dims(), |c| *c == Cell::Falling);

    Some(new_pos)
}