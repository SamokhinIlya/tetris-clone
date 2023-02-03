mod field;
mod draw;
mod grid;
mod input;
mod piece;
mod timer;

use std::any::Any;

use field::{Field, Cell};
use draw::Canvas;
use grid::Grid;
use input::{Move, Turn};
use piece::Piece;
use timer::Timer;

use crate::lume::{Input, RawCanvas};

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

impl Default for Data {
    fn default() -> Self {
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

pub fn update(data: &mut dyn Any, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    let data = data.downcast_mut::<Data>().unwrap();
    let mut canvas = Canvas::from_raw(raw_canvas);

    inner(data, &mut canvas, input, dt);
}

fn inner(data: &mut Data, canvas: &mut Canvas, input: &Input, dt: f64) {
    let tick = data.gravity_timer.tick(dt);
    let mov = if tick {
        Some(Move::Down)
    } else {
        Option::<Move>::from(input)
    };
    let turn = Option::<Turn>::from(input);

    let mut show_disappearing = true;

    data.state = match data.state {
        State::SpawnPiece => {
            let pos = [0, 5];
            spawn(&mut data.field, data.piece, pos);

            State::Spawned { pos }
        }
        State::Spawned { pos } => {
            let mut new_pos = turn.and_then(|t| try_turn_piece(data, t, pos)).unwrap_or(pos);
            new_pos = mov.and_then(|m| try_move_piece(data, m, new_pos)).unwrap_or_else(|| {
                if tick {
                    data.field.iter_mut()
                        .filter(|c| **c == Cell::Falling)
                        .for_each(|c| *c = Cell::Frozen);
                }
                new_pos
            });

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

        draw::clear(canvas);

        let bucket_width_px = data.field.width() * CELL_PX;
        let bucket_height_px = data.field.height() * CELL_PX;

        let x0 = canvas.width() / 2 - bucket_width_px / 2;
        let y0 = canvas.height() / 2 - bucket_height_px / 2;
        draw::grid(canvas, &data.field, CELL_PX, [y0, x0], show_disappearing);

        draw::grid(canvas, &data.piece.next().blueprint(), CELL_PX, [y0, x0 + bucket_width_px + CELL_PX], false);

        let mouse = &input.mouse;
        draw::cell(canvas, CELL_PX, [mouse.y, mouse.x], draw::color::GREEN);
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
    let new_pos = {
        let [height, width] = [data.field.height(), data.field.width()];
        let [piece_height, piece_width] = data.piece.dims();

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

    if has_collided(&data.piece, &data.field, new_pos) {
        return None
    }

    move_piece(&mut data.field, &data.piece, new_pos);
    Some(new_pos)
}

fn try_turn_piece(data: &mut Data, turn: Turn, pos: [usize; 2]) -> Option<[usize; 2]> {
    let mut turned = data.piece;
    turned.turn(turn);

    let new_pos = {
        let [height, width] = [data.field.height(), data.field.width()];
        let [piece_height, piece_width] = turned.dims();

        let [y, x] = pos;
        [
            if y + piece_height > height { height - piece_height } else { y },
            if x + piece_width > width { width - piece_width } else { x },
        ]
    };

    if has_collided(&data.piece, &data.field, new_pos) {
        return None
    }

    data.piece = turned;
    move_piece(&mut data.field, &data.piece, new_pos);
    Some(new_pos)
}

fn has_collided(piece: &Piece, field: &Field, [y0, x0]: [usize; 2]) -> bool {
    let [piece_height, piece_width] = piece.dims();
    let blueprint = piece.blueprint();

    let [y1, x1] = [y0 + piece_height, x0 + piece_width];

    for (bp_y, y) in (y0..y1).enumerate() {
        for (bp_x, x) in (x0..x1).enumerate() {
            let src = blueprint[[bp_y, bp_x]];
            let dst = field[[y, x]];

            if src == Cell::Falling && !(dst == Cell::Falling || dst == Cell::Empty) {
                return true
            }
        }
    }

    false
}

fn move_piece(field: &mut Field, piece: &Piece, pos: [usize; 2]) {
    field.iter_mut()
        .filter(|c| **c == Cell::Falling)
        .for_each(|c| *c = Cell::Empty);

    field.copy_if(&piece.blueprint(), pos, piece.dims(), |c| *c == Cell::Falling);
}