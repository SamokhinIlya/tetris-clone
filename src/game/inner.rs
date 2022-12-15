mod bucket;
mod draw;
mod grid;
mod input;
mod piece;
mod timer;

use bucket::{Bucket, Cell};
use draw::Canvas;
use grid::Grid;
use input::{Move, Turn};
use piece::Piece;
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
    SpawnPiece,
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
    ChangePiece,
}

impl Data {
    pub fn new() -> Self {
        const GRAVITY_TICK: f64 = 0.5;

        Self {
            state: State::SpawnPiece,
            cells: Grid::filled(Cell::Empty),
            gravity_timer: Timer::new(GRAVITY_TICK),
            clear_row_flash_timer: Timer::new(GRAVITY_TICK / 2.0),
            piece: Piece::new(),
        }
    }
}

pub fn update(data: &mut Data, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    let mut canvas = Canvas::from_raw(raw_canvas);

    let mov = Option::<Move>::from(input);
    let turn = Option::<Turn>::from(input);
    let tick = data.gravity_timer.tick(dt);

    let mut show_disappearing = true;

    match data.state {
        State::SpawnPiece => {
            if tick {
                let pos = [0, 5];
                spawn(&mut data.cells, data.piece, pos);

                data.state = State::Spawned { pos };
            }
        }
        State::Spawned { pos } => {
            let mut new_pos = turn.and_then(|t| try_turn_piece(data, t, pos)).unwrap_or(pos);
            new_pos = if tick {
                if let Some(p) = try_move_piece(data, new_pos, Move::Down) {
                    p
                } else {
                    data.cells.iter_mut()
                        .filter(|c| **c == Cell::Falling)
                        .for_each(|c| *c = Cell::Frozen);
                    new_pos
                }
            } else {
                mov.and_then(|m| try_move_piece(data, new_pos, m)).unwrap_or(new_pos)
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
                State::ChangePiece
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

                data.state = State::ChangePiece;
            }
        }
        State::ChangePiece => {
            data.piece = data.piece.next();

            data.state = State::SpawnPiece;
        }
    }

    // draw
    {
        const CELL_PX: usize = 30;

        draw::clear(&mut canvas);

        let bucket_width_px = data.cells.width() * CELL_PX;
        let bucket_height_px = data.cells.height() * CELL_PX;

        let x0 = canvas.width() / 2 - bucket_width_px / 2;
        let y0 = canvas.height() / 2 - bucket_height_px / 2;
        draw::grid(&mut canvas, &data.cells, CELL_PX, [y0, x0], show_disappearing);

        draw::grid(&mut canvas, &data.piece.next().get_blueprint(), CELL_PX, [y0, x0 + bucket_width_px + CELL_PX], false);

        let mouse = &input.mouse;
        draw::cell(&mut canvas, CELL_PX, [mouse.y, mouse.x], draw::color::GREEN);
    }
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

fn try_turn_piece(data: &mut Data, turn: Turn, pos: [usize; 2]) -> Option<[usize; 2]> {
    let mut new_pos = pos;

    let mut turned = data.piece;
    turned.turn(turn);
    {
        let [piece_height, piece_width] = turned.dims();
        let blueprint = turned.get_blueprint();

        let [y0, x0] = {
            let [mut y, mut x] = new_pos;
            if y + piece_height > data.cells.height() - 1 {
                y = data.cells.height() - piece_height;
            }
            if x + piece_width > data.cells.width() - 1 {
                x = data.cells.width() - piece_width;
            }
            new_pos = [y, x];
            new_pos
        };
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

    data.piece = turned;
    data.cells.copy_if(&data.piece.get_blueprint(), new_pos, data.piece.dims(), |c| *c == Cell::Falling);

    Some(new_pos)
}