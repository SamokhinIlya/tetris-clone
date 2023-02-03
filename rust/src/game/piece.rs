use super::{field::Cell, grid::Grid, input::Turn};

#[derive(Clone, Copy, Debug)]
pub struct Piece {
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

impl Piece {
    pub fn new() -> Self {
        Self {
            ty: PieceType::Square,
            rotations: 0,
        }
    }

    pub fn next(self) -> Self {
        let mut result = Self::new();
        result.ty = {
            match (self.ty as usize).wrapping_add(1) % 7 {
                0 => PieceType::Square,
                1 => PieceType::Stick,
                2 => PieceType::L,
                3 => PieceType::ReverseL,
                4 => PieceType::T,
                5 => PieceType::S,
                6 => PieceType::ReverseS,
                _ => unreachable!(),
            }
        };
        result
    }

    pub fn turn(&mut self, turn: Turn) {
        self.rotations = match turn {
            Turn::Left => self.rotations.wrapping_sub(1) % 4,
            Turn::Right => self.rotations.wrapping_add(1) % 4,
        };
    }

    pub fn dims(self) -> [usize; 2] {
        let b = self.blueprint();

        let [y0, x0] = [0, 0];
        let [y1, x1] = [b.height(), b.width()];

        let piece_height = (y0..y1).rev().find(|&y| (x0..x1).any(|x| b[[y, x]] == Cell::Falling)).map_or(y1, |y| y + 1);
        let piece_width  = (x0..x1).rev().find(|&x| (y0..y1).any(|y| b[[y, x]] == Cell::Falling)).map_or(x1, |x| x + 1);
        [piece_height, piece_width]
    }

    pub fn blueprint(self) -> Grid<Cell, 4, 4> {
        let mut blueprint = blueprint(match self.ty {
            PieceType::Square => [
                0b_1100,
                0b_1100,
                0b_0000,
                0b_0000,
            ],
            PieceType::Stick => [
                0b_1111,
                0b_0000,
                0b_0000,
                0b_0000,
            ],
            PieceType::L => [
                0b_1000,
                0b_1000,
                0b_1100,
                0b_0000,
            ],
            PieceType::ReverseL => [
                0b_0100,
                0b_0100,
                0b_1100,
                0b_0000,
            ],
            PieceType::T => [
                0b_0100,
                0b_1110,
                0b_0000,
                0b_0000,
            ],
            PieceType::S => [
                0b_0110,
                0b_1100,
                0b_0000,
                0b_0000,
            ],
            PieceType::ReverseS => [
                0b_1100,
                0b_0110,
                0b_0000,
                0b_0000,
            ],
        });

        for _ in 0..self.rotations {
            blueprint = blueprint.rotate(false);
        }

        {
            let [width, height] = [blueprint.width(), blueprint.height()];

            let y0 = (0..height).find(|&y| (0..width).any(|x| blueprint[[y, x]] == Cell::Falling)).unwrap_or(0);
            let x0 = (0..width).find(|&x| (0..height).any(|y| blueprint[[y, x]] == Cell::Falling)).unwrap_or(0);

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

fn blueprint(bs: [u8; 4]) -> Grid<Cell, 4, 4> {
    [
        from_bin(bs[0]),
        from_bin(bs[1]),
        from_bin(bs[2]),
        from_bin(bs[3]),
    ]
    .into()
}

const fn from_bin(bin: u8) -> [Cell; 4] {
    [
        from_bool(bin & 0b_1000 != 0),
        from_bool(bin & 0b_0100 != 0),
        from_bool(bin & 0b_0010 != 0),
        from_bool(bin & 0b_0001 != 0),
    ]
}

const fn from_bool(b: bool) -> Cell {
    if b {
        Cell::Falling
    } else {
        Cell::Empty
    }
}