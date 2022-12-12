use crate::game::Input;

#[derive(PartialEq, Eq)]
pub enum Move {
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

#[derive(Debug, PartialEq, Eq)]
pub enum Turn {
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