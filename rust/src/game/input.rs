use lume::Input;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Move {
    Left,
    Right,
    Down,
}

impl TryFrom<&Input> for Move {
    type Error = ();

    fn try_from(input: &Input) -> Result<Self, Self::Error> {
        let move_down = input.keyboard.down.is_pressed();
        if move_down {
            Ok(Move::Down)
        } else {
            let move_left = input.keyboard.left.just_pressed();
            let move_right = input.keyboard.right.just_pressed();
            match (move_left, move_right) {
                (true, false) => Ok(Move::Left),
                (false, true) => Ok(Move::Right),
                _             => Err(()),
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Turn {
    Left,
    Right,
}

impl TryFrom<&Input> for Turn {
    type Error = ();

    fn try_from(input: &Input) -> Result<Self, Self::Error> {
        let left = input.mouse.left.just_pressed();
        let right = input.mouse.right.just_pressed();
        match (left, right) {
            (true, false) => Ok(Turn::Left),
            (false, true) => Ok(Turn::Right),
            _             => Err(()),
        }
    }
}