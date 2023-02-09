#![allow(clippy::semicolon_if_nothing_returned)]

mod game;

fn main() -> lume::AnyhowResult {
    let mut data = game::Data::default();
    lume::run(&mut data)
}