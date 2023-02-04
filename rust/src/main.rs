#![allow(clippy::semicolon_if_nothing_returned)]

mod game;

fn main() -> anyhow::Result<()> {
    let mut data = game::Data::default();
    lume::run(&mut data)
}