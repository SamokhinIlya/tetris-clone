mod inner;

use std::ops::IndexMut;

pub fn update(state: &mut State, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    inner::update(&mut state.inner, inner::Canvas::from_raw(raw_canvas), input, dt)
}

pub struct State {
    inner: inner::State,
}

impl State {
    pub fn new() -> Self {
        Self {
            inner: inner::State::new(),
        }
    }
}

pub trait RawCanvas: IndexMut<usize, Output=u32> {
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}

#[derive(Default)]
pub struct Input {
    pub mouse: Mouse,
}

#[derive(Default, Debug)]
pub struct Mouse {
    pub x: i32,
    pub y: i32,
    pub left: bool,
    pub right: bool,
}
