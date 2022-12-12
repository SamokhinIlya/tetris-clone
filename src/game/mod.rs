mod inner;

use std::ops::IndexMut;

pub fn update(state: &mut State, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64) {
    inner::update(&mut state.inner, raw_canvas, input, dt)
}

pub struct State {
    inner: inner::Data,
}

impl State {
    pub fn new() -> Self {
        Self {
            inner: inner::Data::new(),
        }
    }
}

pub trait RawCanvas: IndexMut<usize, Output=u32> {
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}

#[derive(Default, Debug)]
pub struct Input {
    pub mouse: Mouse,
    pub keyboard: Keyboard,
}

#[derive(Default, Debug)]
pub struct Mouse {
    pub x: i32,
    pub y: i32,
    pub left: Button,
    pub right: Button,
}

#[derive(Default, Debug)]
pub struct Keyboard {
    pub left: Button,
    pub right: Button,
    pub down: Button,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Button {
    prev: bool,
    curr: bool,
}

impl Button {
    pub fn update(&mut self, curr: bool) {
        self.prev = self.curr;
        self.curr = curr;
    }

    pub fn is_pressed(self) -> bool {
        self.curr
    }

    pub fn just_pressed(self) -> bool {
        !self.prev && self.curr
    }
}
