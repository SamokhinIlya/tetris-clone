use std::borrow::Borrow;
use std::cmp::min;
use std::fmt::Debug;
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, Debug)]
pub struct Grid<T, const W: usize, const H: usize> {
    inner: [[T; W]; H],
}

impl<T, const W: usize, const H: usize> Grid<T, W, H> {
    pub const fn width(&self) -> usize {
        W
    }

    pub const fn height(&self) -> usize {
        H
    }

    #[allow(dead_code)]
    pub fn rows(&self) -> impl Iterator<Item = &[T; W]> + ExactSizeIterator + DoubleEndedIterator {
        self.inner.iter()
    }

    pub fn rows_mut(&mut self) -> impl Iterator<Item = &mut [T; W]> + ExactSizeIterator + DoubleEndedIterator {
        self.inner.iter_mut()
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter().flat_map(|row| row.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut().flat_map(|row| row.iter_mut())
    }

    pub fn iter_with_idx(&self) -> impl Iterator<Item = ([usize; 2], &T)> {
        self.inner.iter().enumerate()
            .flat_map(|(y, row)| row.iter().enumerate()
                .map(move |(x, v)| ([y, x], v)))
    }
}

impl<T, const W: usize, const H: usize> Grid<T, W, H>
where
    T: Copy,
{
    pub fn filled(value: T) -> Self {
        Self::from([[value; W]; H])
    }

    pub fn copy_if<const N: usize, const M: usize>(
        &mut self,
        grid: &Grid<T, N, M>,
        [y0, x0]: [usize; 2],
        [y1, x1]: [usize; 2],
        pred: impl Fn(&T) -> bool,
    ) {
        let y1 = min(y0 + y1, self.height());
        let x1 = min(x0 + x1, self.width());
        for (src_y, dst_y) in (y0..y1).enumerate() {
            for (src_x, dst_x) in (x0..x1).enumerate() {
                let dst = &mut self[[dst_y, dst_x]];
                let src = grid[[src_y, src_x]];

                if pred(&src) {
                    *dst = src;
                }
            }
        }
    }

    pub fn rotate(&self, left: bool) -> Self {
        fn rotated_left ([_, w]: [usize; 2], [y, x]: [usize; 2]) -> [usize; 2] { [w - 1 - x, y        ] }
        fn rotated_right([h, _]: [usize; 2], [y, x]: [usize; 2]) -> [usize; 2] { [x        , h - 1 - y] }

        let rotated = if left { rotated_left } else { rotated_right };
        let dims = [self.height(), self.width()];

        let mut result = *self;
        for y in 0..self.height() {
            for x in 0..self.width() {
                result[rotated(dims, [y, x])] = self[[y, x]];
            }
        }
        result
    }
}

impl<T, const W: usize, const H: usize> From<[[T; W]; H]> for Grid<T, W, H> {
    fn from(inner: [[T; W]; H]) -> Self {
        Self {
            inner
        }
    }
}

impl<I, T, const W: usize, const H: usize> Index<I> for Grid<T, W, H>
where
    I: Borrow<[usize; 2]>,
{
    type Output = T;

    fn index(&self, i: I) -> &Self::Output {
        let [y, x] = *i.borrow();
        assert!(y < H, "y out of bounds: HEIGHT is {H} but y is {y}");
        assert!(x < W, "x out of bounds: WIDTH is {W} but x is {x}");

        &self.inner[y][x]
    }
}

impl<I, T, const W: usize, const H: usize> IndexMut<I> for Grid<T, W, H>
where
    I: Borrow<[usize; 2]>,
{
    fn index_mut(&mut self, i: I) -> &mut Self::Output {
        let [y, x] = *i.borrow();
        assert!(y < H, "y out of bounds: HEIGHT is {H} but y is {y}");
        assert!(x < W, "x out of bounds: WIDTH is {W} but x is {x}");

        &mut self.inner[y][x]
    }
}