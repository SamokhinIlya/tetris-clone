use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::borrow::Borrow;

#[derive(Copy, Clone, Debug)]
pub struct Grid<T, const N: usize, const M: usize> {
    inner: [[T; N]; M],
}

impl<T, const N: usize, const M: usize> Grid<T, N, M> {
    pub const WIDTH: usize = N;
    pub const HEIGHT: usize = M;

    pub const fn width(&self) -> usize {
        Self::WIDTH
    }

    pub const fn height(&self) -> usize {
        Self::HEIGHT
    }

    pub fn rows(&self) -> impl Iterator<Item = &[T; N]> + ExactSizeIterator + DoubleEndedIterator {
        self.inner.iter()
    }

    pub fn rows_mut(&mut self) -> impl Iterator<Item = &mut [T; N]> + ExactSizeIterator + DoubleEndedIterator {
        self.inner.iter_mut()
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter().flat_map(|row| row.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut().flat_map(|row| row.iter_mut())
    }
}

impl<T, const N: usize, const M: usize> Grid<T, N, M>
where
    T: Copy + std::fmt::Debug,
{
    pub fn filled(value: T) -> Self {
        Self::from([[value; N]; M])
    }

    pub fn copy_if<const O: usize, const P: usize>(
        &mut self,
        grid: &Grid<T, O, P>,
        [y0, x0]: [usize; 2],
        [y1, x1]: [usize; 2],
        pred: impl Fn(&T) -> bool,
    ) {
        use std::cmp::min;

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
}

impl<T, const N: usize> Grid<T, N, N>
where
    T: Copy,
{
    pub fn rotate(&self, left: bool) -> Self {
        let mut result = *self;
        if left {
            for y in 0..self.height() {
                for x in 0..self.width() {
                    result[[self.width() - 1 - x, y]] = self[[y, x]];
                }
            }
        } else {
            for y in 0..self.height() {
                for x in 0..self.width() {
                    result[[x, self.height() - 1 - y]] = self[[y, x]];
                }
            }
        }
        result
    }
}

impl<T, const N: usize, const M: usize> From<[[T; N]; M]> for Grid<T, N, M> {
    fn from(inner: [[T; N]; M]) -> Self {
        Self {
            inner
        }
    }
}

impl<T, I, const N: usize, const M: usize> Index<I> for Grid<T, N, M>
where
    I: Borrow<[usize; 2]>,
{
    type Output = T;

    fn index(&self, i: I) -> &Self::Output {
        let [y, x] = *i.borrow();
        assert!(y < Self::HEIGHT, "y out of bounds: HEIGHT is {} but y is {}", Self::HEIGHT, y);
        assert!(x < Self::WIDTH, "x out of bounds: WIDTH is {} but x is {}", Self::WIDTH, x);

        &self.inner[y][x]
    }
}

impl<T, I, const N: usize, const M: usize> IndexMut<I> for Grid<T, N, M>
where
    I: Borrow<[usize; 2]>,
{
    fn index_mut(&mut self, i: I) -> &mut Self::Output {
        let [y, x] = *i.borrow();
        assert!(y < Self::HEIGHT, "y out of bounds: HEIGHT is {} but y is {}", Self::HEIGHT, y);
        assert!(x < Self::WIDTH, "x out of bounds: WIDTH is {} but x is {}", Self::WIDTH, x);

        &mut self.inner[y][x]
    }
}