pub type Bucket = super::grid::Grid<Cell, 10, 20>;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Cell {
    Empty,
    Falling,
    Frozen,
    Disappearing,
}