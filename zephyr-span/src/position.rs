#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    row: usize,
    col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Position {
        Position { row, col }
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match self.row.cmp(&other.row) {
            Ordering::Equal => self.col.cmp(&other.col),
            order => order,
        }
    }
}
