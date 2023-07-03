mod position;

pub use position::Position;
use std::ops::{Add, AddAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }
}

impl Add for Span {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        if self.start <= rhs.start {
            if self.end <= rhs.end {
                Span { start: self.start, end: rhs.end }
            } else {
                Span { start: self.start, end: self.end }
            }
        } else {
            rhs.add(self)
        }
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(test)]
mod test {
    use crate::{Span, position::Position};

    #[test]
    fn has_distance() {
        let l = Span::new(Position::new(0, 0),  Position::new(1, 2));
        let r = Span::new(Position::new(2, 10), Position::new(3, 5));
        assert_eq!(
            Span::new(Position::new(0, 0), Position::new(3, 5)),
            l + r
        );
        assert_eq!(
            Span::new(Position::new(0, 0), Position::new(3, 5)),
            r + l
        );
    }

    #[test]
    fn crossing() {
        let l = Span::new(Position::new(0, 0), Position::new(3, 4));
        let r = Span::new(Position::new(3, 0), Position::new(4, 8));
        assert_eq!(
            Span::new(Position::new(0, 0), Position::new(4, 8)),
            l + r
        );
        assert_eq!(
            Span::new(Position::new(0, 0), Position::new(4, 8)),
            r + l
        );
    }

    #[test]
    fn contain() {
        let l = Span::new(Position::new(0, 0), Position::new(10, 5));
        let r = Span::new(Position::new(0, 1), Position::new(10, 4));
        assert_eq!(l, l + r);
        assert_eq!(l, r + l);
    }

    #[test]
    fn same_span() {
        let l = Span::new(Position::new(0, 0), Position::new(2, 10));
        let r = Span::new(Position::new(0, 0), Position::new(2, 10));
        assert_eq!(l, l + r);
        assert_eq!(l, r + l);
    }
}
