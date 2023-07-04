//! Provide functional to represent the range of source code.
//!
//! # `Span` and `Spannable`
//!
//! `Span` is the core of this module. This represent the range of the item in source code.
//! You can concat several `Span` with `+`, or appending `Span`s to a `Span` using `+=`.
//!
//! `Spannable` is a trait to generalize the span of item.
//! For example, if a struct hold two spannable item, then with `Spannable`, you can get
//! the span of the struct by calling `span()`.
//!
//! ```
//! use zephyr_span::{Span, Spannable};
//!
//! struct TwoSpan {
//!     one: One,
//!     two: Two,
//! }
//!
//! impl Spannable for TwoSpan {
//!     fn span(&self) -> Span {
//!         self.one.span + self.two.span
//!     }
//! }
//!
//! struct One {
//!     span: Span
//! }
//!
//! struct Two {
//!     span: Span
//! }
//! ```

use derive_new::new;
use std::ops::{Add, AddAssign};

/// A trait for the item which has it own span.
pub trait Spannable {
    fn span(&self) -> Span;
}

/// A struct which hold the range of source code where the item come from.
#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    offset: usize,
    len: usize
}

impl Span {
    /// Return start position of the item in the source code.
    ///
    /// # Examples
    ///
    /// ```
    /// use zephyr_span::Span;
    /// let span = Span::new(0, 10);
    /// assert_eq!(span.offset(), 0);
    /// ```
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Return the length of the item in the source code.
    ///
    /// # Examples
    ///
    /// ```
    /// use zephyr_span::Span;
    /// let span = Span::new(0, 10);
    /// assert_eq!(span.len(), 10);
    /// ```
    pub fn len(&self) -> usize {
        self.len
    }
}

impl Add for Span {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.offset <= rhs.offset {
            if self.offset + self.len < rhs.offset + rhs.len {
                Self { offset: self.offset, len: rhs.offset - self.offset + rhs.len }
            } else {
                Self { offset: self.offset, len: self.len }
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
    use crate::Span;

    #[test]
    fn has_distance() {
        let l = Span::new(0, 3);
        let r = Span::new(5, 3);
        assert_eq!(Span::new(0, 8), l + r);
        assert_eq!(Span::new(0, 8), r + l);
    }

    #[test]
    fn crossing() {
        let l = Span::new(0, 6);
        let r = Span::new(5, 3);
        assert_eq!(Span::new(0, 8), l + r);
        assert_eq!(Span::new(0, 8), r + l);
    }

    #[test]
    fn contain() {
        let l = Span::new(0, 10);
        let r = Span::new(2, 3);
        assert_eq!(Span::new(0, 10), l + r);
        assert_eq!(Span::new(0, 10), r + l);
    }

    #[test]
    fn same_span() {
        let l = Span::new(0, 10);
        assert_eq!(l, l + l);
    }
}
