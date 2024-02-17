use core::{cmp::Ordering, iter::Peekable, slice};

use alloc::collections::{btree_map, BTreeMap};

use super::types::{ProgramPoint, ProgramRange, RangeEndKey};

pub trait RangeKeyIter {
    type Value: Copy;

    fn current(&mut self) -> Option<(ProgramRange, &Self::Value)>;
    fn next(&mut self);
    fn skip_to_endpoint_above(&mut self, pos: ProgramPoint);
}

pub struct BTreeRangeIter<'a, V> {
    tree: &'a BTreeMap<RangeEndKey, V>,
    iter: Peekable<btree_map::Range<'a, RangeEndKey, V>>,
}

impl<'a, V: Copy> RangeKeyIter for BTreeRangeIter<'a, V> {
    type Value = V;

    fn current(&mut self) -> Option<(ProgramRange, &Self::Value)> {
        self.iter.peek().map(|&(key, value)| (key.0, value))
    }

    fn next(&mut self) {
        self.iter.next();
    }

    fn skip_to_endpoint_above(&mut self, pos: ProgramPoint) {
        self.iter = self.tree.range(RangeEndKey::point(pos)..).peekable();
    }
}

pub fn iter_btree_ranges<V>(tree: &BTreeMap<RangeEndKey, V>) -> BTreeRangeIter<'_, V> {
    BTreeRangeIter {
        tree,
        iter: tree.range(..).peekable(),
    }
}

pub struct SliceRangeIter<'a, R, F> {
    key_func: F,
    iter: slice::Iter<'a, R>,
}

impl<'a, R, F, V: Copy> RangeKeyIter for SliceRangeIter<'a, R, F>
where
    F: FnMut(&R) -> (ProgramRange, &V),
{
    type Value = V;

    fn current(&mut self) -> Option<(ProgramRange, &Self::Value)> {
        self.iter.as_slice().first().map(&mut self.key_func)
    }

    fn next(&mut self) {
        self.iter.next();
    }

    fn skip_to_endpoint_above(&mut self, pos: ProgramPoint) {
        let slice = self.iter.as_slice();
        let i = slice
            .binary_search_by_key(&pos, |range_obj| (self.key_func)(range_obj).0.end)
            .unwrap_or_else(|i| i);
        self.iter = slice[i..].iter();
    }
}

pub fn iter_slice_ranges<R, F, V>(slice: &[R], key_func: F) -> SliceRangeIter<R, F>
where
    F: FnMut(&R) -> (ProgramRange, &V),
{
    SliceRangeIter {
        key_func,
        iter: slice.iter(),
    }
}

pub fn iter_conflicts<L, R>(mut lhs: L, mut rhs: R) -> ConflictIter<L, R>
where
    L: RangeKeyIter,
    R: RangeKeyIter,
{
    // Discard "head" sequences on either list that completely precede the first item in the other
    // list. At most one of these skips will do something.
    if let Some((lhs_cur, _)) = lhs.current() {
        rhs.skip_to_endpoint_above(lhs_cur.start);
        if let Some((rhs_cur, _)) = rhs.current() {
            lhs.skip_to_endpoint_above(rhs_cur.start);
        }
    }

    ConflictIter { lhs, rhs }
}

pub struct ConflictIter<L, R> {
    lhs: L,
    rhs: R,
}

impl<L: RangeKeyIter, R: RangeKeyIter> Iterator for ConflictIter<L, R> {
    type Item = ((ProgramRange, L::Value), (ProgramRange, R::Value));

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (lhs_range, lhs_value) = self.lhs.current()?;
            let (rhs_range, rhs_value) = self.rhs.current()?;

            let conflict = if lhs_range.intersects(rhs_range) {
                Some(((lhs_range, *lhs_value), (rhs_range, *rhs_value)))
            } else {
                None
            };

            match lhs_range.end.cmp(&rhs_range.end) {
                Ordering::Less => {
                    // The LHS range ended below the RHS range, so try to get the LHS iterator back
                    // to a point where it ends above the start of the RHS range for the next
                    // iteration. Doing this might completely "miss" the RHS range (there might be
                    // no range intersecting it), but that will be handled correctly later.
                    advance_range_iter(&mut self.lhs, rhs_range.start);
                }
                Ordering::Greater => {
                    // Conversely, if the RHS range ended below the LHS, catch up with the RHS
                    // iterator.
                    advance_range_iter(&mut self.rhs, lhs_range.start);
                }
                Ordering::Equal => {
                    // Both ranges ended at the same point; advance both iterators and check again.
                    self.lhs.next();
                    self.rhs.next();
                }
            }

            if let Some(conflict) = conflict {
                return Some(conflict);
            }
        }
    }
}

fn advance_range_iter(iter: &mut impl RangeKeyIter, pos: ProgramPoint) {
    const MAX_NEXT_CALLS: usize = 8;

    // Try to skip a few ranges for the dense case, keeping things linear.
    for _ in 0..MAX_NEXT_CALLS {
        iter.next();

        if let Some((current, _)) = iter.current() {
            if current.end > pos {
                return;
            }
        } else {
            // Nothing to do if we've exhausted the iterator.
            return;
        }
    }

    // Use the asymptotically more efficient `skip_to_endpoint_above` for large gaps.
    iter.skip_to_endpoint_above(pos);
}
