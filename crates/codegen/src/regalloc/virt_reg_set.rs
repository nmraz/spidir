use alloc::{boxed::Box, vec};

use crate::lir::VirtRegNum;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeStatus {
    Unchanged,
    Changed,
}

impl ChangeStatus {
    #[inline]
    pub fn is_changed(&self) -> bool {
        matches!(self, Self::Changed)
    }
}

// TODO: Maybe this should be sparse?
#[derive(Clone)]
pub struct VirtRegSet {
    words: Box<[usize]>,
}

impl VirtRegSet {
    pub fn new(vreg_count: u32) -> Self {
        Self {
            words: vec![0; ((vreg_count + usize::BITS - 1) / usize::BITS) as usize]
                .into_boxed_slice(),
        }
    }

    pub fn clear(&mut self) {
        self.words.fill(0);
    }

    pub fn iter(&self) -> VirtRegSetIter<'_> {
        VirtRegSetIter { set: self, pos: 0 }
    }

    pub fn union(&mut self, other: &VirtRegSet) -> ChangeStatus {
        debug_assert!(self.words.len() == other.words.len());
        let mut status = ChangeStatus::Unchanged;
        for (lhs, &rhs) in self.words.iter_mut().zip(&*other.words) {
            let old_lhs = *lhs;
            *lhs |= rhs;
            if *lhs != old_lhs {
                status = ChangeStatus::Changed;
            }
        }
        status
    }

    pub fn subtract(&mut self, other: &VirtRegSet) -> ChangeStatus {
        debug_assert!(self.words.len() == other.words.len());
        let mut status = ChangeStatus::Unchanged;
        for (lhs, &rhs) in self.words.iter_mut().zip(&*other.words) {
            let old_lhs = *lhs;
            *lhs &= !rhs;
            if *lhs != old_lhs {
                status = ChangeStatus::Changed;
            }
        }
        status
    }

    pub fn contains(&self, reg: VirtRegNum) -> bool {
        let (word, bit) = word_bit_offset(reg.as_u32());
        self.words[word] >> bit != 0
    }

    pub fn add(&mut self, reg: VirtRegNum) {
        let (word, bit) = word_bit_offset(reg.as_u32());
        self.words[word] |= 1 << bit;
    }

    pub fn remove(&mut self, reg: VirtRegNum) {
        let (word, bit) = word_bit_offset(reg.as_u32());
        self.words[word] &= !(1 << bit);
    }
}

impl<'a> IntoIterator for &'a VirtRegSet {
    type Item = VirtRegNum;
    type IntoIter = VirtRegSetIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct VirtRegSetIter<'a> {
    set: &'a VirtRegSet,
    pos: u32,
}

impl<'a> Iterator for VirtRegSetIter<'a> {
    type Item = VirtRegNum;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: optimize this
        loop {
            let pos = self.pos;
            let (word_off, bit) = word_bit_offset(pos);
            let word = *self.set.words.get(word_off)?;
            self.pos += 1;
            if (word >> bit) & 1 != 0 {
                return Some(VirtRegNum::from_u32(pos));
            }
        }
    }
}

fn word_bit_offset(num: u32) -> (usize, u32) {
    ((num / usize::BITS) as usize, num % usize::BITS)
}
