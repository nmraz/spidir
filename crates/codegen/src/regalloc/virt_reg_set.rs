use cranelift_bitset::ScalarBitSet;
use hashbrown::hash_map::{Entry, Iter as HashMapIter};

use fx_utils::FxHashMap;

use crate::lir::VirtReg;

use super::utils::ChangeStatus;

type Word = u64;
type WordSet = ScalarBitSet<Word>;

#[derive(Default, Clone)]
pub struct VirtRegSet {
    map: FxHashMap<u32, WordSet>,
}

impl VirtRegSet {
    pub fn iter(&self) -> VirtRegSetIter<'_> {
        VirtRegSetIter {
            map_iter: self.map.iter(),
            cur_entry: None,
        }
    }

    pub fn insert(&mut self, reg: VirtReg) {
        let (word, bit) = word_bit_offset(reg.as_u32());
        self.map.entry(word).or_default().insert(bit);
    }

    pub fn contains(&self, reg: VirtReg) -> bool {
        let (word, bit) = word_bit_offset(reg.as_u32());
        self.map
            .get(&word)
            .is_some_and(|word_set| word_set.contains(bit))
    }

    pub fn union(&mut self, other: &VirtRegSet) -> ChangeStatus {
        let mut status = ChangeStatus::Unchanged;

        for (&other_word, &other_word_set) in &other.map {
            let word_set = self.map.entry(other_word).or_default();
            let old_word_set = *word_set;
            word_set.0 |= other_word_set.0;
            if *word_set != old_word_set {
                status = ChangeStatus::Changed;
            }
        }

        status
    }

    pub fn subtract(&mut self, other: &VirtRegSet) -> ChangeStatus {
        let mut status = ChangeStatus::Unchanged;

        for (&other_word, &other_word_set) in &other.map {
            if let Entry::Occupied(mut entry) = self.map.entry(other_word) {
                let mut word_set = *entry.get();
                let old_word_set = word_set;
                word_set.0 &= !other_word_set.0;
                if word_set != old_word_set {
                    status = ChangeStatus::Changed;
                }

                if word_set.is_empty() {
                    entry.remove();
                } else {
                    *entry.get_mut() = word_set;
                }
            }
        }

        status
    }
}

impl<'a> IntoIterator for &'a VirtRegSet {
    type Item = VirtReg;
    type IntoIter = VirtRegSetIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct VirtRegSetIter<'a> {
    map_iter: HashMapIter<'a, u32, WordSet>,
    cur_entry: Option<(u32, WordSet)>,
}

impl Iterator for VirtRegSetIter<'_> {
    type Item = VirtReg;

    fn next(&mut self) -> Option<Self::Item> {
        let num = loop {
            let (word_num, word_set) = match &mut self.cur_entry {
                Some(entry) => entry,
                None => {
                    self.cur_entry = self
                        .map_iter
                        .next()
                        .map(|(&word_num, &word_set)| (word_num, word_set));
                    self.cur_entry.as_mut()?
                }
            };

            let Some(inner_bit) = word_set.pop_min() else {
                self.cur_entry = None;
                continue;
            };

            break *word_num * Word::BITS + inner_bit as u32;
        };

        Some(VirtReg::from_bits(num))
    }
}

fn word_bit_offset(num: u32) -> (u32, u8) {
    ((num / Word::BITS), (num % Word::BITS) as u8)
}

#[cfg(test)]
mod tests {
    use fx_utils::FxHashSet;
    use quickcheck_macros::quickcheck;

    use super::*;

    fn reg_set_from_vals(vals: &[u32]) -> VirtRegSet {
        let mut reg_set = VirtRegSet::default();

        // Put everything we got into the set.
        for &val in vals {
            reg_set.insert(VirtReg::from_bits(val));
        }

        reg_set
    }

    fn vals_from_reg_set(reg_set: &VirtRegSet) -> Vec<u32> {
        let mut vals: Vec<_> = reg_set.iter().map(|reg| reg.as_bits()).collect();
        vals.sort();
        vals
    }

    #[quickcheck]
    fn add_iter_reg_set(mut vals: Vec<u32>) -> bool {
        let reg_set = reg_set_from_vals(&vals);

        // Get a stable representation of the input list for comparison.
        vals.sort();
        vals.dedup();

        vals == vals_from_reg_set(&reg_set)
    }

    #[quickcheck]
    fn reg_set_contains(vals: Vec<u32>, mut other_vals: Vec<u32>) -> bool {
        let val_set: FxHashSet<_> = vals.iter().copied().collect();
        other_vals.retain(|val| !val_set.contains(val));

        let reg_set = reg_set_from_vals(&vals);
        vals.iter()
            .all(|&val| reg_set.contains(VirtReg::from_bits(val)))
            && !other_vals
                .iter()
                .any(|&val| reg_set.contains(VirtReg::from_bits(val)))
    }

    #[quickcheck]
    fn union(a_vals: Vec<u32>, b_vals: Vec<u32>) -> bool {
        let a_hash_set: FxHashSet<_> = a_vals.iter().copied().collect();
        let b_hash_set: FxHashSet<_> = b_vals.iter().copied().collect();

        let union_hash_set = &a_hash_set | &b_hash_set;
        let hash_set_changed = union_hash_set != a_hash_set;
        let mut union_vals: Vec<_> = union_hash_set.into_iter().collect();
        union_vals.sort();

        let mut a_reg_set = reg_set_from_vals(&a_vals);
        let b_reg_set = reg_set_from_vals(&b_vals);
        let reg_set_changed = a_reg_set.union(&b_reg_set).is_changed();
        let union_reg_set_vals = vals_from_reg_set(&a_reg_set);

        union_vals == union_reg_set_vals && hash_set_changed == reg_set_changed
    }

    #[quickcheck]
    fn union_self(mut vals: Vec<u32>) -> bool {
        let mut reg_set = reg_set_from_vals(&vals);
        let reg_set_clone = reg_set.clone();
        let changed = reg_set.union(&reg_set_clone).is_changed();

        // Get a stable representation of the input list for comparison.
        vals.sort();
        vals.dedup();

        vals_from_reg_set(&reg_set) == vals && !changed
    }

    #[quickcheck]
    fn subtract(a_vals: Vec<u32>, b_vals: Vec<u32>) -> bool {
        let a_hash_set: FxHashSet<_> = a_vals.iter().copied().collect();
        let b_hash_set: FxHashSet<_> = b_vals.iter().copied().collect();

        let mut sub_hash_set = a_hash_set.clone();
        for &b in &b_hash_set {
            sub_hash_set.remove(&b);
        }

        let hash_set_changed = sub_hash_set != a_hash_set;
        let mut sub_vals: Vec<_> = sub_hash_set.into_iter().collect();
        sub_vals.sort();

        let mut a_reg_set = reg_set_from_vals(&a_vals);
        let b_reg_set = reg_set_from_vals(&b_vals);
        let reg_set_changed = a_reg_set.subtract(&b_reg_set).is_changed();
        let sub_reg_set_vals = vals_from_reg_set(&a_reg_set);

        sub_vals == sub_reg_set_vals && hash_set_changed == reg_set_changed
    }

    #[quickcheck]
    fn subtract_self(vals: Vec<u32>) -> bool {
        let mut reg_set = reg_set_from_vals(&vals);
        let reg_set_clone = reg_set.clone();
        let changed = reg_set.subtract(&reg_set_clone).is_changed();

        let should_change = !vals.is_empty();

        vals_from_reg_set(&reg_set) == vec![] && changed == should_change
    }
}
