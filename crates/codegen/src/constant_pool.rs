use core::{
    cmp,
    hash::{Hash, Hasher},
};

use alloc::vec::Vec;

use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl};
use fx_utils::FxHasher;
use hashbrown::{HashTable, hash_table::Entry};

use crate::{lir::MemLayout, num_utils::align_up};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constant(u32);
entity_impl!(Constant);

pub struct ConstantPool {
    pub align: u32,
    pub data: Vec<u8>,
    pub offsets: SecondaryMap<Constant, u32>,
}

struct BuilderConstantData {
    offset: u32,
    layout: MemLayout,
    hash: u64,
}

pub struct ConstantPoolBuilder {
    data: Vec<u8>,
    constants: PrimaryMap<Constant, BuilderConstantData>,
    interned_constants: HashTable<Constant>,
}

impl ConstantPoolBuilder {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            constants: PrimaryMap::new(),
            interned_constants: HashTable::new(),
        }
    }

    pub fn get_constant(&mut self, align: u32, data: &[u8]) -> Constant {
        let hash = hash_data(data);

        let entry = self.interned_constants.entry(
            hash,
            |&c| {
                let offset = self.constants[c].offset as usize;
                let size = self.constants[c].layout.size as usize;
                data == &self.data[offset..offset + size]
            },
            |&c| self.constants[c].hash,
        );

        match entry {
            Entry::Occupied(entry) => {
                let c = *entry.get();
                self.constants[c].layout.align = self.constants[c].layout.align.max(align);
                c
            }
            Entry::Vacant(entry) => {
                let offset: u32 = self.data.len().try_into().unwrap();
                let size: u32 = data.len().try_into().unwrap();
                self.data.extend_from_slice(data);
                let c = self.constants.push(BuilderConstantData {
                    offset,
                    layout: MemLayout { size, align },
                    hash,
                });
                entry.insert(c);
                c
            }
        }
    }

    pub fn finish(self) -> ConstantPool {
        let mut constants: Vec<_> = self.constants.keys().collect();
        let mut offsets = SecondaryMap::with_capacity(self.constants.len());
        let mut data = Vec::with_capacity(self.data.len());

        constants.sort_unstable_by_key(|&c| {
            let layout = self.constants[c].layout;
            // Sort by alignment, break ties with size.
            let key = ((layout.align as u64) << 32) | layout.size as u64;
            cmp::Reverse(key)
        });

        let mut size = 0;
        let mut align = 1;
        for &c in &constants {
            let data_offset = self.constants[c].offset as usize;

            let layout = self.constants[c].layout;
            align = align.max(layout.align);
            let offset = align_up(size, layout.align);

            debug_assert!(offset >= size);
            data.resize(offset as usize, 0);
            data.extend_from_slice(&self.data[data_offset..data_offset + layout.size as usize]);

            offsets[c] = offset;
            size = offset + layout.size;
        }

        ConstantPool {
            align,
            data,
            offsets,
        }
    }
}

fn hash_data(data: &[u8]) -> u64 {
    let mut hasher = FxHasher::default();
    data.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let mut builder = ConstantPoolBuilder::new();
        let c1 = builder.get_constant(1, &[0x12, 0x34]);
        let c2 = builder.get_constant(4, &[0xde, 0xad, 0xbe, 0xef]);
        let pool = builder.finish();

        assert_eq!(pool.align, 4);
        assert_eq!(pool.offsets[c1], 4);
        assert_eq!(pool.offsets[c2], 0);
        assert_eq!(&pool.data, &[0xde, 0xad, 0xbe, 0xef, 0x12, 0x34]);
    }

    #[test]
    fn pad() {
        let mut builder = ConstantPoolBuilder::new();
        let c1 = builder.get_constant(2, &[0x12, 0x34]);
        let c2 = builder.get_constant(4, &[0xde, 0xad, 0xbe]);
        let pool = builder.finish();

        assert_eq!(pool.align, 4);
        assert_eq!(pool.offsets[c1], 4);
        assert_eq!(pool.offsets[c2], 0);
        assert_eq!(&pool.data, &[0xde, 0xad, 0xbe, 0x00, 0x12, 0x34]);
    }

    #[test]
    fn intern() {
        let mut builder = ConstantPoolBuilder::new();
        let c1 = builder.get_constant(2, &[0x12, 0x34]);
        let c2 = builder.get_constant(2, &[0x12, 0x34]);
        assert!(c1 == c2);

        let pool = builder.finish();

        assert_eq!(pool.align, 2);
        assert_eq!(&pool.data, &[0x12, 0x34]);
    }

    #[test]
    fn intern_realign() {
        let mut builder = ConstantPoolBuilder::new();
        let c1 = builder.get_constant(2, &[0xde, 0xad, 0xbe, 0xef]);
        let c2 = builder.get_constant(4, &[0xde, 0xad, 0xbe, 0xef]);
        assert!(c1 == c2);

        let pool = builder.finish();

        assert_eq!(pool.align, 4);
        assert_eq!(&pool.data, &[0xde, 0xad, 0xbe, 0xef]);
    }
}
