#![cfg_attr(not(test), no_std)]

use core::marker::PhantomData;

use cranelift_bitset::CompoundBitSet;
use cranelift_entity::EntityRef;

#[derive(Clone, Debug)]
pub struct DenseEntitySet<E> {
    bitset: CompoundBitSet,
    _marker: PhantomData<E>,
}

impl<E: EntityRef> DenseEntitySet<E> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            bitset: CompoundBitSet::with_capacity(capacity),
            _marker: PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.bitset.clear();
    }

    pub fn contains(&self, entity: E) -> bool {
        self.bitset.contains(entity.index())
    }

    pub fn insert(&mut self, entity: E) {
        self.bitset.insert(entity.index());
    }

    pub fn remove(&mut self, entity: E) {
        self.bitset.remove(entity.index());
    }

    pub fn iter(&self) -> Iter<'_, E> {
        Iter::<E> {
            inner: self.bitset.iter(),
            _marker: PhantomData,
        }
    }
}

pub struct Iter<'a, E> {
    inner: cranelift_bitset::compound::Iter<'a>,
    _marker: PhantomData<E>,
}

impl<E: EntityRef> Iterator for Iter<'_, E> {
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(E::new)
    }
}

impl<E> Default for DenseEntitySet<E> {
    fn default() -> Self {
        Self {
            bitset: CompoundBitSet::default(),
            _marker: PhantomData,
        }
    }
}

impl<E: EntityRef> FromIterator<E> for DenseEntitySet<E> {
    fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let (min_size, _) = iter.size_hint();
        let mut set = DenseEntitySet::with_capacity(min_size);
        for entity in iter {
            set.insert(entity);
        }
        set
    }
}
