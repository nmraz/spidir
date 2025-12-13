use alloc::collections::VecDeque;
use cranelift_entity::EntityRef;

use crate::set::DenseEntitySet;

#[derive(Clone)]
pub struct Worklist<E> {
    worklist: VecDeque<E>,
    workset: DenseEntitySet<E>,
}

impl<E: EntityRef> Worklist<E> {
    pub fn new() -> Self {
        Self {
            worklist: VecDeque::new(),
            workset: DenseEntitySet::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.worklist.is_empty()
    }

    pub fn enqueue(&mut self, entity: E) {
        if !self.workset.contains(entity) {
            self.worklist.push_back(entity);
        }
    }

    pub fn dequeue(&mut self) -> Option<E> {
        let entity = self.worklist.pop_front()?;
        self.workset.remove(entity);
        Some(entity)
    }
}

impl<E: EntityRef> Default for Worklist<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E: EntityRef> FromIterator<E> for Worklist<E> {
    fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
        let worklist: VecDeque<_> = iter.into_iter().collect();
        let workset: DenseEntitySet<_> = worklist.iter().copied().collect();
        Self { worklist, workset }
    }
}

impl<E: EntityRef> Extend<E> for Worklist<E> {
    fn extend<T: IntoIterator<Item = E>>(&mut self, iter: T) {
        for entity in iter {
            self.enqueue(entity);
        }
    }
}
