use cranelift_entity::{EntityList, EntityRef, ListPool, packed_option::ReservedValue};

pub fn dedup_entity_list<E: EntityRef + ReservedValue>(
    list: &mut EntityList<E>,
    pool: &mut ListPool<E>,
) {
    let contents = list.as_mut_slice(pool);
    contents.sort_unstable_by_key(|entity| entity.index());
    let new_len = slice_utils::dedup(contents);
    list.truncate(new_len, pool);
}
