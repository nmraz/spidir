use core::hash::BuildHasherDefault;

use hashbrown::{HashMap, HashSet};
use rustc_hash::FxHasher;

pub type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxHashSet<K> = HashSet<K, BuildHasherDefault<FxHasher>>;
