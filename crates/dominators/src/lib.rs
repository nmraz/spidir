#![cfg_attr(not(test), no_std)]

use graphwalk::{GraphRef, PredGraphRef};

extern crate alloc;

pub mod depth_map;
pub mod domtree;
pub mod loops;

pub trait IntoCfg {
    type Node: Copy;
    type Cfg: PredGraphRef<Node = Self::Node>;

    fn into_cfg(self) -> Self::Cfg;
}

impl<G: PredGraphRef> IntoCfg for G {
    type Node = <Self as GraphRef>::Node;
    type Cfg = Self;

    #[inline]
    fn into_cfg(self) -> Self {
        self
    }
}
