#![cfg_attr(not(test), no_std)]

use graphwalk::{Graph, PredGraph};

extern crate alloc;

pub mod domtree;
pub mod loops;

pub trait IntoCfg {
    type Node: Copy;
    type Cfg: PredGraph<Node = Self::Node>;

    fn into_cfg(self) -> Self::Cfg;
}

impl<G: PredGraph> IntoCfg for G {
    type Node = <Self as Graph>::Node;
    type Cfg = Self;

    #[inline]
    fn into_cfg(self) -> Self {
        self
    }
}
