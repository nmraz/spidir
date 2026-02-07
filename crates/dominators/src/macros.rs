macro_rules! define_param_entity {
    ($entity:ident<$param:ident>, $display_prefix:expr) => {
        pub struct $entity<$param>(u32, core::marker::PhantomData<$param>);

        impl<$param> core::clone::Clone for $entity<$param> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<$param> core::marker::Copy for $entity<$param> {}

        impl<$param> core::cmp::PartialEq for $entity<$param> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl<$param> core::cmp::Eq for $entity<$param> {}

        impl<$param> core::hash::Hash for $entity<$param> {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<$param> cranelift_entity::EntityRef for $entity<$param> {
            #[inline]
            fn new(index: usize) -> Self {
                debug_assert!(index < (u32::MAX as usize));
                Self(index as u32, core::marker::PhantomData)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl<$param> cranelift_entity::packed_option::ReservedValue for $entity<$param> {
            #[inline]
            fn reserved_value() -> Self {
                $entity(u32::MAX, core::marker::PhantomData)
            }

            #[inline]
            fn is_reserved_value(&self) -> bool {
                self.0 == u32::MAX
            }
        }

        impl<$param> core::fmt::Display for $entity<$param> {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, concat!($display_prefix, "{}"), self.0)
            }
        }

        impl<$param> core::fmt::Debug for $entity<$param> {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                (self as &dyn core::fmt::Display).fmt(f)
            }
        }

        impl<$param> $entity<$param> {
            #[inline]
            pub fn from_u32(x: u32) -> Self {
                debug_assert!(x < u32::MAX);
                Self(x, core::marker::PhantomData)
            }

            #[inline]
            pub fn as_u32(self) -> u32 {
                self.0
            }
        }
    };
}

pub(crate) use define_param_entity;
