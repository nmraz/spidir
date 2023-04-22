use cranelift_entity::{packed_option::PackedOption, PrimaryMap};

use crate::{
    entities::{Instruction, Value, ValueUse},
    instruction::Type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueData {
    ty: Type,
    first_use: PackedOption<ValueUse>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ValueUseData {
    value: PackedOption<Value>,
    prev: PackedOption<ValueUse>,
    next: PackedOption<ValueUse>,
    instr: Instruction,
    input: u8,
    sub_input: u8, // Used only for basic block calls, where a single input can use many values
}

pub struct UseInfo {
    pub instr: Instruction,
    pub input: u8,
    pub sub_input: u8,
}

pub struct Uses<'a> {
    value_map: &'a ValueMap,
    cur: Option<ValueUse>,
}

impl<'a> Iterator for Uses<'a> {
    type Item = UseInfo;

    fn next(&mut self) -> Option<UseInfo> {
        if let Some(vuse) = self.cur {
            let vuse = self.value_map.uses[vuse];
            self.cur = vuse.next.expand();
            return Some(UseInfo {
                instr: vuse.instr,
                input: vuse.input,
                sub_input: vuse.sub_input,
            });
        }

        None
    }
}

pub struct ValueMap {
    values: PrimaryMap<Value, ValueData>,
    uses: PrimaryMap<ValueUse, ValueUseData>,
}

impl ValueMap {
    pub fn new() -> Self {
        Self {
            values: PrimaryMap::new(),
            uses: PrimaryMap::new(),
        }
    }

    pub fn create_value(&mut self, ty: Type) -> Value {
        self.values.push(ValueData {
            ty,
            first_use: None.into(),
        })
    }

    pub fn value_type(&self, value: Value) -> Type {
        self.values[value].ty
    }

    pub fn record_use(&mut self, value: Value, instr: Instruction, input: u8, sub_input: u8) {
        let vuse = self.uses.push(ValueUseData {
            value: value.into(),
            prev: None.into(),
            next: self.values[value].first_use,
            instr,
            input,
            sub_input,
        });
        self.values[value].first_use = vuse.into();
    }

    pub fn uses(&self, value: Value) -> Uses<'_> {
        Uses {
            value_map: self,
            cur: self.values[value].first_use.expand(),
        }
    }

    pub fn remove_use(&mut self, vuse: ValueUse) {
        let value = self.uses[vuse].value.expand().expect("use already removed");

        let vuse_data = &mut self.uses[vuse];
        let prev = vuse_data.prev.take();
        let next = vuse_data.next.take();
        vuse_data.value = None.into();

        match prev {
            None => self.values[value].first_use = next.into(),
            Some(p) => self.uses[p].next = next.into(),
        }

        if let Some(n) = next {
            self.uses[n].prev = prev.into()
        }
    }
}

#[cfg(test)]
mod tests {
    use core::mem;

    use super::*;

    #[test]
    fn use_data_size() {
        assert_eq!(mem::size_of::<ValueUseData>(), 20);
    }
}
