use core::ops::Index;

use cranelift_entity::{EntityList, ListPool, PrimaryMap};

use crate::{
    entities::{Instruction, Operand, Region, Value},
    fx::FxHashMap,
    instruction::{
        InstructionData, InstructionPool, Opcode, OperandData, OperandPool, Type, ValueData,
        ValuePool,
    },
};

// Regions have more complex invariants, so we don't allow direct mutable access
pub struct Regions(PrimaryMap<Region, RegionData>);

impl Regions {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> cranelift_entity::Iter<'_, Region, RegionData> {
        self.0.iter()
    }
}

impl Index<Region> for Regions {
    type Output = RegionData;

    fn index(&self, index: Region) -> &RegionData {
        &self.0[index]
    }
}

pub struct RegionData {
    pub start: Instruction,
}

pub struct Function {
    pub instrs: PrimaryMap<Instruction, InstructionData>,
    pub operands: PrimaryMap<Operand, OperandData>,
    pub regions: Regions,
    pub ctrl_dep_pool: InstructionPool,
    pub output_pool: ValuePool,
    pub input_pool: OperandPool,
    values: PrimaryMap<Value, ValueData>,
    regions_by_start: FxHashMap<Instruction, Region>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            instrs: PrimaryMap::new(),
            operands: PrimaryMap::new(),
            values: PrimaryMap::new(),
            regions: Regions(PrimaryMap::new()),
            ctrl_dep_pool: ListPool::new(),
            output_pool: ListPool::new(),
            input_pool: ListPool::new(),
            regions_by_start: FxHashMap::default(),
        }
    }

    pub fn create_instr(
        &mut self,
        opcode: Opcode,
        ctrl_deps: &[Instruction],
        inputs: &[OperandData],
        output_types: &[Type],
    ) -> Instruction {
        let ctrl_deps = EntityList::from_slice(ctrl_deps, &mut self.ctrl_dep_pool);

        let dest_regions = inputs.iter().filter_map(|op| match op {
            &OperandData::RegionCall { region, .. } => Some(region),
            _ => None,
        });

        let inputs = EntityList::from_iter(
            inputs.iter().map(|&input| self.operands.push(input)),
            &mut self.input_pool,
        );
        let output_values = output_types
            .iter()
            .map(|&ty| self.values.push(ValueData { ty }));
        let outputs = EntityList::from_iter(output_values, &mut self.output_pool);
        let instr = self.instrs.push(InstructionData {
            opcode,
            ctrl_deps,
            inputs,
            outputs,
        });

        for region in dest_regions {
            let region_start = self.region_start(region);
            self.instrs[region_start]
                .ctrl_deps
                .push(instr, &mut self.ctrl_dep_pool);
        }

        instr
    }

    pub fn create_region(&mut self, param_types: &[Type]) -> Region {
        let region_start = self.create_instr(Opcode::StartRegion, &[], &[], param_types);
        let region = self.regions.0.push(RegionData {
            start: region_start,
        });
        self.regions_by_start.insert(region_start, region);
        region
    }

    pub fn instr_outputs(&self, instr: Instruction) -> &[Value] {
        self.instrs[instr].outputs(&self.output_pool)
    }

    pub fn value_type(&self, value: Value) -> Type {
        self.values[value].ty
    }

    pub fn region_for_start(&self, instr: Instruction) -> Region {
        self.regions_by_start[&instr]
    }

    pub fn region_start(&self, region: Region) -> Instruction {
        self.regions[region].start
    }

    pub fn region_params(&self, region: Region) -> &[Value] {
        self.instr_outputs(self.region_start(region))
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}
