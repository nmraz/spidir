use core::ops::Index;

use cranelift_entity::{EntityList, ListPool, PrimaryMap};

use crate::{
    entities::{Instruction, Operand, Region, Value},
    fx::FxHashMap,
    instruction::{
        InstructionData, InstructionPool, Opcode, OperandData, OperandPool, Type, ValuePool,
    },
    value::{Uses, ValueMap},
};

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

pub struct Instrs(PrimaryMap<Instruction, InstructionData>);

impl Instrs {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> cranelift_entity::Iter<'_, Instruction, InstructionData> {
        self.0.iter()
    }
}

impl Index<Instruction> for Instrs {
    type Output = InstructionData;

    fn index(&self, index: Instruction) -> &InstructionData {
        &self.0[index]
    }
}

pub struct RegionData {
    pub start: Instruction,
}

pub struct Function {
    pub instrs: Instrs,
    pub regions: Regions,
    operands: PrimaryMap<Operand, OperandData>,
    ctrl_dep_pool: InstructionPool,
    value_pool: ValuePool,
    input_pool: OperandPool,
    values: ValueMap,
    regions_by_start: FxHashMap<Instruction, Region>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            instrs: Instrs(PrimaryMap::new()),
            operands: PrimaryMap::new(),
            regions: Regions(PrimaryMap::new()),
            ctrl_dep_pool: ListPool::new(),
            value_pool: ListPool::new(),
            input_pool: ListPool::new(),
            values: ValueMap::new(),
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

        let compressed_inputs = EntityList::from_iter(
            inputs.iter().map(|&input| self.operands.push(input)),
            &mut self.input_pool,
        );
        let output_values = output_types.iter().map(|&ty| self.values.create_value(ty));
        let outputs = EntityList::from_iter(output_values, &mut self.value_pool);
        let instr = self.instrs.0.push(InstructionData {
            opcode,
            ctrl_deps,
            inputs: compressed_inputs,
            outputs,
        });

        for (i, input) in inputs.iter().enumerate() {
            match *input {
                OperandData::Value(value) => self.values.record_use(value, instr, i as u8, 0),
                OperandData::RegionCall { region, args } => {
                    // Record all uses
                    for (j, &arg) in args.as_slice(&self.value_pool).iter().enumerate() {
                        self.values.record_use(arg, instr, i as u8, j as u8);
                    }

                    // Fix up control dependencies
                    let region_start = self.region_start(region);
                    self.instrs.0[region_start]
                        .ctrl_deps
                        .push(instr, &mut self.ctrl_dep_pool);
                }
                _ => {}
            }
        }

        instr
    }

    pub fn create_region_call_operand(&mut self, region: Region, args: &[Value]) -> OperandData {
        let args = EntityList::from_slice(args, &mut self.value_pool);
        OperandData::RegionCall { region, args }
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
        self.instrs[instr].outputs(&self.value_pool)
    }

    pub fn value_type(&self, value: Value) -> Type {
        self.values.value_type(value)
    }

    pub fn value_uses(&self, value: Value) -> Uses<'_> {
        self.values.uses(value)
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
