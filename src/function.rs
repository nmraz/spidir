use cranelift_entity::{EntityList, ListPool, PrimaryMap};
use smallvec::SmallVec;

use crate::{
    entities::{Instruction, Operand, Region, Value},
    fx::FxHashMap,
    instruction::{
        InstructionData, InstructionList, Opcode, OperandData, OperandList, Type, ValueData,
        ValueList,
    },
};

pub struct RegionData {
    pub start: Instruction,
}

pub struct Function {
    instrs: PrimaryMap<Instruction, InstructionData>,
    operands: PrimaryMap<Operand, OperandData>,
    values: PrimaryMap<Value, ValueData>,
    regions: PrimaryMap<Region, RegionData>,
    regions_by_start: FxHashMap<Instruction, Region>,
    ctrl_dep_pool: ListPool<Instruction>,
    output_pool: ListPool<Value>,
    input_pool: ListPool<Operand>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            instrs: PrimaryMap::new(),
            operands: PrimaryMap::new(),
            values: PrimaryMap::new(),
            regions: PrimaryMap::new(),
            regions_by_start: FxHashMap::default(),
            ctrl_dep_pool: ListPool::new(),
            output_pool: ListPool::new(),
            input_pool: ListPool::new(),
        }
    }

    pub fn instr_data(&self, instr: Instruction) -> &InstructionData {
        &self.instrs[instr]
    }

    pub fn instr_data_mut(&mut self, instr: Instruction) -> &mut InstructionData {
        &mut self.instrs[instr]
    }

    pub fn resolve_ctrl_deps(&self, ctrl_deps: InstructionList) -> &[Instruction] {
        ctrl_deps.as_slice(&self.ctrl_dep_pool)
    }

    pub fn resolve_input_list(&self, operands: OperandList) -> &[Operand] {
        operands.as_slice(&self.input_pool)
    }

    pub fn resolve_output_list(&self, values: ValueList) -> &[Value] {
        values.as_slice(&self.output_pool)
    }

    pub fn create_instr(
        &mut self,
        opcode: Opcode,
        ctrl_deps: &[Instruction],
        inputs: &[OperandData],
        output_types: &[Type],
    ) -> Instruction {
        let ctrl_deps = EntityList::from_slice(ctrl_deps, &mut self.ctrl_dep_pool);
        let inputs = EntityList::from_iter(
            inputs.iter().map(|&input| self.operands.push(input)),
            &mut self.input_pool,
        );
        let output_values = output_types.iter().map(|&ty| {
            self.values.push(ValueData {
                ty,
                uses: SmallVec::new(),
            })
        });
        let outputs = EntityList::from_iter(output_values, &mut self.output_pool);
        self.instrs.push(InstructionData {
            opcode,
            ctrl_deps,
            inputs,
            outputs,
        })
    }

    pub fn region_data(&self, region: Region) -> &RegionData {
        &self.regions[region]
    }

    pub fn create_region(&mut self, param_types: &[Type]) -> Region {
        let region_start = self.create_instr(Opcode::StartRegion, &[], &[], param_types);
        let region = self.regions.push(RegionData {
            start: region_start,
        });
        self.regions_by_start.insert(region_start, region);
        region
    }

    pub fn region_params(&self, region: Region) -> &[Value] {
        let region_start = self.region_data(region).start;
        self.resolve_output_list(self.instr_data(region_start).outputs)
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}
