use entity_set::DenseEntitySet;

use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir},
    target::x64::X64Machine,
};

pub struct BlockFlagLivenessTracker {
    liveness: Option<DenseEntitySet<Instr>>,
}

impl BlockFlagLivenessTracker {
    pub fn new() -> Self {
        Self { liveness: None }
    }

    pub fn reset_block(&mut self) {
        self.liveness = None;
    }

    pub fn flags_live_before(
        &mut self,
        cfg_ctx: &CfgContext,
        lir: &Lir<X64Machine>,
        pos: Instr,
    ) -> bool {
        let liveness = self.liveness.get_or_insert_with(|| {
            let mut liveness = DenseEntitySet::new();

            let block = lir.instr_block_index(pos);
            let block = cfg_ctx.block_order[block];

            let mut flags_live = false;
            for instr in lir.block_instrs(block).into_iter().rev() {
                let instr_data = lir.instr_data(instr);

                // Note: instructions that both define and use flags keep them live.
                if instr_data.uses_flags() {
                    flags_live = true;
                } else if instr_data.defines_flags() {
                    flags_live = false;
                }

                if flags_live {
                    liveness.insert(instr);
                }
            }

            liveness
        });

        liveness.contains(pos)
    }
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;

    use crate::{
        cfg::{Block, BlockCfg},
        lir::{
            Builder as LirBuilder, DefOperand, OperandPos, UseOperandConstraint,
            test_utils::push_instr,
        },
        target::x64::{AluBinOp, AluCommBinOp, CondCode, OperandSize, RC_GPR64, REG_RAX, X64Instr},
    };

    use super::*;

    struct CheckContext {
        cfg_ctx: CfgContext,
        lir: Lir<X64Machine>,
        tracker: BlockFlagLivenessTracker,
    }

    impl CheckContext {
        fn flags_live_before(&mut self, pos: Instr) -> bool {
            self.tracker
                .flags_live_before(&self.cfg_ctx, &self.lir, pos)
        }
    }

    fn check_flag_liveness(
        build: impl FnOnce(&mut LirBuilder<'_, X64Machine>),
        check: impl FnOnce(&mut CheckContext),
    ) {
        let mut cfg = BlockCfg::new();
        let entry = cfg.create_block();

        let cfg_ctx = CfgContext::compute(cfg, entry);

        let mut builder = LirBuilder::new(&cfg_ctx.block_order);
        builder.advance_block();
        build(&mut builder);
        builder.advance_block();
        let lir = builder.finish();

        let mut ctx = CheckContext {
            cfg_ctx,
            lir,
            tracker: BlockFlagLivenessTracker::new(),
        };

        check(&mut ctx);
    }

    #[test]
    fn flags_live_before_jcc() {
        check_flag_liveness(
            |builder| {
                push_instr(
                    builder,
                    X64Instr::Jumpcc(CondCode::E, Block::new(0), Block::new(0)),
                    [],
                    [],
                );
                let [a, _] = push_instr(
                    builder,
                    X64Instr::AluRRm(OperandSize::S32, AluBinOp::Cmp),
                    [],
                    [
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
            },
            |ctx| {
                // xor aaa, bbb
                assert!(!ctx.flags_live_before(Instr::new(0)));
                // cmp aaa, ccc
                assert!(!ctx.flags_live_before(Instr::new(1)));
                // je
                assert!(ctx.flags_live_before(Instr::new(2)));
            },
        );
    }

    #[test]
    fn flags_live_before_setcc() {
        check_flag_liveness(
            |builder| {
                let [a] = push_instr(
                    builder,
                    X64Instr::Ret,
                    [],
                    [(
                        RC_GPR64,
                        UseOperandConstraint::Fixed(REG_RAX),
                        OperandPos::Early,
                    )],
                );
                push_instr(
                    builder,
                    X64Instr::Setcc(CondCode::E),
                    [DefOperand::any_reg(a)],
                    [(
                        RC_GPR64,
                        UseOperandConstraint::TiedToDef(0),
                        OperandPos::Early,
                    )],
                );
                let [a, _] = push_instr(
                    builder,
                    X64Instr::AluRRm(OperandSize::S32, AluBinOp::Cmp),
                    [],
                    [
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
            },
            |ctx| {
                // xor aaa, bbb
                assert!(!ctx.flags_live_before(Instr::new(0)));
                // cmp aaa, ccc
                assert!(!ctx.flags_live_before(Instr::new(1)));
                // sete aaa
                assert!(ctx.flags_live_before(Instr::new(2)));
                // ret
                assert!(!ctx.flags_live_before(Instr::new(3)));
            },
        );
    }

    #[test]
    fn flags_live_across_setcc() {
        check_flag_liveness(
            |builder| {
                push_instr(
                    builder,
                    X64Instr::Jumpcc(CondCode::E, Block::new(0), Block::new(0)),
                    [],
                    [],
                );
                let a = builder.create_vreg(RC_GPR64);
                push_instr(
                    builder,
                    X64Instr::Setcc(CondCode::E),
                    [DefOperand::any_reg(a)],
                    [(
                        RC_GPR64,
                        UseOperandConstraint::TiedToDef(0),
                        OperandPos::Early,
                    )],
                );
                let [a, _] = push_instr(
                    builder,
                    X64Instr::AluRRm(OperandSize::S32, AluBinOp::Cmp),
                    [],
                    [
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
            },
            |ctx| {
                // xor aaa, bbb
                assert!(!ctx.flags_live_before(Instr::new(0)));
                // cmp aaa, ccc
                assert!(!ctx.flags_live_before(Instr::new(1)));
                // sete aaa
                assert!(ctx.flags_live_before(Instr::new(2)));
                // je
                assert!(ctx.flags_live_before(Instr::new(3)));
            },
        );
    }

    #[test]
    fn flags_live_across_movi() {
        check_flag_liveness(
            |builder| {
                push_instr(
                    builder,
                    X64Instr::Jumpcc(CondCode::E, Block::new(0), Block::new(0)),
                    [],
                    [],
                );

                let a = builder.create_vreg(RC_GPR64);
                push_instr(builder, X64Instr::MovRmS32(12), [DefOperand::any(a)], []);

                let [a, _] = push_instr(
                    builder,
                    X64Instr::AluRRm(OperandSize::S32, AluBinOp::Cmp),
                    [],
                    [
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
            },
            |ctx| {
                // xor aaa, bbb
                assert!(!ctx.flags_live_before(Instr::new(0)));
                // cmp aaa, ccc
                assert!(!ctx.flags_live_before(Instr::new(1)));
                // mov ddd, 12
                assert!(ctx.flags_live_before(Instr::new(2)));
                // je
                assert!(ctx.flags_live_before(Instr::new(3)));
            },
        );
    }

    #[test]
    fn flags_not_live_across_xor() {
        check_flag_liveness(
            |builder| {
                push_instr(
                    builder,
                    X64Instr::Jumpcc(CondCode::E, Block::new(0), Block::new(0)),
                    [],
                    [],
                );

                let a = builder.create_vreg(RC_GPR64);
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );

                let [a, _] = push_instr(
                    builder,
                    X64Instr::AluRRm(OperandSize::S32, AluBinOp::Cmp),
                    [],
                    [
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                push_instr(
                    builder,
                    X64Instr::AluCommRR(OperandSize::S32, AluCommBinOp::Xor),
                    [DefOperand::any(a)],
                    [
                        (
                            RC_GPR64,
                            UseOperandConstraint::TiedToDef(0),
                            OperandPos::Early,
                        ),
                        (RC_GPR64, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
            },
            |ctx| {
                // xor aaa, bbb
                assert!(!ctx.flags_live_before(Instr::new(0)));
                // cmp aaa, ccc
                assert!(!ctx.flags_live_before(Instr::new(1)));
                // xor ddd, eee
                assert!(!ctx.flags_live_before(Instr::new(2)));
                // je
                assert!(ctx.flags_live_before(Instr::new(3)));
            },
        );
    }
}
