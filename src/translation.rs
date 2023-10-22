use crate::{context::NessaContext, operations::{Operator, ADD_BINOP_ID, SUB_BINOP_ID, MUL_BINOP_ID, DIV_BINOP_ID, MOD_BINOP_ID, LT_BINOP_ID, GT_BINOP_ID, LTEQ_BINOP_ID, GTEQ_BINOP_ID, EQ_BINOP_ID, NEQ_BINOP_ID, OR_BINOP_ID, AND_BINOP_ID, DEREF_UNOP_ID, NEG_UNOP_ID, NOT_UNOP_ID}, types::{Type, INT_ID, FLOAT_ID, INT, BOOL_ID}, compilation::CompiledNessaExpr};

fn load_unop_opcodes<F: Fn(&Type) -> Option<CompiledNessaExpr>>(ctx: &mut NessaContext, id: usize, f: F) {
    if let Operator::Unary { operations, .. } = &ctx.unary_ops[id] {
        for (ov_id, (_, arg, _, _)) in operations.iter().enumerate() {
            if let Some(opcode) = f(arg) {
                let mut offset = arg.is_ref() as usize;

                if opcode.needs_float() {
                    offset += (*arg.deref_type() == INT) as usize;
                }

                ctx.cache.opcodes.unary.insert((id, ov_id), (opcode, offset));
            }
        }
    }
}

fn load_binop_opcodes<F: Fn(&Type, &Type) -> Option<CompiledNessaExpr>>(ctx: &mut NessaContext, id: usize, f: F) {
    if let Operator::Binary { operations, .. } = &ctx.binary_ops[id] {
        for (ov_id, (_, args, _, _)) in operations.iter().enumerate() {
            if let Type::And(types) = args {
                if let Some(opcode) = f(&types[0], &types[1]) {
                    let mut offset = (types[0].is_ref() as usize) + (types[1].is_ref() as usize);

                    if opcode.needs_float() {
                        offset += (*types[0].deref_type() == INT) as usize;
                        offset += (*types[1].deref_type() == INT) as usize;
                    }

                    ctx.cache.opcodes.binary.insert((id, ov_id), (opcode, offset));
                }
                
            } else {
                unreachable!();
            }
        }
    }
}

pub fn load_optimized_binop_opcodes(ctx: &mut NessaContext) {
    use CompiledNessaExpr::*;

    // Arithmetic
    let ids = [
        ADD_BINOP_ID, SUB_BINOP_ID, MUL_BINOP_ID, DIV_BINOP_ID, MOD_BINOP_ID,
        LT_BINOP_ID, GT_BINOP_ID, LTEQ_BINOP_ID, GTEQ_BINOP_ID, EQ_BINOP_ID, NEQ_BINOP_ID
    ];

    let opcodes = [
        (Addi, Addf), (Subi, Subf), (Muli, Mulf), (Divi, Divf), (Modi, Modf),
        (Lti, Ltf), (Gti, Gtf), (Lteqi, Lteqf), (Gteqi, Gteqf), (Eqi, Eqf), (Neqi, Neqf)
    ];

    for (id, (i_opcode, f_opcode)) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(INT_ID), Type::Basic(INT_ID)) => Some(i_opcode.clone()),
                (Type::Basic(FLOAT_ID), Type::Basic(FLOAT_ID)) => Some(f_opcode.clone()),
                _ => None
            }
        });
    }

    let ids = [OR_BINOP_ID, AND_BINOP_ID];
    let opcodes = [Or, And];

    for (id, opcode) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(BOOL_ID), Type::Basic(BOOL_ID)) => Some(opcode.clone()),
                _ => None
            }
        });
    }
}

pub fn load_optimized_unop_opcodes(ctx: &mut NessaContext) {
    use CompiledNessaExpr::*;

    ctx.cache.opcodes.unary.insert((DEREF_UNOP_ID, 0), (Copy, 0));

    let ids = [NEG_UNOP_ID];
    let opcodes = [(Negi, Negf)];

    for (id, (i_opcode, f_opcode)) in ids.iter().zip(opcodes) {
        load_unop_opcodes(ctx, *id, |a| {
            return match a.deref_type() {
                Type::Basic(INT_ID) => Some(i_opcode.clone()),
                Type::Basic(FLOAT_ID) => Some(f_opcode.clone()),
                _ => None
            }
        });
    }

    let ids = [NOT_UNOP_ID];
    let opcodes = [Not];

    for (id, opcode) in ids.iter().zip(opcodes) {
        load_unop_opcodes(ctx, *id, |a| {
            return match a.deref_type() {
                Type::Basic(BOOL_ID) => Some(opcode.clone()),
                _ => None
            }
        });
    }
}

pub fn load_optimized_fn_opcodes(ctx: &mut NessaContext) {
    use CompiledNessaExpr::*;

    let deref_id = ctx.get_function_id("deref".into()).unwrap();
    ctx.cache.opcodes.functions.insert((deref_id, 0), (Copy, 0));
}

pub fn load_optimized_opcodes(ctx: &mut NessaContext) {
    load_optimized_unop_opcodes(ctx);
    load_optimized_binop_opcodes(ctx);
    load_optimized_fn_opcodes(ctx);
}