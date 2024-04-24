use crate::{compilation::CompiledNessaExpr, context::NessaContext, operations::{Operator, ADD_BINOP_ID, ANDB_BINOP_ID, AND_BINOP_ID, ASSIGN_BINOP_ID, DEREF_UNOP_ID, DIV_BINOP_ID, EQ_BINOP_ID, GTEQ_BINOP_ID, GT_BINOP_ID, LTEQ_BINOP_ID, LT_BINOP_ID, MOD_BINOP_ID, MUL_BINOP_ID, NEG_UNOP_ID, NEQ_BINOP_ID, NOT_UNOP_ID, ORB_BINOP_ID, OR_BINOP_ID, SHL_BINOP_ID, SHR_BINOP_ID, SUB_BINOP_ID, XOR_BINOP_ID}, types::{Type, BOOL_ID, FLOAT_ID, INT, INT_ID, STR_ID}};

fn load_unop_opcodes<F: Fn(&Type) -> Option<CompiledNessaExpr>>(ctx: &mut NessaContext, id: usize, f: F) {
    if let Operator::Unary { operations, .. } = &ctx.unary_ops[id] {
        for (ov_id, op_ov) in operations.iter().enumerate() {
            if let Some(opcode) = f(&op_ov.args) {
                let mut offset = op_ov.args.is_ref() as usize;

                if opcode.needs_float() {
                    offset += (*op_ov.args.deref_type() == INT) as usize;
                }

                ctx.cache.opcodes.unary.insert((id, ov_id), (opcode, offset));
            }
        }
    }
}

fn load_binop_opcodes<F: Fn(&Type, &Type) -> Option<CompiledNessaExpr>>(ctx: &mut NessaContext, id: usize, f: F) {
    if let Operator::Binary { operations, .. } = &ctx.binary_ops[id] {
        for (ov_id, op_ov) in operations.iter().enumerate() {
            if let Type::And(types) = &op_ov.args {
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

    // Arithmetic and Bitwise
    let ids = [
        ADD_BINOP_ID, SUB_BINOP_ID, MUL_BINOP_ID, DIV_BINOP_ID, MOD_BINOP_ID,
        LT_BINOP_ID, GT_BINOP_ID, LTEQ_BINOP_ID, GTEQ_BINOP_ID, EQ_BINOP_ID, NEQ_BINOP_ID,
        ANDB_BINOP_ID, ORB_BINOP_ID, XOR_BINOP_ID, SHL_BINOP_ID, SHR_BINOP_ID
    ];

    let opcodes = [
        (Addi, Addf), (Subi, Subf), (Muli, Mulf), (Divi, Divf), (Modi, Modf),
        (Lti, Ltf), (Gti, Gtf), (Lteqi, Lteqf), (Gteqi, Gteqf), (Eqi, Eqf), (Neqi, Neqf),
        (AndB, Halt), (OrB, Halt), (XorB, Halt), (Shl, Halt), (Shr, Halt)
    ];

    for (id, (i_opcode, f_opcode)) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(INT_ID), Type::Basic(INT_ID)) => Some(i_opcode.clone()),
                (Type::Basic(FLOAT_ID), Type::Basic(FLOAT_ID)) => Some(f_opcode.clone()),
                (Type::Basic(INT_ID), Type::Basic(FLOAT_ID)) => Some(f_opcode.clone()),
                (Type::Basic(FLOAT_ID), Type::Basic(INT_ID)) => Some(f_opcode.clone()),
                _ => None
            }
        });
    }

    // String opcodes
    let ids = [
        EQ_BINOP_ID, NEQ_BINOP_ID, ADD_BINOP_ID
    ];

    let opcodes = [
        EqStr, NeqStr, AddStr
    ];
    
    for (id, opcode) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(STR_ID), Type::Basic(STR_ID)) => Some(opcode.clone()),
                _ => None
            }
        });
    }

    // Bool opcodes
    let ids = [
        EQ_BINOP_ID, NEQ_BINOP_ID
    ];

    let opcodes = [
        EqBool, NeqBool
    ];
    
    for (id, opcode) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(BOOL_ID), Type::Basic(BOOL_ID)) => Some(opcode.clone()),
                _ => None
            }
        });
    }

    // Logical
    let ids = [OR_BINOP_ID, AND_BINOP_ID, XOR_BINOP_ID];
    let opcodes = [Or, And, Xor];

    for (id, opcode) in ids.iter().zip(opcodes) {
        load_binop_opcodes(ctx, *id, |a, b| {
            return match (a.deref_type(), b.deref_type()) {
                (Type::Basic(BOOL_ID), Type::Basic(BOOL_ID)) => Some(opcode.clone()),
                _ => None
            }
        });
    }

    // Other
    ctx.cache.opcodes.binary.insert((ASSIGN_BINOP_ID, 0), (Assign, 0));
}

pub fn load_optimized_unop_opcodes(ctx: &mut NessaContext) {
    use CompiledNessaExpr::*;

    ctx.cache.opcodes.unary.insert((DEREF_UNOP_ID, 0), (Copy, 0));
    ctx.cache.opcodes.unary.insert((DEREF_UNOP_ID, 1), (Copy, 0));

    let ids = [NEG_UNOP_ID, NOT_UNOP_ID];
    let opcodes = [(Negi, Negf), (NotB, Halt)];

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
    ctx.cache.opcodes.functions.insert((deref_id, 1), (Copy, 0));
    
    let demut_id = ctx.get_function_id("demut".into()).unwrap();
    ctx.cache.opcodes.functions.insert((demut_id, 0), (Demut, 0));

    let move_id = ctx.get_function_id("move".into()).unwrap();
    ctx.cache.opcodes.functions.insert((move_id, 0), (Move, 0));

    let ref_id = ctx.get_function_id("ref".into()).unwrap();
    ctx.cache.opcodes.functions.insert((ref_id, 0), (Ref, 0));

    let mut_id = ctx.get_function_id("mut".into()).unwrap();
    ctx.cache.opcodes.functions.insert((mut_id, 0), (Mut, 0));

    let inc_id = ctx.get_function_id("inc".into()).unwrap();
    ctx.cache.opcodes.functions.insert((inc_id, 0), (Inc, 0));

    let dec_id = ctx.get_function_id("dec".into()).unwrap();
    ctx.cache.opcodes.functions.insert((dec_id, 0), (Dec, 0));
}

pub fn load_optimized_opcodes(ctx: &mut NessaContext) {
    load_optimized_unop_opcodes(ctx);
    load_optimized_binop_opcodes(ctx);
    load_optimized_fn_opcodes(ctx);
}