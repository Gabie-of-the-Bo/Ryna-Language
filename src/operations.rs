use crate::annotations::Annotation;
use crate::compilation::CompiledNessaExpr;
use crate::types::{Type, INT, BOOL, STR, T_0, FLOAT};
use crate::{object::*, ARR_OF};
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type UnaryFunctionInner = fn(&Vec<Type>, &Type, Object) -> Result<Object, String>;
pub type BinaryFunctionInner = fn(&Vec<Type>, &Type, Object, Object, &NessaContext) -> Result<Object, String>;
pub type NaryFunctionInner = fn((&mut Vec<Object>, &mut usize, &mut Vec<(i32, usize, i32)>, &mut i32), &Vec<Type>, &Type) -> Result<(), String>;

pub type UnaryFunction = Option<UnaryFunctionInner>;
pub type BinaryFunction = Option<BinaryFunctionInner>;
pub type NaryFunction = Option<NaryFunctionInner>;

pub type UnaryOperations = Vec<(Vec<Annotation>, usize, Type, Type, UnaryFunction)>;
pub type BinaryOperations = Vec<(Vec<Annotation>, usize, Type, Type, BinaryFunction)>;
pub type NaryOperations = Vec<(Vec<Annotation>, usize, Type, Type, NaryFunction)>;

const EMPTY_UN_FUNC: UnaryFunctionInner = |_, _, _| Ok(Object::empty());
const EMPTY_BIN_FUNC: BinaryFunctionInner = |_, _, _, _, _| Ok(Object::empty());
const EMPTY_NARY_FUNC: NaryFunctionInner = |_, _, _| Ok(());

#[derive(Clone)]
pub enum Operator {
    Unary {
        id: usize,
        representation: String,
        prefix: bool,
        precedence: usize,
        operations: UnaryOperations
    },

    Binary {
        id: usize,
        right_associative: bool,
        representation: String,
        precedence: usize,
        operations: BinaryOperations
    },

    Nary {
        id: usize,
        open_rep: String, // N-ary operators are only allowed to be enclosers, such as the call operator and the multidimensional index operator
        close_rep: String,
        precedence: usize,
        operations: NaryOperations
    }
}

impl Operator {
    pub fn get_id(&self) -> usize {
        match self {
            Operator::Unary { id, .. } => *id,
            Operator::Binary { id, .. } => *id,
            Operator::Nary { id, .. } => *id
        }
    }

    pub fn get_precedence(&self) -> usize {
        match self {
            Operator::Unary { precedence: p, .. } => *p,
            Operator::Binary { precedence: p, .. } => *p,
            Operator::Nary { precedence: p, .. } => *p
        }
    }

    pub fn get_repr(&self) -> String {
        match self {
            Operator::Unary { representation: r, .. } => r.into(),
            Operator::Binary { representation: r, .. } => r.into(),
            Operator::Nary { open_rep: o, close_rep: c, .. } => format!("{o}{c}")
        }
    }

    pub fn is_right_associative(&self) -> bool {
        match self {
            Operator::Binary { right_associative, .. } => *right_associative,
            _ => unreachable!()
        }
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

macro_rules! define_unary_native_op {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr) => {
        $ctx.define_native_unary_operation($id, 0, $inner_type, $return_type, EMPTY_UN_FUNC).unwrap();
    };
}

macro_rules! define_unary_native_op_combinations {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr) => {
        let base_ref = Type::Ref(Box::new($inner_type.clone()));
        let base_mut = Type::MutRef(Box::new($inner_type.clone()));
        
        define_unary_native_op!($ctx, $id, $inner_type, $return_type);
        define_unary_native_op!($ctx, $id, base_ref.clone(), $return_type);
        define_unary_native_op!($ctx, $id, base_mut.clone(), $return_type);
    };
}

pub const NEG_UNOP_ID: usize = 0;
pub const NOT_UNOP_ID: usize = 1;
pub const DEREF_UNOP_ID: usize = 2;

pub fn standard_unary_operations(ctx: &mut NessaContext) {
    ctx.define_unary_operator("-".into(), true, 300).unwrap();

    define_unary_native_op_combinations!(ctx, 0, INT, INT);
    define_unary_native_op_combinations!(ctx, 0, FLOAT, FLOAT);

    ctx.define_unary_operator("!".into(), true, 250).unwrap();

    define_unary_native_op_combinations!(ctx, 1, BOOL, BOOL);
    define_unary_native_op_combinations!(ctx, 1, INT, INT);

    ctx.define_unary_operator("*".into(), true, 155).unwrap();

    ctx.define_native_unary_operation(2, 1, T_0.to_mut(), T_0, EMPTY_UN_FUNC).unwrap();
    ctx.define_native_unary_operation(2, 1, T_0.to_ref(), T_0, EMPTY_UN_FUNC).unwrap();
}

macro_rules! define_binary_native_op {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr) => {
        $ctx.define_native_binary_operation($id, 0, $l_type, $r_type, $return_type, EMPTY_BIN_FUNC).unwrap();
    };
}

macro_rules! define_binary_native_op_combinations_distinct {
    ($ctx: ident, $id: expr, $base_type_1: expr, $base_type_2: expr, $return_type: expr) => {
        let base_ref_1 = Type::Ref(Box::new($base_type_1.clone()));
        let base_mut_1 = Type::MutRef(Box::new($base_type_1.clone()));
        let base_ref_2 = Type::Ref(Box::new($base_type_2.clone()));
        let base_mut_2 = Type::MutRef(Box::new($base_type_2.clone()));
        
        define_binary_native_op!($ctx, $id, $base_type_1, $base_type_2, $return_type);
        define_binary_native_op!($ctx, $id, base_ref_1.clone(), $base_type_2, $return_type);
        define_binary_native_op!($ctx, $id, $base_type_1, base_ref_2.clone(), $return_type);
        define_binary_native_op!($ctx, $id, base_mut_1.clone(), $base_type_2, $return_type);
        define_binary_native_op!($ctx, $id, $base_type_1, base_mut_2.clone(), $return_type);
        define_binary_native_op!($ctx, $id, base_ref_1.clone(), base_ref_2.clone(), $return_type);
        define_binary_native_op!($ctx, $id, base_ref_1.clone(), base_mut_2.clone(), $return_type);
        define_binary_native_op!($ctx, $id, base_mut_1.clone(), base_mut_2.clone(), $return_type);
        define_binary_native_op!($ctx, $id, base_mut_1.clone(), base_ref_2.clone(), $return_type);
    };
}

macro_rules! define_binary_native_op_combinations {
    ($ctx: ident, $id: expr, $base_type: expr, $return_type: expr) => {
        define_binary_native_op_combinations_distinct!($ctx, $id, $base_type, $base_type, $return_type);
    };
}

// Constant identifiers
pub const ADD_BINOP_ID: usize = 0;
pub const SUB_BINOP_ID: usize = 1;
pub const MUL_BINOP_ID: usize = 2;
pub const DIV_BINOP_ID: usize = 3;
pub const MOD_BINOP_ID: usize = 4;

pub const DOT_BINOP_ID: usize = 5;

pub const LT_BINOP_ID: usize = 6;
pub const GT_BINOP_ID: usize = 7;
pub const LTEQ_BINOP_ID: usize = 8;
pub const GTEQ_BINOP_ID: usize = 9;
pub const EQ_BINOP_ID: usize = 10;
pub const NEQ_BINOP_ID: usize = 11;

pub const OR_BINOP_ID: usize = 12;
pub const AND_BINOP_ID: usize = 13;
pub const XOR_BINOP_ID: usize = 19;

pub const SHR_BINOP_ID: usize = 15;
pub const SHL_BINOP_ID: usize = 16;
pub const ANDB_BINOP_ID: usize = 17;
pub const ORB_BINOP_ID: usize = 18;

pub const ASSIGN_BINOP_ID: usize = 14;

pub const LT_BINOP_PREC: usize = 900;

pub fn standard_binary_operations(ctx: &mut NessaContext) {
    
    /*
        ╒═════════════════════════════╕
        │ Basic arithmetic operations │
        ╘═════════════════════════════╛
    */

    ctx.define_binary_operator("+".into(), false, 650).unwrap();

    define_binary_native_op_combinations!(ctx, 0, INT, INT);
    define_binary_native_op_combinations!(ctx, 0, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 0, INT, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 0, FLOAT, INT, FLOAT);

    define_binary_native_op_combinations!(ctx, 0, STR, STR);

    ctx.define_binary_operator("-".into(), true, 700).unwrap();

    define_binary_native_op_combinations!(ctx, 1, INT, INT);
    define_binary_native_op_combinations!(ctx, 1, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 1, INT, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 1, FLOAT, INT, FLOAT);

    ctx.define_binary_operator("*".into(), false, 500).unwrap();

    define_binary_native_op_combinations!(ctx, 2, INT, INT);
    define_binary_native_op_combinations!(ctx, 2, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 2, INT, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 2, FLOAT, INT, FLOAT);

    ctx.define_binary_operator("/".into(), false, 550).unwrap();

    define_binary_native_op_combinations!(ctx, 3, INT, INT);
    define_binary_native_op_combinations!(ctx, 3, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 3, INT, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 3, FLOAT, INT, FLOAT);

    ctx.define_binary_operator("%".into(), false, 600).unwrap();

    define_binary_native_op_combinations!(ctx, 4, INT, INT);
    define_binary_native_op_combinations!(ctx, 4, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 4, INT, FLOAT, FLOAT);
    define_binary_native_op_combinations_distinct!(ctx, 4, FLOAT, INT, FLOAT);

    /*
        ╒══════════════════════╕
        │ Ancillary operations │
        ╘══════════════════════╛
    */

    ctx.define_binary_operator(".".into(), true, 100).unwrap();

    /*
        ╒═══════════════════════╕
        │ Comparison operations │
        ╘═══════════════════════╛
    */

    ctx.define_binary_operator("<".into(), false, 900).unwrap();

    define_binary_native_op_combinations!(ctx, 6, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 6, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 6, INT, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 6, FLOAT, INT, BOOL);

    ctx.define_binary_operator(">".into(), false, 950).unwrap();

    define_binary_native_op_combinations!(ctx, 7, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 7, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 7, INT, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 7, FLOAT, INT, BOOL);

    ctx.define_binary_operator("<=".into(), false, 1000).unwrap();

    define_binary_native_op_combinations!(ctx, 8, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 8, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 8, INT, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 8, FLOAT, INT, BOOL);

    ctx.define_binary_operator(">=".into(), false, 1050).unwrap();

    define_binary_native_op_combinations!(ctx, 9, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 9, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 9, INT, FLOAT, BOOL);
    define_binary_native_op_combinations_distinct!(ctx, 9, FLOAT, INT, BOOL);

    ctx.define_binary_operator("==".into(), false, 1100).unwrap();

    define_binary_native_op_combinations!(ctx, 10, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 10, FLOAT, BOOL);
    define_binary_native_op_combinations!(ctx, 10, STR, BOOL);
    define_binary_native_op_combinations!(ctx, 10, BOOL, BOOL);

    ctx.define_binary_operator("!=".into(), false, 1150).unwrap();

    define_binary_native_op_combinations!(ctx, 11, INT, BOOL);
    define_binary_native_op_combinations!(ctx, 11, FLOAT, BOOL);
    define_binary_native_op_combinations!(ctx, 11, STR, BOOL);
    define_binary_native_op_combinations!(ctx, 11, BOOL, BOOL);

    /*
        ╒════════════════════╕
        │ Logical operations │
        ╘════════════════════╛
    */

    ctx.define_binary_operator("||".into(), false, 1500).unwrap();

    define_binary_native_op_combinations!(ctx, 12, BOOL, BOOL);

    ctx.define_binary_operator("&&".into(), false, 1550).unwrap();

    define_binary_native_op_combinations!(ctx, 13, BOOL, BOOL);

    ctx.define_binary_operator(":=".into(), false, 100000).unwrap();

    ctx.define_native_binary_operation(
        14, 1, 
        T_0.to_mut(), T_0, Type::Empty, 
        |_, _, a, b, ctx| {
            a.assign(b, ctx)?;
            Ok(Object::empty())
        }
    ).unwrap();

    ctx.define_binary_operator(">>".into(), false, 350).unwrap();

    define_binary_native_op_combinations!(ctx, 15, INT, INT);

    ctx.define_binary_operator("<<".into(), false, 360).unwrap();

    define_binary_native_op_combinations!(ctx, 16, INT, INT);

    ctx.define_binary_operator("&".into(), false, 370).unwrap();

    define_binary_native_op_combinations!(ctx, 17, INT, INT);

    ctx.define_binary_operator("|".into(), false, 380).unwrap();

    define_binary_native_op_combinations!(ctx, 18, INT, INT);

    ctx.define_binary_operator("^".into(), false, 390).unwrap();

    define_binary_native_op_combinations!(ctx, 19, BOOL, BOOL);
    define_binary_native_op_combinations!(ctx, 19, INT, INT);
}

pub const CALL_OP: usize = 0;

pub fn standard_nary_operations(ctx: &mut NessaContext) {
    ctx.define_nary_operator("(".into(), ")".into(), 50).unwrap();

    for n in 0..30 {
        let args = (0..n).map(|i| Type::TemplateParam(i, vec!())).collect::<Vec<_>>();

        let f_type = Type::Function(
            Box::new(Type::And(args.clone())),
            Box::new(Type::TemplateParam(n, vec!()))
        );

        let res = ctx.define_native_nary_operation(
            0, n + 1, 
            Type::MutRef(Box::new(f_type.clone())), 
            args.as_slice(), 
            Type::TemplateParam(n, vec!()), 
            EMPTY_NARY_FUNC
        ).unwrap();

        ctx.cache.opcodes.nary.insert((0, res), (CompiledNessaExpr::LambdaCallRef, 0));

        let res = ctx.define_native_nary_operation(
            0, n + 1, 
            Type::Ref(Box::new(f_type.clone())), 
            args.as_slice(), 
            Type::TemplateParam(n, vec!()), 
            EMPTY_NARY_FUNC
        ).unwrap();

        ctx.cache.opcodes.nary.insert((0, res), (CompiledNessaExpr::LambdaCallRef, 0));

        let res = ctx.define_native_nary_operation(
            0, n + 1, 
            f_type, 
            args.as_slice(), 
            Type::TemplateParam(n, vec!()), 
            EMPTY_NARY_FUNC
        ).unwrap();

        ctx.cache.opcodes.nary.insert((0, res), (CompiledNessaExpr::LambdaCall, 0));
    }  

    ctx.define_nary_operator("[".into(), "]".into(), 75).unwrap();

    // Indexing operations on arrays
    let res = ctx.define_native_nary_operation(1, 1, ARR_OF!(T_0), &[INT], T_0, EMPTY_NARY_FUNC).unwrap();
    ctx.cache.opcodes.nary.insert((1, res), (CompiledNessaExpr::IdxMove, 0));
    
    let res = ctx.define_native_nary_operation(1, 1, ARR_OF!(T_0).to_mut(), &[INT], T_0.to_mut(), EMPTY_NARY_FUNC).unwrap();
    ctx.cache.opcodes.nary.insert((1, res), (CompiledNessaExpr::IdxMut, 0));
    
    let res = ctx.define_native_nary_operation(1, 1, ARR_OF!(T_0).to_ref(), &[INT], T_0.to_ref(), EMPTY_NARY_FUNC).unwrap();
    ctx.cache.opcodes.nary.insert((1, res), (CompiledNessaExpr::IdxRef, 0));
}