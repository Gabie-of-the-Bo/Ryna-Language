use crate::types::*;
use crate::operations::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone)]
pub struct NessaContext {
    pub type_templates: Vec<TypeTemplate>, 
    pub unary_ops: Vec<UnaryOperator>,
    pub binary_ops: Vec<BinaryOperator>,
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD CTX  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_ctx() -> NessaContext {
    return NessaContext {
        type_templates: vec!(),
        unary_ops: standard_unary_operations(),
        binary_ops: standard_binary_operations()
    };
}