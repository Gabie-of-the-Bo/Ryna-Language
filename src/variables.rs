use crate::object::Object;
use crate::types::Type;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone)]
pub struct Variable{
    pub id: usize,
    pub name: String,
    pub var_type: Type,
    pub value: Option<Object>
}