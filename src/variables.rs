use crate::object::Object;
use crate::types::Type;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Debug, Clone, PartialEq)]
pub struct Variable{
    pub id: usize,
    pub name: String,
    pub var_type: Type,
    pub value: Option<Object>
}