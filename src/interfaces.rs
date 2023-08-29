use colored::Colorize;

use crate::{types::Type, context::NessaContext};

pub struct Interface {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub fns: Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>
}
pub struct InterfaceImpl {
    pub interface_id: usize,
    pub args: Vec<Type>,
    pub interface_type: Type
}

#[derive(Clone, Hash, Debug, PartialEq)]
pub struct InterfaceConstraint {
    pub id: usize,
    pub args: Vec<Type>
}

impl InterfaceConstraint {
    pub fn new(id: usize, args: Vec<Type>) -> InterfaceConstraint {
        return InterfaceConstraint {
            id: id,
            args: args
        };
    }
}

impl InterfaceConstraint {
    pub fn get_name(&self, ctx: &NessaContext) -> String {
        if self.args.len() > 0 {
            return format!(
                "{}<{}>", 
                ctx.interfaces[self.id].name.green(),
                self.args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            );

        } else {
            return format!("{}", ctx.interfaces[self.id].name.green());
        }
    }
}

/*
                                                  ╒═══════════════════════╕
    ============================================= │  STANDARD INTERFACES  │ =============================================
                                                  ╘══════════════════════=╛
*/

// Constants for common interfaces
pub const ITERABLE_ID: usize = 0;
pub const PRINTABLE_ID: usize = 1;

// Standard context
pub fn standard_interfaces(ctx: &mut NessaContext) {
    ctx.define_interface("Iterable".into(), vec!("T".into()), vec!(/* TODO */)).unwrap();

    ctx.define_interface("Printable".into(), vec!(), vec!(
        ("Print".into(), None, vec!(("".into(), Type::SelfType)), Type::Empty)
    )).unwrap();
} 