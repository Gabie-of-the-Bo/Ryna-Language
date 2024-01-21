use colored::Colorize;
use serde::{Serialize, Deserialize};

use crate::{types::{Type, INT, FLOAT, STR, BOOL, T_1, T_0, T_2}, context::NessaContext, ARR_OF, ARR_IT_OF};

pub type InterfaceFunctionHeader = (String, Option<Vec<String>>, Vec<(String, Type)>, Type);
pub type InterfaceUnaryOpHeader = (usize, Vec<String>, String, Type, Type);
pub type InterfaceBinaryOpHeader = (usize, Vec<String>, (String, Type), (String, Type), Type);

#[derive(Clone)]
pub struct Interface {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub fns: Vec<InterfaceFunctionHeader>,
    pub uns: Vec<InterfaceUnaryOpHeader>,
    pub bin: Vec<InterfaceBinaryOpHeader>
}

#[derive(Clone, Serialize, Deserialize)]
pub struct InterfaceImpl {
    pub interface_id: usize,
    pub args: Vec<Type>,
    pub interface_type: Type
}

#[derive(Clone, Hash, Debug, PartialEq, Serialize, Deserialize)]
pub struct InterfaceConstraint {
    pub id: usize,
    pub args: Vec<Type>
}

impl InterfaceConstraint {
    pub fn new(id: usize, args: Vec<Type>) -> InterfaceConstraint {
        InterfaceConstraint {
            id,
            args
        }
    }
}

impl InterfaceConstraint {
    pub fn get_name(&self, ctx: &NessaContext) -> String {
        if !self.args.is_empty() {
            format!(
                "{}<{}>", 
                ctx.interfaces[self.id].name.green(),
                self.args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            )

        } else {
            format!("{}", ctx.interfaces[self.id].name.green())
        }
    }

    pub fn get_name_plain(&self, ctx: &NessaContext) -> String {
        if !self.args.is_empty() {
            format!(
                "{}<{}>", 
                ctx.interfaces[self.id].name,
                self.args.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(", ")
            )

        } else {
            ctx.interfaces[self.id].name.to_string()
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

pub const PRINTABLE: InterfaceConstraint = InterfaceConstraint { id: PRINTABLE_ID, args: vec!() };

#[macro_export]
macro_rules! ITERABLE_OF { ($t: expr) => { InterfaceConstraint::new($t) }; }

// Standard context
pub fn standard_interfaces(ctx: &mut NessaContext) {
    
    // Definitions
    ctx.define_interface("Iterable".into(), vec!("Iter".into(), "Elem".into()), vec!(
        ("iterator".into(), None, vec!(("".into(), Type::SelfType)), T_0),
        ("next".into(), None, vec!(("".into(), T_0.to_mut())), T_1),
        ("is_consumed".into(), None, vec!(("".into(), T_0.to_mut())), BOOL)        
    ), vec!(), vec!()).unwrap();

    ctx.define_interface("Printable".into(), vec!(), vec!(
        ("print".into(), None, vec!(("".into(), Type::SelfType)), Type::Empty)
    ), vec!(), vec!()).unwrap();

    // Implementations
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_OF!(T_2), vec!(ARR_IT_OF!(T_2.to_mut()), T_2.to_mut())).unwrap();
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_OF!(T_2).to_ref(), vec!(ARR_IT_OF!(T_2.to_ref()), T_2.to_ref())).unwrap();
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_OF!(T_2).to_mut(), vec!(ARR_IT_OF!(T_2.to_mut()), T_2.to_mut())).unwrap();

    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_IT_OF!(T_2.to_mut()), vec!(ARR_IT_OF!(T_2.to_mut()), T_2.to_mut())).unwrap();
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_IT_OF!(T_2.to_ref()), vec!(ARR_IT_OF!(T_2.to_ref()), T_2.to_ref())).unwrap();

    ctx.define_interface_impl("Printable".into(), vec!(), BOOL, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), INT, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), FLOAT, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), STR, vec!()).unwrap();
} 