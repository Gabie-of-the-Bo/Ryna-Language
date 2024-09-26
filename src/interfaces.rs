use colored::Colorize;
use serde::{Serialize, Deserialize};

use crate::{annotations::Annotation, context::RynaContext, html_ext::HTMLColorable, parser::Location, types::{Type, BOOL, FLOAT, INT, STR, T_0, T_1, T_2}, ARR_IT_OF, ARR_OF};

pub type InterfaceFunctionHeader = (Vec<Annotation>, String, Option<Vec<String>>, Vec<(String, Type)>, Type);
pub type InterfaceUnaryOpHeader = (Vec<Annotation>, usize, Vec<String>, String, Type, Type);
pub type InterfaceBinaryOpHeader = (Vec<Annotation>, usize, Vec<String>, (String, Type), (String, Type), Type);
pub type InterfaceNaryOpHeader = (Vec<Annotation>, usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type);

#[derive(Clone, Serialize, Deserialize)]
pub struct Interface {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub location: Location,
    pub annotations: Vec<Annotation>,
    pub fns: Vec<InterfaceFunctionHeader>,
    pub uns: Vec<InterfaceUnaryOpHeader>,
    pub bin: Vec<InterfaceBinaryOpHeader>,
    pub nary: Vec<InterfaceNaryOpHeader>
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
    pub fn get_name(&self, ctx: &RynaContext) -> String {
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

    pub fn get_name_html(&self, ctx: &RynaContext) -> String {
        if !self.args.is_empty() {
            format!(
                "{}&lt;{}&gt;", 
                ctx.interfaces[self.id].name.html_green(),
                self.args.iter().map(|i| i.get_name_html(ctx)).collect::<Vec<_>>().join(", ")
            )

        } else {
            format!("{}", ctx.interfaces[self.id].name.green())
        }
    }

    pub fn get_name_plain(&self, ctx: &RynaContext) -> String {
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
pub fn standard_interfaces(ctx: &mut RynaContext) {
    
    // Definitions
    ctx.define_interface(Location::none(), vec!(), "Iterable".into(), vec!("Iter".into(), "Elem".into()), vec!(
        (vec!(), "iterator".into(), None, vec!(("".into(), Type::SelfType)), T_0),
        (vec!(), "next".into(), None, vec!(("".into(), T_0.to_mut())), T_1),
        (vec!(), "is_consumed".into(), None, vec!(("".into(), T_0.to_mut())), BOOL)        
    ), vec!(), vec!(), vec!()).unwrap();

    ctx.define_interface(Location::none(), vec!(), "Printable".into(), vec!(), vec!(
        (vec!(), "print".into(), None, vec!(("".into(), Type::SelfType)), Type::Empty)
    ), vec!(), vec!(), vec!()).unwrap();

    ctx.define_interface(Location::none(), vec!(), "Destroyable".into(), vec!(), vec!(
        (vec!(), "destroy".into(), None, vec!(("".into(), Type::SelfType.to_ref())), Type::Empty)
    ), vec!(), vec!(), vec!()).unwrap();

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
    ctx.define_interface_impl("Printable".into(), vec!(), BOOL.to_ref().or(BOOL.to_mut()), vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), INT.to_ref().or(INT.to_mut()), vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), FLOAT.to_ref().or(FLOAT.to_mut()), vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), STR.to_ref().or(STR.to_mut()), vec!()).unwrap();
} 