use std::collections::HashMap;
use std::collections::HashSet;

use colored::Colorize;
use serde::{Serialize, Deserialize};

use crate::context::NessaContext;
use crate::interfaces::InterfaceConstraint;
use crate::object::Object;
use crate::patterns::Pattern;
use crate::number::Integer;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type ParsingFunction = fn(&NessaContext, &TypeTemplate, &String) -> Result<Object, String>;

#[derive(Clone, Serialize, Deserialize)]
pub struct TypeTemplate {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,

    #[serde(skip)]
    pub attributes: Vec<(String, Type)>,

    pub alias: Option<Type>,
    
    #[serde(skip)]
    pub patterns: Vec<Pattern>,

    #[serde(skip)]
    pub parser: Option<ParsingFunction>
}

impl TypeTemplate {
    pub fn is_nominal(&self) -> bool {
        self.alias.is_none()
    }

    pub fn is_structural(&self) -> bool {
        self.alias.is_some()
    }
}

#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Clone, Hash, Debug, Serialize, Deserialize)]
pub enum Type {
    // Empty type (also called void)
    Empty,

    // Type to infer later
    InferenceMarker,

    // Type to substitute in interfaces
    SelfType,

    // Simple types
    Basic(usize),

    // References
    Ref(Box<Type>),
    MutRef(Box<Type>),

    // Algebraic types
    Or(Vec<Type>),
    And(Vec<Type>),

    // Parametric types
    Wildcard,
    TemplateParam(usize, Vec<InterfaceConstraint>),
    TemplateParamStr(String, Vec<InterfaceConstraint>),
    Template(usize, Vec<Type>),

    // Function type
    Function(Box<Type>, Box<Type>)
}

impl PartialEq for Type {
    fn eq(&self, b: &Self) -> bool {
        return match (self, b) {
            (Type::SelfType, Type::SelfType) |
            (Type::Empty, Type::Empty) |
            (Type::InferenceMarker, Type::InferenceMarker) |
            (Type::Wildcard, Type::Wildcard) => true,
            (Type::Basic(id_a), Type::Basic(id_b)) => id_a == id_b,
            (Type::Ref(ta), Type::Ref(tb)) => ta == tb,
            (Type::MutRef(ta), Type::MutRef(tb)) => ta == tb,
            (Type::Or(va), Type::Or(vb)) => va.iter().all(|i| vb.contains(i)) && vb.iter().all(|i| va.contains(i)),
            (Type::And(va), Type::And(vb)) => va == vb,
            (Type::And(va), b) => va.len() == 1 && va[0] == *b,
            (a, Type::And(vb)) => vb.len() == 1 && vb[0] == *a,
            (Type::TemplateParam(id_a, v_a), Type::TemplateParam(id_b, v_b)) => id_a == id_b && v_a == v_b,
            (Type::TemplateParamStr(n_a, v_a), Type::TemplateParamStr(n_b, v_b)) => n_a == n_b && v_a == v_b,
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va == vb,
            (Type::Function(fa, ta), Type::Function(fb, tb)) => fa == fb && ta == tb,
            
            _ => false
        }
    }
}

impl Eq for Type {}

impl Type {
    pub fn is_ref(&self) -> bool {
        matches!(
            self,
            Type::Ref(_) | Type::MutRef(_)
        )
    }

    pub fn to_ref(self) -> Type {
        Type::Ref(Box::new(self))
    }

    pub fn to_mut(self) -> Type {
        Type::MutRef(Box::new(self))
    }

    pub fn deref_type(&self) -> &Type {
        match self {
            Type::Ref(t) | Type::MutRef(t) => t,
            _ => self
        }
    }

    pub fn get_name(&self, ctx: &NessaContext) -> String {
        return match self {
            Type::Empty => "()".into(),
            Type::SelfType => format!("{}", "Self".green()),
            Type::InferenceMarker => "[Inferred]".into(),

            Type::Basic(id) => ctx.type_templates[*id].name.clone().cyan().to_string(),
            Type::Ref(t) => format!("{}{}", "&".magenta(), t.get_name(ctx)),
            Type::MutRef(t) => format!("{}{}", "&&".magenta(), t.get_name(ctx)),
            Type::Or(v) => v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(" | "),
            Type::And(v) => format!("({})", v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")),

            Type::Wildcard => "*".cyan().to_string(),

            Type::TemplateParam(id, v) => {
                if !v.is_empty() {
                    format!(
                        "{} [{}]", 
                        format!("'T_{}", id).green(), 
                        v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
                    )

                } else {
                    format!("'T_{}", id).green().to_string()
                }
            },
            Type::TemplateParamStr(name, v) => {
                if !v.is_empty() {
                    format!(
                        "{} [{}]", 
                        format!("'{}", name).green(), 
                        v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
                    )
                    
                } else {
                    format!("'{}", name).green().to_string()
                }   
            },
            Type::Template(id, v) => format!("{}<{}>", ctx.type_templates[*id].name.cyan().to_string().clone(), 
                                                       v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")),
            Type::Function(from, to) => format!("{} => {}", from.get_name(ctx), to.get_name(ctx))
        }
    }

    pub fn get_name_plain(&self, ctx: &NessaContext) -> String {
        return match self {
            Type::Empty => "()".into(),
            Type::SelfType => "Self".to_string(),
            Type::InferenceMarker => "[Inferred]".into(),

            Type::Basic(id) => ctx.type_templates[*id].name.clone().to_string(),
            Type::Ref(t) => format!("{}{}", "&", t.get_name_plain(ctx)),
            Type::MutRef(t) => format!("{}{}", "&&", t.get_name_plain(ctx)),
            Type::Or(v) => v.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(" | "),
            Type::And(v) => format!("({})", v.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(", ")),

            Type::Wildcard => "*".to_string(),

            Type::TemplateParam(id, v) => {
                if !v.is_empty() {
                    format!(
                        "T_{} [{}]", 
                        id, 
                        v.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(", ")
                    )

                } else {
                    format!("'T_{}", id).to_string()
                }
            },
            Type::TemplateParamStr(name, v) => {
                if !v.is_empty() {
                    format!(
                        "'{} [{}]", 
                        name, 
                        v.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(", ")
                    )
                    
                } else {
                    format!("'{}", name).to_string()
                }   
            },
            Type::Template(id, v) => format!("{}<{}>", ctx.type_templates[*id].name.to_string().clone(), 
                                                       v.iter().map(|i| i.get_name_plain(ctx)).collect::<Vec<_>>().join(", ")),
            Type::Function(from, to) => format!("{} => {}", from.get_name_plain(ctx), to.get_name_plain(ctx))
        }
    }

    pub fn has_templates(&self) -> bool {
        return match self {
            Type::Wildcard |
            Type::Empty |
            Type::Basic(_) => false,

            Type::Ref(a) |
            Type::MutRef(a) => a.has_templates(),

            Type::Template(_, a) |
            Type::Or(a) |
            Type::And(a) => a.iter().any(Type::has_templates),

            Type::TemplateParam(..) => true,
            
            Type::Function(a, b) => a.has_templates() || b.has_templates(),

            _ => unimplemented!()
        };
    }

    pub fn has_self(&self) -> bool {
        return match self {
            Type::SelfType => true,

            Type::Wildcard |
            Type::Empty |
            Type::Basic(_) => false,

            Type::Ref(a) |
            Type::MutRef(a) => a.has_self(),

            Type::Template(_, a) |
            Type::Or(a) |
            Type::And(a) => a.iter().any(Type::has_self),

            Type::TemplateParam(_, c) => c.iter().flat_map(|i| &i.args).any(Type::has_self),

            Type::Function(a, b) => a.has_self() || b.has_self(),

            _ => unimplemented!()
        };
    }

    pub fn template_dependencies(&self, templates: &mut HashSet<usize>) {
        return match self {
            Type::Wildcard |
            Type::Empty  |
            Type::InferenceMarker  |
            Type::SelfType  |
            Type::Basic(..)  |
            Type::TemplateParamStr(..) => {}

            Type::Ref(t) |
            Type::MutRef(t) => t.template_dependencies(templates),

            Type::Template(_, ts) |
            Type::Or(ts) |
            Type::And(ts) => ts.iter().for_each(|i| i.template_dependencies(templates)),

            Type::TemplateParam(id, cs) => {
                templates.insert(*id);

                for c in cs {
                    for t in &c.args {
                        t.template_dependencies(templates);
                    }
                }
            },

            Type::Function(a, b) => {
                a.template_dependencies(templates);
                b.template_dependencies(templates);
            },
        };
    }

    pub fn type_dependencies(&self) -> Vec<usize> {
        return match self {
            Type::Wildcard |
            Type::Empty  |
            Type::InferenceMarker  |
            Type::SelfType  |
            Type::TemplateParamStr(..) => vec!(),

            Type::Basic(id) => vec!(*id),

            Type::Ref(a) |
            Type::MutRef(a) => a.type_dependencies(),

            Type::Or(ts) |
            Type::And(ts) => ts.iter().flat_map(|t| t.type_dependencies()).collect(),

            Type::Function(a, b) => {
                let mut res = a.type_dependencies();
                res.append(&mut b.type_dependencies());

                res
            }

            Type::TemplateParam(_, v) => {
                v.iter().flat_map(|i| i.args.clone()).flat_map(|i| i.type_dependencies()).collect()
            }

            Type::Template(id, ts) => {
                let mut res = vec!(*id);
                res.append(&mut ts.iter().flat_map(|t| t.type_dependencies()).collect());

                res
            }
        };
    }

    pub fn interface_dependencies(&self) -> Vec<usize> {
        return match self {
            Type::Wildcard |
            Type::Empty  |
            Type::InferenceMarker  |
            Type::SelfType  |
            Type::Basic(..) |
            Type::TemplateParamStr(..) => vec!(),

            Type::Ref(a) |
            Type::MutRef(a) => a.interface_dependencies(),

            Type::Or(ts) |
            Type::And(ts) => ts.iter().flat_map(|t| t.interface_dependencies()).collect(),

            Type::Function(a, b) => {
                let mut res = a.interface_dependencies();
                res.append(&mut b.interface_dependencies());

                res
            }

            Type::TemplateParam(_, v) => {
                let mut res = v.iter().map(|i| i.id).collect::<Vec<_>>();

                for i in v {
                    res.extend(i.args.iter().flat_map(|i| i.interface_dependencies()));
                }

                res
            },

            Type::Template(_, ts) => {
                let mut res = vec!();
                res.append(&mut ts.iter().flat_map(|t| t.interface_dependencies()).collect());

                res
            }
        };
    }

    pub fn bindable_to(&self, other: &Type, ctx: &NessaContext) -> bool {
        self.template_bindable_to(other, &mut HashMap::new(), &mut HashMap::new(), ctx)
    }

    pub fn bindable_to_subtitutions(&self, other: &Type, ctx: &NessaContext) -> (bool, HashMap<usize, Type>) {
        let mut assignments = HashMap::new();
        let res = self.template_bindable_to(other, &mut assignments, &mut HashMap::new(), ctx);

        (res, assignments)
    }

    pub fn bindable_to_template(&self, other: &Type, templates: &[Type], ctx: &NessaContext) -> bool {
        return self.template_bindable_to(other, &mut templates.iter().cloned().enumerate().collect(), &mut HashMap::new(), ctx);
    }

    pub fn template_bindable_to(&self, other: &Type, t_assignments: &mut HashMap<usize, Type>, t_deps: &mut HashMap<usize, HashSet<usize>>, ctx: &NessaContext) -> bool {
        return match (self, other) {
            (_, Type::Wildcard) => true,

            (a, b) if a == b => true,

            (_, Type::Empty) => false,

            (Type::Ref(ta), Type::Ref(tb)) => ta.template_bindable_to(tb, t_assignments, t_deps, ctx),
            (Type::MutRef(ta), Type::MutRef(tb)) => ta.template_bindable_to(tb, t_assignments, t_deps, ctx),

            (Type::Basic(id), b) if ctx.type_templates[*id].is_structural() => {
                let alias = ctx.type_templates[*id].alias.as_ref().unwrap();
                return alias.template_bindable_to(b, t_assignments, t_deps, ctx);
            },

            (a, Type::Basic(id)) if ctx.type_templates[*id].is_structural() => {
                let alias = ctx.type_templates[*id].alias.as_ref().unwrap();
                return a.template_bindable_to(alias, t_assignments, t_deps, ctx);
            },

            (Type::Template(id, v), b) if ctx.type_templates[*id].is_structural() => {
                let alias = ctx.type_templates[*id].alias.as_ref().unwrap();
                let sub_alias = alias.sub_templates(&v.iter().cloned().enumerate().collect());

                return sub_alias.template_bindable_to(b, t_assignments, t_deps, ctx);
            },

            (a, Type::Template(id, v)) if ctx.type_templates[*id].is_structural() => {
                let alias = ctx.type_templates[*id].alias.as_ref().unwrap();
                let sub_alias = alias.sub_templates(&v.iter().cloned().enumerate().collect());   

                return a.template_bindable_to(&sub_alias, t_assignments, t_deps, ctx);
            },

            (Type::TemplateParam(id, cs), b) |
            (b, Type::TemplateParam(id, cs)) => {
                if let Some(t) = t_assignments.get(id) {
                    return b == t;
                
                } else {
                    for c in cs {
                        if !ctx.implements_interface(b, c, t_assignments, t_deps) {
                            return false;
                        }
                    }

                    t_assignments.insert(*id, b.clone());

                    return b.template_cyclic_reference_check(*id, t_deps);
                }
            },

            (Type::Or(v), b) => v.iter().all(|i| i.template_bindable_to(b, t_assignments, t_deps, ctx)),

            (a, Type::Or(v)) => {
                let mut t_assignments_cpy = t_assignments.clone();
                let mut t_deps_cpy = t_deps.clone();

                for i in v {
                    if a.template_bindable_to(i, &mut t_assignments_cpy, &mut t_deps_cpy, ctx) {
                        *t_assignments = t_assignments_cpy;
                        *t_deps = t_deps_cpy;
    
                        return true;
                    }
                }

                return false;
            },

            (Type::And(va), Type::And(vb)) => va.len() == vb.len() && va.iter().zip(vb).all(|(i, j)| i.template_bindable_to(j, t_assignments, t_deps, ctx)),
            (Type::And(va), b) => va.len() == 1 && va[0].template_bindable_to(b, t_assignments, t_deps, ctx),
            (a, Type::And(vb)) => vb.len() == 1 && a.template_bindable_to(&vb[0], t_assignments, t_deps, ctx),
                        
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va.len() == vb.len() && 
                                                                    va.iter().zip(vb).all(|(i, j)| i.template_bindable_to(j, t_assignments, t_deps, ctx)),

            (Type::Function(fa, ta), Type::Function(fb, tb)) => fa.template_bindable_to(fb, t_assignments, t_deps, ctx) && ta.template_bindable_to(tb, t_assignments, t_deps, ctx),

            _ => false
        }
    }

    fn template_cyclic_reference_check(&self, t_id: usize, t_deps: &mut HashMap<usize, HashSet<usize>>) -> bool {
        return match self {
            Type::Ref(t) => t.template_cyclic_reference_check(t_id, t_deps),
            Type::MutRef(t) => t.template_cyclic_reference_check(t_id, t_deps),

            Type::Or(v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),
            Type::And(v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),
            
            Type::TemplateParam(id, _) => {
                t_deps.entry(t_id).or_default().insert(*id);

                t_id != *id && !t_deps.entry(*id).or_default().contains(&t_id)
            },
            
            Type::Template(_, v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),

            Type::Function(f, t) => f.template_cyclic_reference_check(t_id, t_deps) && t.template_cyclic_reference_check(t_id, t_deps),

            _ => true
        }
    }

    pub fn compile_templates(&mut self, templates: &Vec<String>) {
        return match self {
            Type::Ref(t) => t.compile_templates(templates),
            Type::MutRef(t) => t.compile_templates(templates),

            Type::Or(v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),
            Type::And(v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),
            
            Type::TemplateParamStr(name, v) => {
                v.iter_mut().for_each(|i| {
                    i.args.iter_mut().for_each(|j| j.compile_templates(templates));
                });
                
                *self = Type::TemplateParam(templates.iter().position(|i| i == name).unwrap(), v.clone());
            },
            
            Type::Template(_, v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),

            Type::Function(f, t) => {
                f.compile_templates(templates); 
                t.compile_templates(templates)
            },

            _ => { }
        }
    }

    pub fn offset_templates(&mut self, offset: usize) {
        return match self {
            Type::Ref(t) => t.offset_templates(offset),
            Type::MutRef(t) => t.offset_templates(offset),

            Type::Or(v) => v.iter_mut().for_each(|i| i.offset_templates(offset)),
            Type::And(v) => v.iter_mut().for_each(|i| i.offset_templates(offset)),
            
            Type::TemplateParam(id, v) => {
                *id += offset;

                v.iter_mut().for_each(|i| {
                    i.args.iter_mut().for_each(|j| j.offset_templates(offset));
                });
            },
            
            Type::Template(_, v) => v.iter_mut().for_each(|i| i.offset_templates(offset)),

            Type::Function(f, t) => {
                f.offset_templates(offset); 
                t.offset_templates(offset)
            },

            _ => { }
        }
    }

    pub fn max_template(&self) -> i32 {
        return match self {
            Type::Ref(t) => t.max_template(),
            Type::MutRef(t) => t.max_template(),

            Type::Or(v) => v.iter().map(|i| i.max_template()).max().unwrap_or(-1),
            Type::And(v) => v.iter().map(|i| i.max_template()).max().unwrap_or(-1),
            
            Type::TemplateParam(id, v) => {
                v.iter().flat_map(|i| {
                    i.args.iter().map(|j| j.max_template())
                }).max().unwrap_or(-1).max(*id as i32)
            },
            
            Type::Template(_, v) => v.iter().map(|i| i.max_template()).max().unwrap_or(-1),

            Type::Function(f, t) => {
                f.max_template().max(t.max_template())
            },

            _ => -1
        }
    }

    pub fn sub_templates(&self, args: &HashMap<usize, Type>) -> Type {
        return match self {
            Type::Ref(t) => Type::Ref(Box::new(t.sub_templates(args))),
            Type::MutRef(t) => Type::MutRef(Box::new(t.sub_templates(args))),
            Type::Or(t) => Type::Or(t.iter().map(|i| i.sub_templates(args)).collect()),
            Type::And(t) => Type::And(t.iter().map(|i| i.sub_templates(args)).collect()),
            Type::Function(f, t) => Type::Function(Box::new(f.sub_templates(args)), Box::new(t.sub_templates(args))),
            Type::TemplateParam(id, v) => {
                args.get(id).cloned().unwrap_or_else(||
                    Type::TemplateParam(*id, v.iter().map(|i| {
                        let mapped_args = i.args.iter().map(|j| j.sub_templates(args)).collect();
                        InterfaceConstraint::new(i.id, mapped_args)
                    }).collect())
                )
            },
            Type::Template(id, t) => Type::Template(*id, t.iter().map(|i| i.sub_templates(args)).collect()),
            _ => self.clone()
        };
    }

    pub fn sub_self(&self, sub: &Type) -> Type {
        return match self {
            Type::SelfType => sub.clone(),
            Type::Ref(t) => Type::Ref(Box::new(t.sub_self(sub))),
            Type::MutRef(t) => Type::MutRef(Box::new(t.sub_self(sub))),
            Type::Or(t) => Type::Or(t.iter().map(|i| i.sub_self(sub)).collect()),
            Type::And(t) => Type::And(t.iter().map(|i| i.sub_self(sub)).collect()),
            Type::Function(f, t) => Type::Function(Box::new(f.sub_self(sub)), Box::new(t.sub_self(sub))),
            Type::TemplateParam(id, v) => {
                Type::TemplateParam(*id, v.iter().map(|i| {
                    let mapped_args = i.args.iter().map(|j| j.sub_self(sub)).collect();
                    InterfaceConstraint::new(i.id, mapped_args)
                }).collect())
            }
            Type::Template(id, t) => Type::Template(*id, t.iter().map(|i| i.sub_self(sub)).collect()),
            _ => self.clone()
        };
    }

    pub fn map_type(&self, ctx: &mut NessaContext, other_ctx: &NessaContext, classes: &mut HashMap<usize, usize>, interfaces: &mut HashMap<usize, usize>) -> Type {
        self.map_basic_types(&mut |id| ctx.map_nessa_class(other_ctx, id, classes, interfaces))
            .map_interfaces(&mut |id| ctx.map_nessa_interface(other_ctx, id, classes, interfaces))
    }

    pub fn map_basic_types(&self, mapping: &mut impl FnMut(usize) -> Result<usize, String>) -> Type {
        return match self {
            Type::Basic(id) => Type::Basic(mapping(*id).unwrap()),
            
            Type::Ref(t) => Type::Ref(Box::new(t.map_basic_types(mapping))),
            Type::MutRef(t) => Type::MutRef(Box::new(t.map_basic_types(mapping))),
            Type::Or(t) => Type::Or(t.iter().map(|i| i.map_basic_types(mapping)).collect()),
            Type::And(t) => Type::And(t.iter().map(|i| i.map_basic_types(mapping)).collect()),
            Type::Function(f, t) => Type::Function(Box::new(f.map_basic_types(mapping)), Box::new(t.map_basic_types(mapping))),
            Type::TemplateParam(id, v) => {
                Type::TemplateParam(*id, v.iter().map(|i| {
                    let mapped_args = i.args.iter().map(|j| j.map_basic_types(mapping)).collect();
                    InterfaceConstraint::new(i.id, mapped_args)
                }).collect())
            }
            Type::Template(id, t) => Type::Template(mapping(*id).unwrap(), t.iter().map(|i| i.map_basic_types(mapping)).collect()),

            _ => self.clone()
        };
    }

    pub fn map_interfaces(&self, mapping: &mut impl FnMut(usize) -> Result<usize, String>) -> Type {
        return match self {            
            Type::Ref(t) => Type::Ref(Box::new(t.map_interfaces(mapping))),
            Type::MutRef(t) => Type::MutRef(Box::new(t.map_interfaces(mapping))),
            Type::Or(t) => Type::Or(t.iter().map(|i| i.map_interfaces(mapping)).collect()),
            Type::And(t) => Type::And(t.iter().map(|i| i.map_interfaces(mapping)).collect()),
            Type::Function(f, t) => Type::Function(Box::new(f.map_interfaces(mapping)), Box::new(t.map_interfaces(mapping))),
            Type::TemplateParam(id, v) => {
                Type::TemplateParam(*id, v.iter().map(|i| {
                    let mapped_args = i.args.iter().map(|j| j.map_interfaces(mapping)).collect();
                    InterfaceConstraint::new(mapping(i.id).unwrap(), mapped_args)
                }).collect())
            }
            Type::Template(id, t) => Type::Template(*id, t.iter().map(|i| i.map_interfaces(mapping)).collect()),

            _ => self.clone()
        };
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::{types::*, context::standard_ctx};

    #[test]
    fn basic_type_binding() {
        let ctx = standard_ctx();

        let number_t = TypeTemplate {
            id: 0,
            name: "Int".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let string_t = TypeTemplate {
            id: 1,
            name: "String".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let bool_t = TypeTemplate {
            id: 2,
            name: "Bool".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let vector_t = TypeTemplate {
            id: 3,
            name: "Vector".into(),
            params: vec!("T".into()),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let number = Type::Basic(number_t.id);
        let string = Type::Basic(string_t.id);
        let boolean = Type::Basic(bool_t.id);

        assert!(number.bindable_to(&number, &ctx));
        assert!(string.bindable_to(&string, &ctx));
        assert!(!number.bindable_to(&string, &ctx));

        let number_ref = Type::Ref(Box::new(number.clone()));
        let number_mut = Type::MutRef(Box::new(number.clone()));

        assert!(number_ref.bindable_to(&number_ref, &ctx));
        assert!(number_mut.bindable_to(&number_mut, &ctx));
        assert!(!number_mut.bindable_to(&number_ref, &ctx));
        assert!(!number_ref.bindable_to(&number_mut, &ctx));

        let string_or_number = Type::Or(vec!(string.clone(), number.clone()));
        let number_or_string = Type::Or(vec!(number.clone(), string.clone()));

        assert!(string_or_number.bindable_to(&string_or_number, &ctx));
        assert!(number_or_string.bindable_to(&number_or_string, &ctx));
        assert!(string_or_number.bindable_to(&number_or_string, &ctx));
        assert!(number_or_string.bindable_to(&string_or_number, &ctx));

        assert!(number.bindable_to(&string_or_number, &ctx));
        assert!(string.bindable_to(&string_or_number, &ctx));
        assert!(!boolean.bindable_to(&string_or_number, &ctx));

        assert!(!string_or_number.bindable_to(&string, &ctx));

        let string_and_number = Type::And(vec!(string.clone(), number.clone()));

        assert!(string_and_number.bindable_to(&string_and_number, &ctx));
        assert!(!string_or_number.bindable_to(&string_and_number, &ctx));
        assert!(!string.bindable_to(&string_and_number, &ctx));
        assert!(!number.bindable_to(&string_and_number, &ctx));

        let wildcard = Type::Wildcard;

        assert!(number.bindable_to(&wildcard, &ctx));
        assert!(string.bindable_to(&wildcard, &ctx));
        assert!(boolean.bindable_to(&wildcard, &ctx));
        assert!(number_or_string.bindable_to(&wildcard, &ctx));
        assert!(string_and_number.bindable_to(&wildcard, &ctx));
        assert!(wildcard.bindable_to(&wildcard, &ctx));

        let empty = Type::Empty;

        assert!(!number.bindable_to(&empty, &ctx));
        assert!(!string.bindable_to(&empty, &ctx));
        assert!(!boolean.bindable_to(&empty, &ctx));
        assert!(!number_or_string.bindable_to(&empty, &ctx));
        assert!(!string_and_number.bindable_to(&empty, &ctx));
        assert!(empty.bindable_to(&empty, &ctx));

        let vector_number = Type::Template(vector_t.id, vec!(number.clone()));
        let vector_string = Type::Template(vector_t.id, vec!(string));
        let vector_number_or_string = Type::Template(vector_t.id, vec!(number_or_string.clone()));

        assert!(vector_number.bindable_to(&vector_number, &ctx));
        assert!(vector_number.bindable_to(&vector_number_or_string, &ctx));
        assert!(!vector_number.bindable_to(&vector_string, &ctx));

        let f_number_number = Type::Function(Box::new(number.clone()), Box::new(number.clone()));
        let f_number_or_string_number = Type::Function(Box::new(number_or_string.clone()), Box::new(number.clone()));

        assert!(f_number_number.bindable_to(&f_number_number, &ctx));
        assert!(f_number_or_string_number.bindable_to(&f_number_or_string_number, &ctx));
        assert!(f_number_number.bindable_to(&f_number_or_string_number, &ctx));
        assert!(!f_number_or_string_number.bindable_to(&f_number_number, &ctx));
    }

    #[test]
    fn template_binding() {
        let ctx = standard_ctx();

        let number_t = TypeTemplate {
            id: 0,
            name: "Int".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let string_t = TypeTemplate {
            id: 1,
            name: "String".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let bool_t = TypeTemplate {
            id: 2,
            name: "Bool".into(),
            params: vec!(),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let vector_t = TypeTemplate {
            id: 3,
            name: "Vector".into(),
            params: vec!("T".into()),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let map_t = TypeTemplate {
            id: 3,
            name: "Map".into(),
            params: vec!("T".into(), "G".into()),
            attributes: vec!(),
            alias: None,
            patterns: vec!(),
            parser: None
        };

        let number = Type::Basic(number_t.id);
        let string = Type::Basic(string_t.id);
        let boolean = Type::Basic(bool_t.id);

        let template_1 = Type::TemplateParam(0, vec!());
        let template_2 = Type::Ref(Box::new(Type::TemplateParam(0, vec!())));

        assert!(number.bindable_to(&template_1, &ctx));
        assert!(string.bindable_to(&template_1, &ctx));
        assert!(boolean.bindable_to(&template_1, &ctx));
        assert!(!number.bindable_to(&template_2, &ctx));
        assert!(!string.bindable_to(&template_2, &ctx));
        assert!(!boolean.bindable_to(&template_2, &ctx));
        assert!(!template_1.bindable_to(&template_2, &ctx));
        assert!(!template_2.bindable_to(&template_1, &ctx));

        let template_1 = Type::Template(vector_t.id, vec!(Type::TemplateParam(0, vec!()))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::TemplateParam(1, vec!()))); 

        let binding_1 = Type::Template(vector_t.id, vec!(number.clone()));
        let binding_2 = Type::Template(map_t.id, vec!(number.clone(), string.clone()));

        assert!(binding_1.bindable_to(&template_1, &ctx));
        assert!(binding_2.bindable_to(&template_2, &ctx));
        assert!(!binding_2.bindable_to(&template_1, &ctx));
        assert!(!binding_1.bindable_to(&template_2, &ctx));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::TemplateParam(0, vec!()))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), number.clone()));
        let binding_2 = Type::Template(map_t.id, vec!(boolean.clone(), boolean.clone()));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), string.clone()));
        
        assert!(binding_1.bindable_to(&template_1, &ctx));
        assert!(binding_2.bindable_to(&template_1, &ctx));
        assert!(!binding_3.bindable_to(&template_1, &ctx));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::Template(map_t.id, vec!(Type::TemplateParam(1, vec!()), Type::TemplateParam(0, vec!()))))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(number.clone(), number.clone()))));
        let binding_2 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_4 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_5 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        
        assert!(binding_1.bindable_to(&template_1, &ctx));
        assert!(binding_2.bindable_to(&template_1, &ctx));
        assert!(binding_3.bindable_to(&template_1, &ctx));
        assert!(!binding_4.bindable_to(&template_1, &ctx));
        assert!(!binding_5.bindable_to(&template_1, &ctx));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::Wildcard)))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(number.clone(), number.clone()))));
        let binding_2 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_4 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_5 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_6 = Type::Template(map_t.id, vec!(boolean.clone(), Type::Template(map_t.id, vec!(boolean.clone(), number.clone()))));
        
        assert!(binding_1.bindable_to(&template_1, &ctx));
        assert!(binding_2.bindable_to(&template_1, &ctx));
        assert!(!binding_3.bindable_to(&template_1, &ctx));
        assert!(!binding_4.bindable_to(&template_1, &ctx));
        assert!(binding_5.bindable_to(&template_1, &ctx));
        assert!(binding_6.bindable_to(&template_1, &ctx));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::TemplateParam(1, vec!()))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(1, vec!()), Type::TemplateParam(0, vec!()))); 

        assert!(template_1.bindable_to(&template_1, &ctx));
        assert!(template_2.bindable_to(&template_2, &ctx));
        assert!(!template_1.bindable_to(&template_2, &ctx));
        assert!(!template_2.bindable_to(&template_1, &ctx));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0, vec!()), Type::Template(map_t.id, vec!(Type::TemplateParam(1, vec!()), Type::TemplateParam(0, vec!()))))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(1, vec!()), Type::Template(map_t.id, vec!(Type::TemplateParam(1, vec!()), Type::TemplateParam(1, vec!()))))); 

        assert!(template_1.bindable_to(&template_2, &ctx));
        assert!(template_2.bindable_to(&template_1, &ctx));
    }

    #[test]
    fn alias_type_binding() {
        let mut ctx = standard_ctx();
        let number_id = ctx.type_templates.len();
        let number = Type::Basic(number_id);

        ctx.define_type("Number".into(), vec!(), vec!(), Some(Type::Or(vec!(INT, FLOAT))), vec!(), None).unwrap();

        assert!(INT.bindable_to(&number, &ctx));
        assert!(FLOAT.bindable_to(&number, &ctx));
        assert!(!STR.bindable_to(&number, &ctx));
    }

    #[test]
    fn recursive_type_binding() {
        let mut ctx = standard_ctx();
        let list_id = ctx.type_templates.len();
        let list = Type::Basic(list_id);

        ctx.define_type("List".into(), vec!(), vec!(), Some(Type::Or(vec!(
            INT,
            Type::And(vec!(INT, list.clone()))
        ))), vec!(), None).unwrap();

        let tuple_1 = Type::And(vec!(INT, INT));
        let tuple_2 = Type::And(vec!(INT, tuple_1.clone()));
        let tuple_3 = Type::And(vec!(INT, tuple_2.clone()));
        let tuple_4 = Type::And(vec!(INT, FLOAT));
        let tuple_5 = Type::And(vec!(INT, tuple_4.clone()));

        assert!(INT.bindable_to(&list, &ctx));
        assert!(tuple_1.bindable_to(&list, &ctx));
        assert!(tuple_2.bindable_to(&list, &ctx));
        assert!(tuple_3.bindable_to(&list, &ctx));

        assert!(!FLOAT.bindable_to(&list, &ctx));
        assert!(!tuple_4.bindable_to(&list, &ctx));
        assert!(!tuple_5.bindable_to(&list, &ctx));
    }

    #[test]
    fn parametric_recursive_type_binding() {
        let mut ctx = standard_ctx();
        let nil_id = ctx.type_templates.len();
        let list_id = nil_id + 1;
        let nil = Type::Basic(nil_id);
        let list = Type::Template(list_id, vec!(T_0));

        ctx.define_type("Nil".into(), vec!(), vec!(), None, vec!(), None).unwrap();
        ctx.define_type("List".into(), vec!(), vec!(), Some(Type::Or(vec!(
            nil.clone(),
            Type::And(vec!(T_0, list.clone()))
        ))), vec!(), None).unwrap();

        let tuple_1 = Type::And(vec!(INT, nil.clone()));
        let tuple_2 = Type::And(vec!(INT, tuple_1.clone()));
        let tuple_3 = Type::And(vec!(INT, tuple_2.clone()));
        let tuple_4 = Type::And(vec!(FLOAT, nil.clone()));
        let tuple_5 = Type::And(vec!(FLOAT, tuple_4.clone()));
        let tuple_6 = Type::And(vec!(INT, tuple_4.clone()));
        let tuple_7 = Type::And(vec!(INT, tuple_6.clone()));

        assert!(nil.bindable_to(&list, &ctx));
        assert!(tuple_1.bindable_to(&list, &ctx));
        assert!(tuple_2.bindable_to(&list, &ctx));
        assert!(tuple_3.bindable_to(&list, &ctx));
        assert!(tuple_4.bindable_to(&list, &ctx));
        assert!(tuple_5.bindable_to(&list, &ctx));
        assert!(!tuple_6.bindable_to(&list, &ctx));
        assert!(!tuple_7.bindable_to(&list, &ctx));
    }
}

/*
                                                  ╒══════════════════╕
    ============================================= │  STANDARD TYPES  │ =============================================
                                                  ╘══════════════════╛
*/

// Constants for common types
pub const INT_ID: usize = 0;
pub const FLOAT_ID: usize = 1;
pub const STR_ID: usize = 2;
pub const BOOL_ID: usize = 3;
pub const ARR_ID: usize = 4;
pub const ARR_IT_ID: usize = 5;
pub const FILE_ID: usize = 6;

pub const INT: Type = Type::Basic(INT_ID);
pub const FLOAT: Type = Type::Basic(FLOAT_ID);
pub const STR: Type = Type::Basic(STR_ID);
pub const BOOL: Type = Type::Basic(BOOL_ID);
pub const FILE: Type = Type::Basic(FILE_ID);

#[macro_export]
macro_rules! ARR_OF { ($t: expr) => { Type::Template($crate::types::ARR_ID, vec!($t)) }; }

#[macro_export]
macro_rules! ARR_IT_OF { ($t: expr) => { Type::Template($crate::types::ARR_IT_ID, vec!($t)) }; }

pub const T_0: Type = Type::TemplateParam(0, vec!());
pub const T_1: Type = Type::TemplateParam(1, vec!());
pub const T_2: Type = Type::TemplateParam(2, vec!());

// Standard context
pub fn standard_types(ctx: &mut NessaContext) {
    ctx.define_type("Int".into(), vec!(), vec!(), None, vec!(), Some(|_, _, s| s.parse::<Integer>().map(Object::new))).unwrap();
    ctx.define_type("Float".into(), vec!(), vec!(), None, vec!(), Some(|_, _, s| s.parse::<f64>().map(Object::new).map_err(|_| "Invalid float format".to_string()))).unwrap();
    ctx.define_type("String".into(), vec!(), vec!(), None, vec!(), None).unwrap();

    ctx.define_type("Bool".into(), vec!(), vec!(), None, vec!(), Some(|_, _, s| 
        if s == "true" || s == "false" {
            Ok(Object::new(s.starts_with('t')))

        } else {
            Err(format!("Unable to parse bool from {}", s))
        }
    )).unwrap();

    ctx.define_type("Array".into(), vec!("Inner".into()), vec!(), None, vec!(), None).unwrap();
    ctx.define_type("ArrayIterator".into(), vec!("Inner".into()), vec!(), None, vec!(), None).unwrap();

    ctx.define_type("File".into(), vec!(), vec!(), None, vec!(), None).unwrap();
} 