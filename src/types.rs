use std::collections::HashMap;
use std::collections::HashSet;

use crate::context::NessaContext;
use crate::object::Object;
use crate::patterns::Pattern;
use crate::number::Number;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone, PartialEq)]
pub struct Tuple {
    pub types: Vec<Type>,
    pub exprs: Vec<Object>
}

pub type ParsingFunction = fn(&NessaContext, &TypeTemplate, &String) -> Result<Object, String>;

#[derive(Clone)]
pub struct TypeTemplate {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub attributes: Vec<(String, Type)>,
    pub patterns: Vec<Pattern>,
    pub parser: Option<ParsingFunction>
}

#[derive(Clone, PartialEq)]
pub struct TypeInstance {
    pub id: usize,
    pub params: Vec<Type>,
    pub attributes: Vec<Object>
}

#[derive(Clone, Hash, Debug)]
pub enum Type {
    // Empty type (also called void)
    Empty,

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
    TemplateParam(usize),
    TemplateParamStr(String),
    Template(usize, Vec<Type>),

    // Function type
    Function(Option<usize>, Box<Type>, Box<Type>)
}

impl PartialEq for Type {
    fn eq(&self, b: &Self) -> bool {
        return match (self, b) {
            (Type::Empty, Type::Empty) => true,
            (Type::Basic(id_a), Type::Basic(id_b)) => id_a == id_b,
            (Type::Ref(ta), Type::Ref(tb)) => ta == tb,
            (Type::MutRef(ta), Type::MutRef(tb)) => ta == tb,
            (Type::Or(va), Type::Or(vb)) => va.iter().all(|i| vb.contains(i)) && vb.iter().all(|i| va.contains(i)),
            (Type::And(va), Type::And(vb)) => va == vb,
            (Type::And(va), b) => va.len() == 1 && va[0] == *b,
            (a, Type::And(vb)) => vb.len() == 1 && vb[0] == *a,
            (Type::Wildcard, Type::Wildcard) => true,
            (Type::TemplateParam(id_a), Type::TemplateParam(id_b)) => id_a == id_b,
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va == vb,
            (Type::Function(ia, fa, ta), Type::Function(ib, fb, tb)) => ia == ib && fa == fb && ta == tb,
            
            _ => false
        }
    }
}

impl Eq for Type {}

impl Type {
    pub fn get_name(&self, ctx: &NessaContext) -> String {
        return match self {
            Type::Empty => "()".into(),

            Type::Basic(id) => ctx.type_templates[*id].name.clone(),
            Type::Ref(t) => format!("&{}", t.get_name(ctx)),
            Type::MutRef(t) => format!("&&{}", t.get_name(ctx)),
            Type::Or(v) => v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(" | "),
            Type::And(v) => format!("({})", v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")),

            Type::Wildcard => "*".into(),

            Type::TemplateParam(id) => format!("'T_{}", id),
            Type::TemplateParamStr(name) => format!("'{}", name),
            Type::Template(id, v) => format!("{}<{}>", ctx.type_templates[*id].name.clone(), 
                                                       v.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")),
            Type::Function(_, from, to) => format!("{} => {}", from.get_name(ctx), to.get_name(ctx))
        }
    }

    pub fn bindable_to(&self, other: &Type) -> bool {
        return self.template_bindable_to(other, &mut HashMap::new(), &mut HashMap::new());
    }

    pub fn bindable_to_subtitutions(&self, other: &Type) -> (bool, HashMap<usize, Type>) {
        let mut assignments = HashMap::new();
        let res = self.template_bindable_to(other, &mut assignments, &mut HashMap::new());

        return (res, assignments);
    }

    pub fn bindable_to_template(&self, other: &Type, templates: &[Type]) -> bool {
        return self.template_bindable_to(other, &mut templates.iter().cloned().enumerate().collect(), &mut HashMap::new());
    }

    pub fn template_bindable_to(&self, other: &Type, t_assignments: &mut HashMap<usize, Type>, t_deps: &mut HashMap<usize, HashSet<usize>>) -> bool {
        return match (self, other) {
            (_, Type::Wildcard) => true,

            (a, b) if a == b => true,

            (_, Type::Empty) => false,

            (Type::Ref(ta), Type::Ref(tb)) => ta.template_bindable_to(tb, t_assignments, t_deps),
            (Type::MutRef(ta), Type::MutRef(tb)) => ta.template_bindable_to(tb, t_assignments, t_deps),

            (Type::Or(v), b) => v.iter().all(|i| i.template_bindable_to(b, t_assignments, t_deps)),
            (a, Type::Or(v)) => v.iter().any(|i| a.template_bindable_to(i, t_assignments, t_deps)),

            (Type::And(va), Type::And(vb)) => va.len() == vb.len() && va.iter().zip(vb).all(|(i, j)| i.template_bindable_to(j, t_assignments, t_deps)),
            (Type::And(va), b) => va.len() == 1 && va[0].template_bindable_to(b, t_assignments, t_deps),
            (a, Type::And(vb)) => vb.len() == 1 && a.template_bindable_to(&vb[0], t_assignments, t_deps),
            
            (Type::TemplateParam(id), b) |
            (b, Type::TemplateParam(id)) => {
                if let Some(t) = t_assignments.get(id) {
                    return b == t;
                
                } else {
                    t_assignments.insert(*id, b.clone());

                    return b.template_cyclic_reference_check(*id, t_deps);
                }
            },
            
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va.len() == vb.len() && 
                                                                    va.iter().zip(vb).all(|(i, j)| i.template_bindable_to(j, t_assignments, t_deps)),

            (Type::Function(_, fa, ta), Type::Function(_, fb, tb)) => fa.template_bindable_to(fb, t_assignments, t_deps) && ta.template_bindable_to(tb, t_assignments, t_deps),

            _ => false
        }
    }

    fn template_cyclic_reference_check(&self, t_id: usize, t_deps: &mut HashMap<usize, HashSet<usize>>) -> bool {
        return match self {
            Type::Ref(t) => t.template_cyclic_reference_check(t_id, t_deps),
            Type::MutRef(t) => t.template_cyclic_reference_check(t_id, t_deps),

            Type::Or(v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),
            Type::And(v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),
            
            Type::TemplateParam(id) => {
                t_deps.entry(t_id).or_default().insert(*id);

                t_id != *id && !t_deps.entry(*id).or_default().contains(&t_id)
            },
            
            Type::Template(_, v) => v.iter().all(|i| i.template_cyclic_reference_check(t_id, t_deps)),

            Type::Function(_, f, t) => f.template_cyclic_reference_check(t_id, t_deps) && t.template_cyclic_reference_check(t_id, t_deps),

            _ => true
        }
    }

    pub fn compile_templates(&mut self, templates: &Vec<String>) {
        return match self {
            Type::Ref(t) => t.compile_templates(templates),
            Type::MutRef(t) => t.compile_templates(templates),

            Type::Or(v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),
            Type::And(v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),
            
            Type::TemplateParamStr(name) => *self = Type::TemplateParam(templates.iter().position(|i| i == name).unwrap()),
            
            Type::Template(_, v) => v.iter_mut().for_each(|i| i.compile_templates(templates)),

            Type::Function(_, f, t) => {
                f.compile_templates(templates); 
                t.compile_templates(templates)
            },

            _ => { }
        }
    }

    pub fn sub_templates(&self, args: &HashMap<usize, Type>) -> Type {
        return match self {
            Type::Ref(t) => Type::Ref(Box::new(t.sub_templates(args))),
            Type::MutRef(t) => Type::MutRef(Box::new(t.sub_templates(args))),
            Type::Or(t) => Type::Or(t.iter().map(|i| i.sub_templates(args)).collect()),
            Type::And(t) => Type::And(t.iter().map(|i| i.sub_templates(args)).collect()),
            Type::Function(i, f, t) => Type::Function(i.clone(), Box::new(f.sub_templates(args)), Box::new(t.sub_templates(args))),
            Type::TemplateParam(id) => args.get(id).unwrap_or(self).clone(),
            Type::Template(id, t) => Type::Template(*id, t.iter().map(|i| i.sub_templates(args)).collect()),
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
    use crate::types::*;

    #[test]
    fn basic_type_binding() {
        let number_t = TypeTemplate {
            id: 0,
            name: "Number".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let string_t = TypeTemplate {
            id: 1,
            name: "String".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let bool_t = TypeTemplate {
            id: 2,
            name: "Bool".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let vector_t = TypeTemplate {
            id: 3,
            name: "Vector".into(),
            params: vec!("T".into()),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let number = Type::Basic(number_t.id);
        let string = Type::Basic(string_t.id);
        let boolean = Type::Basic(bool_t.id);

        assert!(number.bindable_to(&number));
        assert!(string.bindable_to(&string));
        assert!(!number.bindable_to(&string));

        let number_ref = Type::Ref(Box::new(number.clone()));
        let number_mut = Type::MutRef(Box::new(number.clone()));

        assert!(number_ref.bindable_to(&number_ref));
        assert!(number_mut.bindable_to(&number_mut));
        assert!(!number_mut.bindable_to(&number_ref));
        assert!(!number_ref.bindable_to(&number_mut));

        let string_or_number = Type::Or(vec!(string.clone(), number.clone()));
        let number_or_string = Type::Or(vec!(number.clone(), string.clone()));

        assert!(string_or_number.bindable_to(&string_or_number));
        assert!(number_or_string.bindable_to(&number_or_string));
        assert!(string_or_number.bindable_to(&number_or_string));
        assert!(number_or_string.bindable_to(&string_or_number));

        assert!(number.bindable_to(&string_or_number));
        assert!(string.bindable_to(&string_or_number));
        assert!(!boolean.bindable_to(&string_or_number));

        assert!(!string_or_number.bindable_to(&string));

        let string_and_number = Type::And(vec!(string.clone(), number.clone()));

        assert!(string_and_number.bindable_to(&string_and_number));
        assert!(!string_or_number.bindable_to(&string_and_number));
        assert!(!string.bindable_to(&string_and_number));
        assert!(!number.bindable_to(&string_and_number));

        let wildcard = Type::Wildcard;

        assert!(number.bindable_to(&wildcard));
        assert!(string.bindable_to(&wildcard));
        assert!(boolean.bindable_to(&wildcard));
        assert!(number_or_string.bindable_to(&wildcard));
        assert!(string_and_number.bindable_to(&wildcard));
        assert!(wildcard.bindable_to(&wildcard));

        let empty = Type::Empty;

        assert!(!number.bindable_to(&empty));
        assert!(!string.bindable_to(&empty));
        assert!(!boolean.bindable_to(&empty));
        assert!(!number_or_string.bindable_to(&empty));
        assert!(!string_and_number.bindable_to(&empty));
        assert!(empty.bindable_to(&empty));

        let vector_number = Type::Template(vector_t.id, vec!(number.clone()));
        let vector_string = Type::Template(vector_t.id, vec!(string));
        let vector_number_or_string = Type::Template(vector_t.id, vec!(number_or_string.clone()));

        assert!(vector_number.bindable_to(&vector_number));
        assert!(vector_number.bindable_to(&vector_number_or_string));
        assert!(!vector_number.bindable_to(&vector_string));

        let f_number_number = Type::Function(None, Box::new(number.clone()), Box::new(number.clone()));
        let f_number_or_string_number = Type::Function(None, Box::new(number_or_string.clone()), Box::new(number.clone()));

        assert!(f_number_number.bindable_to(&f_number_number));
        assert!(f_number_or_string_number.bindable_to(&f_number_or_string_number));
        assert!(f_number_number.bindable_to(&f_number_or_string_number));
        assert!(!f_number_or_string_number.bindable_to(&f_number_number));
    }

    #[test]
    fn template_binding() {
        let number_t = TypeTemplate {
            id: 0,
            name: "Number".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let string_t = TypeTemplate {
            id: 1,
            name: "String".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let bool_t = TypeTemplate {
            id: 2,
            name: "Bool".into(),
            params: vec!(),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let vector_t = TypeTemplate {
            id: 3,
            name: "Vector".into(),
            params: vec!("T".into()),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let map_t = TypeTemplate {
            id: 3,
            name: "Map".into(),
            params: vec!("T".into(), "G".into()),
            attributes: vec!(),
            patterns: vec!(),
            parser: None
        };

        let number = Type::Basic(number_t.id);
        let string = Type::Basic(string_t.id);
        let boolean = Type::Basic(bool_t.id);

        let template_1 = Type::TemplateParam(0);
        let template_2 = Type::Ref(Box::new(Type::TemplateParam(0)));

        assert!(number.bindable_to(&template_1));
        assert!(string.bindable_to(&template_1));
        assert!(boolean.bindable_to(&template_1));
        assert!(!number.bindable_to(&template_2));
        assert!(!string.bindable_to(&template_2));
        assert!(!boolean.bindable_to(&template_2));
        assert!(!template_1.bindable_to(&template_2));
        assert!(!template_2.bindable_to(&template_1));

        let template_1 = Type::Template(vector_t.id, vec!(Type::TemplateParam(0))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::TemplateParam(1))); 

        let binding_1 = Type::Template(vector_t.id, vec!(number.clone()));
        let binding_2 = Type::Template(map_t.id, vec!(number.clone(), string.clone()));

        assert!(binding_1.bindable_to(&template_1));
        assert!(binding_2.bindable_to(&template_2));
        assert!(!binding_2.bindable_to(&template_1));
        assert!(!binding_1.bindable_to(&template_2));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::TemplateParam(0))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), number.clone()));
        let binding_2 = Type::Template(map_t.id, vec!(boolean.clone(), boolean.clone()));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), string.clone()));
        
        assert!(binding_1.bindable_to(&template_1));
        assert!(binding_2.bindable_to(&template_1));
        assert!(!binding_3.bindable_to(&template_1));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::Template(map_t.id, vec!(Type::TemplateParam(1), Type::TemplateParam(0))))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(number.clone(), number.clone()))));
        let binding_2 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_4 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_5 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        
        assert!(binding_1.bindable_to(&template_1));
        assert!(binding_2.bindable_to(&template_1));
        assert!(binding_3.bindable_to(&template_1));
        assert!(!binding_4.bindable_to(&template_1));
        assert!(!binding_5.bindable_to(&template_1));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::Wildcard)))); 

        let binding_1 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(number.clone(), number.clone()))));
        let binding_2 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_3 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_4 = Type::Template(map_t.id, vec!(number.clone(), Type::Template(map_t.id, vec!(string.clone(), string.clone()))));
        let binding_5 = Type::Template(map_t.id, vec!(string.clone(), Type::Template(map_t.id, vec!(string.clone(), number.clone()))));
        let binding_6 = Type::Template(map_t.id, vec!(boolean.clone(), Type::Template(map_t.id, vec!(boolean.clone(), number.clone()))));
        
        assert!(binding_1.bindable_to(&template_1));
        assert!(binding_2.bindable_to(&template_1));
        assert!(!binding_3.bindable_to(&template_1));
        assert!(!binding_4.bindable_to(&template_1));
        assert!(binding_5.bindable_to(&template_1));
        assert!(binding_6.bindable_to(&template_1));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::TemplateParam(1))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(1), Type::TemplateParam(0))); 

        assert!(template_1.bindable_to(&template_1));
        assert!(template_2.bindable_to(&template_2));
        assert!(!template_1.bindable_to(&template_2));
        assert!(!template_2.bindable_to(&template_1));

        let template_1 = Type::Template(map_t.id, vec!(Type::TemplateParam(0), Type::Template(map_t.id, vec!(Type::TemplateParam(1), Type::TemplateParam(0))))); 
        let template_2 = Type::Template(map_t.id, vec!(Type::TemplateParam(1), Type::Template(map_t.id, vec!(Type::TemplateParam(1), Type::TemplateParam(1))))); 

        assert!(template_1.bindable_to(&template_2));
        assert!(template_2.bindable_to(&template_1));
    }
}

/*
                                                  ╒══════════════════╕
    ============================================= │  STANDARD TYPES  │ =============================================
                                                  ╘══════════════════╛
*/

pub fn standard_types(ctx: &mut NessaContext) {
    ctx.define_type("Number".into(), vec!(), vec!(), vec!(), Some(|_, _, s| s.parse::<Number>().map(Object::new))).unwrap();
    ctx.define_type("String".into(), vec!(), vec!(), vec!(), None).unwrap();

    ctx.define_type("Bool".into(), vec!(), vec!(), vec!(), Some(|_, _, s| 
        if s == "true" || s == "false" {
            Ok(Object::new(s.starts_with('t')))

        } else {
            Err(format!("Unable to parse bool from {}", s))
        }
    )).unwrap();

    ctx.define_type("Array".into(), vec!("Inner".into()), vec!(), vec!(), None).unwrap();
    ctx.define_type("Map".into(), vec!("Key".into(), "Value".into()), vec!(), vec!(), None).unwrap();
    ctx.define_type("ArrayIterator".into(), vec!("Inner".into()), vec!(), vec!(), None).unwrap();
} 