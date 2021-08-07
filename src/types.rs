/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone)]
pub struct TypeTemplate {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>
}

#[derive(Clone)]
pub enum Type {
    // Empty type (also called void)
    Empty,

    // Simple types
    Basic(usize),

    // References
    Ref(usize),
    MutRef(usize),

    // Algebraic types
    Or(Vec<Type>),
    And(Vec<Type>),

    // Parametric types
    Wildcard,
    Template(usize, Vec<Type>),
}

impl PartialEq for Type {
    fn eq(&self, b: &Self) -> bool {
        return match (self, b) {
            (Type::Empty, Type::Empty) => true,
            (Type::Basic(id_a), Type::Basic(id_b)) => id_a == id_b,
            (Type::Ref(id_a), Type::Ref(id_b)) => id_a == id_b,
            (Type::MutRef(id_a), Type::MutRef(id_b)) => id_a == id_b,
            (Type::Or(va), Type::Or(vb)) => va.iter().all(|i| vb.contains(i)) && vb.iter().all(|i| va.contains(i)),
            (Type::And(va), Type::And(vb)) => va == vb,
            (Type::Wildcard, Type::Wildcard) => true,
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va == vb,
            
            _ => false
        }
    }
}

impl Eq for Type {}

impl Type {
    fn bindable_to(&self, other: &Type) -> bool {
        return match (self, other) {
            (_, Type::Wildcard) => true,

            (a, b) if a == b => true,

            (_, Type::Empty) => false,

            (Type::MutRef(id_a), Type::Ref(id_b)) => id_a == id_b,

            (Type::Or(v), b) => v.iter().all(|i| i.bindable_to(b)),
            (a, Type::Or(v)) => v.iter().any(|i| a.bindable_to(i)),

            (Type::And(va), Type::And(vb)) => va.len() == vb.len() && va.iter().zip(vb).all(|(i, j)| i.bindable_to(j)),
            (Type::Template(id_a, va), Type::Template(id_b, vb)) => id_a == id_b && va.len() == vb.len() && 
                                                                    va.iter().zip(vb).all(|(i, j)| i.bindable_to(j)),

            _ => false
        }
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
    fn binding() {
        let number_t = TypeTemplate {
            id: 0,
            name: "Number".into(),
            params: vec!()
        };

        let string_t = TypeTemplate {
            id: 1,
            name: "String".into(),
            params: vec!()
        };

        let bool_t = TypeTemplate {
            id: 2,
            name: "Bool".into(),
            params: vec!()
        };

        let vector_t = TypeTemplate {
            id: 3,
            name: "Vector".into(),
            params: vec!("T".into())
        };

        let number = Type::Basic(number_t.id);
        let string = Type::Basic(string_t.id);
        let boolean = Type::Basic(bool_t.id);

        assert!(number.bindable_to(&number));
        assert!(string.bindable_to(&string));
        assert!(!number.bindable_to(&string));

        let number_ref = Type::Ref(number_t.id);
        let number_mut = Type::MutRef(number_t.id);

        assert!(number_ref.bindable_to(&number_ref));
        assert!(number_mut.bindable_to(&number_ref));
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

        let vector_number = Type::Template(vector_t.id, vec!(number));
        let vector_string = Type::Template(vector_t.id, vec!(string));
        let vector_number_or_string = Type::Template(vector_t.id, vec!(number_or_string));

        assert!(vector_number.bindable_to(&vector_number));
        assert!(vector_number.bindable_to(&vector_number_or_string));
        assert!(!vector_number.bindable_to(&vector_string));
    }
}