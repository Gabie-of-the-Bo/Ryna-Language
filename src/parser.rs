use pom::parser::*;

use crate::types::Type;
use crate::context::NessaContext;

fn spaces<'a>() -> Parser<'a, char, ()> {
    return one_of(" \t\r\n").repeat(0..).discard();
}

impl NessaContext {
    fn get_type_id(&self, name: String) -> usize {
        return self.type_templates.iter().filter(|t| t.name == name).next().unwrap().id;
    }

    fn basic_type_parser(&self) -> Parser<char, Type> {
        return is_a(|i: char| i.is_alphabetic()).repeat(1..).name("Basic type").map(move |n| Type::Basic(self.get_type_id(n.iter().collect())));
    }

    fn constant_reference_type_parser(&self) -> Parser<char, Type> {
        return (sym('&') * call(move || self.type_parser(true))).name("Constant reference").map(|i| Type::Ref(Box::new(i)));
    }

    fn mutable_reference_type_parser(&self) -> Parser<char, Type> {
        return (sym('&').repeat(2) * spaces() * call(move || self.type_parser(true))).name("Mutable reference").map(|i| Type::MutRef(Box::new(i)))
    }

    fn parametric_type_parser(&self) -> Parser<char, Type> {
        return (self.basic_type_parser() - spaces() + (
            sym('<') * spaces() * list(call(move || self.type_parser(true)), spaces() * sym(',')  * spaces()) - spaces() * sym('>')
        
        )).name("Template type").map(|(i, args)| {
            if let Type::Basic(id) = i {
                return Type::Template(id, args);
            }

            return Type::Empty;
        });
    }

    fn empty_type_parser(&self) -> Parser<char, Type> {
        return (sym('(') * spaces() * sym(')')).name("Empty type").map(|_| Type::Empty);
    }

    fn wildcard_type_parser(&self) -> Parser<char, Type> {
        return sym('*').name("Wildcard type").map(|_| Type::Wildcard);
    }

    fn and_type_parser(&self) -> Parser<char, Type> {
        return (sym('(') * spaces() * list(call(move || self.type_parser(true)), spaces() * sym(',')  * spaces()) - spaces() * sym(')'))
            .name("And type").map(|args| if args.len() == 1 { args[0].clone() } else { Type::And(args) });
    }

    fn or_type_parser(&self) -> Parser<char, Type> {
        return list(call(move || self.type_parser(false)), spaces() * sym('|')  * spaces()).name("Or type")
            .map(|args| if args.len() == 1 { args[0].clone() } else { Type::Or(args) });
    }

    fn type_parser(&self, include_or: bool) -> Parser<char, Type> {
        let mut res = self.mutable_reference_type_parser()
                    | self.constant_reference_type_parser()
                    | self.parametric_type_parser()
                    | self.empty_type_parser()
                    | self.and_type_parser();
        
        // The or case is excluded in order to properly parse types without using parentheses
        if include_or {
            res = res | self.or_type_parser();
        }
        
        res = res | self.wildcard_type_parser()
                  | self.basic_type_parser();

        return res;
    }
}

#[cfg(test)]
mod tests {
    use crate::types::*;
    use crate::context::*;

    #[test]
    fn type_parsing() {
        let ctx = standard_ctx();

        let wildcard_str = "*".chars().collect::<Vec<_>>();
        let empty_str = "()".chars().collect::<Vec<_>>();

        let number_str = "Number".chars().collect::<Vec<_>>();
        let number_ref_str = "&Number".chars().collect::<Vec<_>>();
        let string_mut_str = "&&String".chars().collect::<Vec<_>>();
        let wildcard_mut_str = "&&*".chars().collect::<Vec<_>>();

        let or_str = "Number | &&String".chars().collect::<Vec<_>>();
        let and_str = "(Number, &&String, &Bool)".chars().collect::<Vec<_>>();
        let and_one_str = "(Number)".chars().collect::<Vec<_>>();

        let array_str = "Array<Number>".chars().collect::<Vec<_>>();
        let map_str = "Map<Number, String>".chars().collect::<Vec<_>>();
        let map_refs_str = "&Map<&Number, &&String>".chars().collect::<Vec<_>>();

        let parser = ctx.type_parser(true);
        
        let wildcard = parser.parse(&wildcard_str).unwrap();
        let empty = parser.parse(&empty_str).unwrap();

        assert_eq!(wildcard, Type::Wildcard);
        assert_eq!(empty, Type::Empty);

        let number = parser.parse(&number_str).unwrap();
        let number_ref = parser.parse(&number_ref_str).unwrap();
        let string_mut = parser.parse(&string_mut_str).unwrap();
        let wildcard_mut = parser.parse(&wildcard_mut_str).unwrap();

        assert_eq!(number, Type::Basic(0));
        assert_eq!(number_ref, Type::Ref(Box::new(Type::Basic(0))));
        assert_eq!(string_mut, Type::MutRef(Box::new(Type::Basic(1))));
        assert_eq!(wildcard_mut, Type::MutRef(Box::new(Type::Wildcard)));

        let or = parser.parse(&or_str).unwrap();
        let and = parser.parse(&and_str).unwrap();
        let and_one = parser.parse(&and_one_str).unwrap();

        assert_eq!(or, Type::Or(vec!(Type::Basic(0), Type::MutRef(Box::new(Type::Basic(1))))));
        assert_eq!(and, Type::And(vec!(Type::Basic(0), Type::MutRef(Box::new(Type::Basic(1))), Type::Ref(Box::new(Type::Basic(2))))));
        assert_eq!(and_one, Type::Basic(0));

        let array = parser.parse(&array_str).unwrap();
        let map = parser.parse(&map_str).unwrap();
        let map_refs = parser.parse(&map_refs_str).unwrap();

        assert_eq!(array, Type::Template(3, vec!(Type::Basic(0))));
        assert_eq!(map, Type::Template(4, vec!(Type::Basic(0), Type::Basic(1))));
        assert_eq!(map_refs, Type::Ref(Box::new(Type::Template(4, vec!(Type::Ref(Box::new(Type::Basic(0))), Type::MutRef(Box::new(Type::Basic(1))))))));
    }
}