use std::collections::HashSet;
use pom::parser::*;

use crate::object::Object;
use crate::types::Type;
use crate::operations::Operator;
use crate::number::*;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Debug, PartialEq)]
pub enum NessaExpr {
    Literal(Object),
    NameReference(String),

    UnaryOperation(usize, Box<NessaExpr>),
    BinaryOperation(usize, Box<NessaExpr>, Box<NessaExpr>),
    NaryOperation(usize, Box<NessaExpr>, Vec<NessaExpr>),

    VariableDefinition(String, Type, Box<NessaExpr>),
    FunctionDefinition(String, Vec<(String, Type)>, Type, Vec<NessaExpr>),
    PrefixOperatorDefinition(String),
    PostfixOperatorDefinition(String),
    BinaryOperatorDefinition(String),
    NaryOperatorDefinition(String, String),

    If(Box<NessaExpr>, Vec<NessaExpr>),
    For(Box<NessaExpr>, Box<NessaExpr>, Vec<NessaExpr>),
    Return(Box<NessaExpr>)
}

fn spaces<'a>() -> Parser<'a, char, ()> {
    return one_of(" \t\r\n").repeat(0..).discard();
}

impl NessaContext {
    /*
        ╒═══════════════════╕
        │ Auxiliary methods │
        ╘═══════════════════╛
    */

    fn get_type_id(&self, name: String) -> usize {
        return self.type_templates.iter().filter(|t| t.name == name).next().unwrap().id;
    }

    /*
        ╒═════════════════╕
        │ Type subparsers │
        ╘═════════════════╛
    */

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

    /*
        ╒═════════════════╕
        │ Expr subparsers │
        ╘═════════════════╛
    */

    fn variable_name_parser(&self) -> Parser<char, String> {
        return (
            is_a(|i: char| i.is_alphabetic() || i == '_').repeat(1..) + 
            is_a(|i: char| i.is_alphanumeric() || i == '_').repeat(0..)
        ).name("Variable name").map(move |(a, b)| format!("{}{}", a.iter().collect::<String>(), b.iter().collect::<String>()));
    }

    fn bool_parser(&self) -> Parser<char, bool> {
        return (seq(&['t', 'r', 'u', 'e']) | seq(&['f', 'a', 'l', 's', 'e'])).map(|i| i[0] == 't');
    }

    fn number_parser(&self) -> Parser<char, Number> {
        return (tag("-").opt() + is_a(|i: char| i.is_digit(10)).repeat(1..))
            .map(|(s, i)| Number::from(format!("{}{}", s.unwrap_or(""), i.iter().collect::<String>())));
    }

    fn string_parser(&self) -> Parser<char, String> {
        return (sym('"').discard() * not_a(|i: char| i == '"').repeat(0..) - sym('"').discard()).map(|i| i.iter().collect());
    }

    fn literal_parser(&self) -> Parser<char, NessaExpr> {
        return self.number_parser().name("Numeric literal").map(|i| NessaExpr::Literal(Object::new(i)))
            | self.bool_parser().name("Boolean literal").map(|i| NessaExpr::Literal(Object::new(i)))
            | self.string_parser().name("String literal").map(|i| NessaExpr::Literal(Object::new(i)))
    }

    fn variable_parser(&self) -> Parser<char, NessaExpr> {
        return self.variable_name_parser().map(|i| NessaExpr::NameReference(i));
    }

    fn variable_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (
            (spaces() * seq(&['l', 'e', 't']).discard() * spaces() * self.variable_name_parser()) +
            ((spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true))).opt() - spaces() - sym('=').discard() - spaces()) +
            (call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()))) - 
            spaces() - sym(';')
        
        ).map(|((n, t), v)| NessaExpr::VariableDefinition(n, t.unwrap_or(Type::Wildcard), Box::new(v)));
    }

    fn function_header_parser(&self) -> Parser<char, (String, (Vec<(String, Type)>, Type))> {
        return (spaces() * tag("fn").discard() * spaces() * self.variable_name_parser()) +
            (
                spaces() * sym('(').discard() * spaces() * 
                list(
                    self.variable_name_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true)), 
                    spaces() * sym(',') - spaces()
                ) + 
                (spaces() - sym(')').discard() - spaces()) *
                spaces() * tag("->") * spaces() * call(move || self.type_parser(true))
            );
    }

    fn code_block_parser(&self) -> Parser<char, Vec<NessaExpr>> {
        return spaces() * sym('{') * spaces() *
            list(call(move || self.nessa_line_parser()), spaces()) -
            spaces() - sym('}') - spaces();
    }

    fn function_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.function_header_parser() + self.code_block_parser())
            .map(|((n, (a, r)), b)| NessaExpr::FunctionDefinition(n, a, r, b))
    }

    fn unary_operation_parser(&self, id: usize, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        let op = &self.unary_ops[id];

        if let Operator::Unary{id, representation: r, ..} = op {
            return (
                tag(r.as_str()).map(move |_| id) + 
                spaces() *
                call(move || self.nessa_expr_parser(b_ex.clone(), n_ex.clone()))
            
            ).map(|(id, expr)| NessaExpr::UnaryOperation(*id, Box::new(expr)));
        }

        unreachable!();
    }

    fn binary_operation_parser(&self, id: usize, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        let op = &self.binary_ops[id];

        if let Operator::Binary{id, representation: r, ..} = op {
            let mut n_ex_2 = n_ex.clone();
            let mut b_ex_2 = b_ex.clone();
            b_ex_2.insert(*id);

            return (
                call(move || self.nessa_expr_parser(b_ex_2.clone(), n_ex_2.clone())) +
                spaces() *
                tag(r.as_str()).map(move |_| id) +
                spaces() *
                call(move || self.nessa_expr_parser(b_ex.clone(), n_ex.clone()))                
            
            ).map(|((a, id), b)| NessaExpr::BinaryOperation(*id, Box::new(a), Box::new(b)));
        }

        unreachable!();
    }

    fn nary_operation_parser(&self, id: usize, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        let op = &self.nary_ops[id];

        if let Operator::Nary{id, open_rep: or, close_rep: cr, ..} = op {
            let b_ex_2 = b_ex.clone();
            let mut n_ex_2 = n_ex.clone();
            n_ex_2.insert(*id);

            return (
                call(move || self.nessa_expr_parser(b_ex_2.clone(), n_ex_2.clone())) +
                spaces() *
                tag(or.as_str()).map(move |_| id) +
                spaces() *
                list(call(move || self.nessa_expr_parser(b_ex.clone(), n_ex.clone())), spaces() * sym(',') * spaces()) -
                tag(cr.as_str()).discard()
            
            ).map(|((a, id), b)| NessaExpr::NaryOperation(*id, Box::new(a), b));
        }

        unreachable!();
    }

    fn operation_parser(&self, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        let mut ops = self.unary_ops.iter().chain(&self.binary_ops).chain(&self.nary_ops).collect::<Vec<_>>();

        // Sort by precedence in order to iterate from the first to the last 
        ops.sort_by(|a, b| b.get_precedence().cmp(&a.get_precedence()));

        let res = ops.iter()
            .filter(|o| match o {
                Operator::Binary{id, ..} if b_ex.contains(id) => false,
                Operator::Nary{id, ..} if n_ex.contains(id) => false,
                _ => true
            })
            .map(|o| match o {
                Operator::Unary{id, ..} => self.unary_operation_parser(*id, b_ex.clone(), n_ex.clone()),
                Operator::Binary{id, ..} => self.binary_operation_parser(*id, b_ex.clone(), n_ex.clone()),
                Operator::Nary{id, ..} => self.nary_operation_parser(*id, b_ex.clone(), n_ex.clone()),
            })
            .reduce(|a, b| a | b);

        if let Some(p) = res {
            return p;
        }

        unreachable!();
    }

    fn parenthesized_expr_parser(&self) -> Parser<char, NessaExpr> {
        return sym('(') * spaces() * call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new())) - spaces() - sym(')');
    }
    
    fn nessa_expr_parser(&self, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        return self.parenthesized_expr_parser()
            | self.operation_parser(b_ex, n_ex)
            | self.literal_parser()
            | self.variable_parser();
    }

    fn nessa_line_parser(&self) -> Parser<char, NessaExpr> {
        return self.variable_definition_parser()
            | self.nessa_expr_parser(HashSet::new(), HashSet::new()) - spaces() - sym(';');
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
    use crate::context::*;
    use crate::parser::*;

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

    #[test]
    fn literal_parsing() {
        let ctx = standard_ctx();

        let number_str = "123".chars().collect::<Vec<_>>();
        let bool_v_str = "true".chars().collect::<Vec<_>>();
        let string_str = "\"test\"".chars().collect::<Vec<_>>();

        let parser = ctx.literal_parser();

        let number = parser.parse(&number_str).unwrap();
        let bool_v = parser.parse(&bool_v_str).unwrap();
        let string = parser.parse(&string_str).unwrap();

        assert_eq!(number, NessaExpr::Literal(Object::new(Number::from(123))));
        assert_eq!(bool_v, NessaExpr::Literal(Object::new(true)));
        assert_eq!(string, NessaExpr::Literal(Object::new("test".to_string())));
    }

    #[test]
    fn variable_definition_parsing() {
        let ctx = standard_ctx();

        let def_1_str = "let var: Number = a;".chars().collect::<Vec<_>>();
        let def_2_str = "let foo: Array<Number | &String> = 5;".chars().collect::<Vec<_>>();
        let def_3_str = "let bar = \"test\";".chars().collect::<Vec<_>>();
        let def_4_str = "let foobar = false;".chars().collect::<Vec<_>>();

        let parser = ctx.variable_definition_parser();

        let def_1 = parser.parse(&def_1_str).unwrap();
        let def_2 = parser.parse(&def_2_str).unwrap();
        let def_3 = parser.parse(&def_3_str).unwrap();
        let def_4 = parser.parse(&def_4_str).unwrap();

        assert_eq!(def_1, NessaExpr::VariableDefinition("var".into(), Type::Basic(0), Box::new(NessaExpr::NameReference("a".into()))));
        assert_eq!(def_2, NessaExpr::VariableDefinition(
                "foo".into(), 
                Type::Template(3, vec!(Type::Or(vec!(Type::Basic(0), Type::Ref(Box::new(Type::Basic(1))))))), 
                Box::new(NessaExpr::Literal(Object::new(Number::from(5))))
            )
        );
        assert_eq!(def_3, NessaExpr::VariableDefinition("bar".into(), Type::Wildcard, Box::new(NessaExpr::Literal(Object::new("test".to_string())))));
        assert_eq!(def_4, NessaExpr::VariableDefinition("foobar".into(), Type::Wildcard, Box::new(NessaExpr::Literal(Object::new(false)))));
    }

    #[test]
    fn operation_parsing() {
        let ctx = standard_ctx();

        let number_str = "-10".chars().collect::<Vec<_>>();
        let var_str = "-!a".chars().collect::<Vec<_>>();
        let n_var_str = "-5 + a".chars().collect::<Vec<_>>();
        let n_call_str = "5(-b + !10)".chars().collect::<Vec<_>>();

        let parser = ctx.operation_parser(HashSet::new(), HashSet::new());

        let number = parser.parse(&number_str).unwrap();
        let var = parser.parse(&var_str).unwrap();
        let n_var = parser.parse(&n_var_str).unwrap();
        let n_call = parser.parse(&n_call_str).unwrap();

        assert_eq!(number, NessaExpr::UnaryOperation(0, Box::new(NessaExpr::Literal(Object::new(Number::from(10))))));
        assert_eq!(
            var, 
            NessaExpr::UnaryOperation(0, 
            Box::new(NessaExpr::UnaryOperation(1, Box::new(NessaExpr::NameReference("a".into()))))
        ));
        assert_eq!(n_var, NessaExpr::BinaryOperation(
            0, 
            Box::new(NessaExpr::UnaryOperation(0, Box::new(NessaExpr::Literal(Object::new(Number::from(5)))))),
            Box::new(NessaExpr::NameReference("a".into()))
        ));
        assert_eq!(n_call, NessaExpr::NaryOperation(
            0, 
            Box::new(NessaExpr::Literal(Object::new(Number::from(5)))),
            vec!(
                NessaExpr::BinaryOperation(
                    0, 
                    Box::new(NessaExpr::UnaryOperation(0, Box::new(NessaExpr::NameReference("b".into())))),
                    Box::new(NessaExpr::UnaryOperation(1, Box::new(NessaExpr::Literal(Object::new(Number::from(10)))))),
                )
            )
        ));
    }

    #[test]
    fn function_header_parsing() {
        let ctx = standard_ctx();

        let number_header_str = "fn test(a: Number) -> Number".chars().collect::<Vec<_>>();
        let ref_header_str = "fn test_2(arg: &Number) -> &&Number".chars().collect::<Vec<_>>();
        let two_args_header_str = "fn test_3(arg_1: &Number, arg_2: String | Number) -> Number | String".chars().collect::<Vec<_>>();
        let complex_args_header_str = "fn test_4(a: String | &Number, b: &Array<(Bool, Number)>, c: &&*) -> Map<Number, String>".chars().collect::<Vec<_>>();

        let parser = ctx.function_header_parser();

        let number_header = parser.parse(&number_header_str).unwrap();
        let ref_header = parser.parse(&ref_header_str).unwrap();
        let two_args_header = parser.parse(&two_args_header_str).unwrap();
        let complex_args_header = parser.parse(&complex_args_header_str).unwrap();

        assert_eq!(number_header, ("test".into(), (vec!(("a".into(), Type::Basic(0))), Type::Basic(0))));
        assert_eq!(ref_header, ("test_2".into(), (vec!(("arg".into(), Type::Ref(Box::new(Type::Basic(0))))), Type::MutRef(Box::new(Type::Basic(0))))));
        assert_eq!(two_args_header, (
            "test_3".into(), 
            (
                vec!(
                    ("arg_1".into(), Type::Ref(Box::new(Type::Basic(0)))),
                    ("arg_2".into(), Type::Or(vec!(
                        Type::Basic(0),
                        Type::Basic(1)
                    )))
                ),
                Type::Or(vec!(
                    Type::Basic(0),
                    Type::Basic(1)
                ))
            )
        ));
        assert_eq!(complex_args_header, (
            "test_4".into(), 
            (
                vec!(
                    ("a".into(), Type::Or(vec!(
                        Type::Ref(Box::new(Type::Basic(0))),
                        Type::Basic(1)
                    ))),
                    ("b".into(), Type::Ref(Box::new(
                        Type::Template(
                            3,
                            vec!(Type::And(vec!(
                                Type::Basic(2),
                                Type::Basic(0)
                            )))
                        ))
                    )),
                    ("c".into(), Type::MutRef(Box::new(Type::Wildcard)))
                ),
                Type::Template(
                    4,
                    vec!(
                        Type::Basic(0),
                        Type::Basic(1)
                    )
                )
            )
        ));
    }
}