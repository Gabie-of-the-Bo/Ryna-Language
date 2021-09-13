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

#[derive(Debug, PartialEq, Clone)]
pub enum NessaExpr {
    // Compiled
    FunctionName(usize),
    Variable(usize, String),
    CompiledVariableDefinition(usize, String, Type, Box<NessaExpr>),
    FunctionCall(usize, Vec<Type>, Vec<NessaExpr>),
    CompiledFor(usize, String, Box<NessaExpr>, Vec<NessaExpr>),

    // Uncompiled
    Literal(Object),
    NameReference(String),

    UnaryOperation(usize, Box<NessaExpr>),
    BinaryOperation(usize, Box<NessaExpr>, Box<NessaExpr>),
    NaryOperation(usize, Vec<Type>, Box<NessaExpr>, Vec<NessaExpr>),

    VariableDefinition(String, Type, Box<NessaExpr>),
    VariableAssignment(String, Box<NessaExpr>),
    FunctionDefinition(String, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>),
    PrefixOperatorDefinition(String, usize),
    PostfixOperatorDefinition(String, usize),
    BinaryOperatorDefinition(String, usize),
    NaryOperatorDefinition(String, String, usize),
    ClassDefinition(String, Vec<String>,Vec<(String, Type)>),

    PrefixOperationDefinition(usize, String, Type, Type, Vec<NessaExpr>),
    PostfixOperationDefinition(usize, String, Type, Type, Vec<NessaExpr>),
    BinaryOperationDefinition(usize, (String, Type), (String, Type), Type, Vec<NessaExpr>),
    NaryOperationDefinition(usize, (String, Type), Vec<(String, Type)>, Type, Vec<NessaExpr>),

    If(Box<NessaExpr>, Vec<NessaExpr>, Vec<(NessaExpr, Vec<NessaExpr>)>, Option<Vec<NessaExpr>>),
    For(String, Box<NessaExpr>, Vec<NessaExpr>),
    Return(Box<NessaExpr>)
}

impl NessaExpr {
    fn compile_types(&mut self, templates: &Vec<String>) {
        match self {
            NessaExpr::VariableDefinition(_, t, e) => {
                t.compile_templates(templates);
                e.compile_types(templates);
            }

            NessaExpr::UnaryOperation(_, e) => e.compile_types(templates),
            NessaExpr::BinaryOperation(_, a, b) => {
                a.compile_types(templates);
                b.compile_types(templates);
            },
            NessaExpr::NaryOperation(_, t, a, b) => {
                t.iter_mut().for_each(|i| i.compile_templates(templates));
                a.compile_types(templates);
                b.iter_mut().for_each(|i| i.compile_types(templates));
            },

            NessaExpr::If(h, ib, ei, eb) => {
                h.compile_types(templates);
                ib.iter_mut().for_each(|i| i.compile_types(templates));

                ei.iter_mut().for_each(|(ei_h, ei_b)| {
                    ei_h.compile_types(templates); 
                    ei_b.iter_mut().for_each(|i| i.compile_types(templates));                   
                });

                if let Some(eb_inner) = eb {
                    eb_inner.iter_mut().for_each(|i| i.compile_types(templates));
                }
            }
            NessaExpr::For(_, c, b) => {
                c.compile_types(templates);
                b.iter_mut().for_each(|i| i.compile_types(templates));
            },

            NessaExpr::Return(e) => e.compile_types(templates),

            _ => {}
        }
    }
}

fn spaces<'a>() -> Parser<'a, char, ()> {
    return one_of(" \t\r\n").repeat(0..).discard();
}

fn spaces_1<'a>() -> Parser<'a, char, ()> {
    return one_of(" \t\r\n").repeat(1..).discard();
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

    fn identifier_parser(&self) -> Parser<char, String> {
        return (
            is_a(|i: char| i.is_alphabetic() || i == '_').repeat(1..) + 
            is_a(|i: char| i.is_alphanumeric() || i == '_').repeat(0..)
        ).name("Variable name").map(move |(a, b)| format!("{}{}", a.iter().collect::<String>(), b.iter().collect::<String>()));
    }

    /*
        ╒═════════════════╕
        │ Type subparsers │
        ╘═════════════════╛
    */

    fn basic_type_parser(&self) -> Parser<char, Type> {
        return self.identifier_parser().map(move |n| Type::Basic(self.get_type_id(n)));
    }

    fn template_type_parser(&self) -> Parser<char, Type> {
        return sym('\'') * self.identifier_parser().map(move |n| Type::TemplateParamStr(n));
    }

    fn constant_reference_type_parser(&self) -> Parser<char, Type> {
        return (sym('&') * call(move || self.type_parser(true, true))).name("Constant reference").map(|i| Type::Ref(Box::new(i)));
    }

    fn mutable_reference_type_parser(&self) -> Parser<char, Type> {
        return (sym('&').repeat(2) * spaces() * call(move || self.type_parser(true, true))).name("Mutable reference").map(|i| Type::MutRef(Box::new(i)))
    }

    fn parametric_type_parser(&self) -> Parser<char, Type> {
        return (self.basic_type_parser() - spaces() + (
            sym('<') * spaces() * list(call(move || self.type_parser(true, true)), spaces() * sym(',')  * spaces()) - spaces() * sym('>')
        
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
        return (sym('(') * spaces() * list(call(move || self.type_parser(true, true)), spaces() * sym(',')  * spaces()) - spaces() * sym(')'))
            .name("And type").map(|args| if args.len() == 1 { args[0].clone() } else { Type::And(args) });
    }

    fn or_type_parser(&self, include_func: bool) -> Parser<char, Type> {
        return list(call(move || self.type_parser(false, include_func)), spaces() * sym('|')  * spaces()).name("Or type")
            .map(|args| if args.len() == 1 { args[0].clone() } else { Type::Or(args) });
    }

    fn function_type_parser(&self, include_or: bool) -> Parser<char, Type> {
        return (call(move || self.type_parser(include_or, false)) + spaces() * tag("=>")  * spaces() * call(move || self.type_parser(include_or, true))).name("Function type")
            .map(|(from, to)| Type::Function(Box::new(from), Box::new(to)));
    }

    fn type_parser(&self, include_or: bool, include_func: bool) -> Parser<char, Type> {
        let mut res = if include_func {
            self.function_type_parser(include_or) | self.mutable_reference_type_parser()

        } else{ 
            self.mutable_reference_type_parser() 
        } 
        | self.constant_reference_type_parser()
        | self.parametric_type_parser()
        | self.empty_type_parser()
        | self.and_type_parser();
        
        // The or case is excluded in order to properly parse types without using parentheses
        if include_or {
            res = res | self.or_type_parser(include_func);
        }
        
        res = res | self.wildcard_type_parser()
                  | self.template_type_parser()
                  | self.basic_type_parser();

        return res;
    }

    /*
        ╒═════════════════╕
        │ Expr subparsers │
        ╘═════════════════╛
    */

    fn bool_parser(&self) -> Parser<char, bool> {
        return (tag("true") | tag("false")).map(|i| i == "true");
    }

    fn number_parser(&self) -> Parser<char, Number> {
        return (tag("-").opt() + is_a(|i: char| i.is_digit(10)).repeat(1..))
            .map(|(s, i)| Number::from(format!("{}{}", s.unwrap_or(""), i.iter().collect::<String>())));
    }

    fn string_parser(&self) -> Parser<char, String> {
        return (sym('"').discard() * none_of("\"").repeat(0..) - sym('"').discard()).map(|i| i.iter().collect());
    }

    fn literal_parser(&self) -> Parser<char, NessaExpr> {
        return self.number_parser().name("Numeric literal").map(|i| NessaExpr::Literal(Object::new(i)))
            | self.bool_parser().name("Boolean literal").map(|i| NessaExpr::Literal(Object::new(i)))
            | self.string_parser().name("String literal").map(|i| NessaExpr::Literal(Object::new(i)))
    }

    fn variable_parser(&self) -> Parser<char, NessaExpr> {
        return self.identifier_parser().map(|i| NessaExpr::NameReference(i));
    }

    fn return_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("return") * spaces_1() * 
            call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new())) - 
            spaces() - sym(';')
        ).map(|e| NessaExpr::Return(Box::new(e)));
    }

    fn variable_assignment_parser(&self) -> Parser<char, NessaExpr> {
        return (
            (spaces() * self.identifier_parser()) -
            (spaces() - sym('=').discard() - spaces()) +
            (call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()))) - 
            spaces() - sym(';')
        
        ).map(|(n, v)| NessaExpr::VariableAssignment(n, Box::new(v)));
    }

    fn variable_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (
            (spaces() * tag("let").discard() * spaces_1() * self.identifier_parser()) +
            ((spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true))).opt() - spaces() - sym('=').discard() - spaces()) +
            (call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()))) - 
            spaces() - sym(';')
        
        ).map(|((n, t), v)| NessaExpr::VariableDefinition(n, t.unwrap_or(Type::Wildcard), Box::new(v)));
    }

    fn function_header_parser(&self) -> Parser<char, ((String, Option<Vec<String>>), (Vec<(String, Type)>, Type))> {
        return (spaces() * tag("fn").discard() * spaces_1() * self.identifier_parser()) +
            (
                spaces() * sym('<') * spaces() *
                list(self.identifier_parser(), spaces() * sym(',') * spaces()) -
                spaces() * sym('>') * spaces()
            ).opt() - spaces() +
            (
                spaces() * sym('(').discard() * spaces() * 
                list(
                    self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)), 
                    spaces() * sym(',') - spaces()
                ) + 
                (spaces() - sym(')').discard() - spaces()) *
                spaces() * tag("->") * spaces() * call(move || self.type_parser(true, true))
            );
    }

    fn unary_prefix_operation_header_parser(&self) -> Parser<char, (((usize, String), Type), Type)> {
        return spaces() * tag("op").discard() * spaces_1() * 
            self.unary_ops.iter()
            .filter(|i| {
                if let Operator::Unary{prefix, ..} = i {
                    return *prefix;
                }

                unreachable!();
            })
            .map(|i| {
                if let Operator::Unary{representation: r, id, ..} = i {
                    return tag(r.as_str()).map(move |_| *id);
                }

                unreachable!();
            }).reduce(|a, b| a | b).unwrap() - spaces() +
            sym('(') * spaces() *
            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - 
            spaces() * sym(')') * spaces() +
            tag("->") * spaces() * call(move || self.type_parser(true, true));
    }

    fn unary_postfix_operation_header_parser(&self) -> Parser<char, (((String, Type), usize), Type)> {
        return spaces() * tag("op").discard() * spaces_1() * 
            sym('(') * spaces() *
            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - 
            spaces() * sym(')') * spaces() +
            self.unary_ops.iter()
            .filter(|i| {
                if let Operator::Unary{prefix, ..} = i {
                    return !*prefix;
                }

                unreachable!();
            })
            .map(|i| {
                if let Operator::Unary{representation: r, id, ..} = i {
                    return tag(r.as_str()).map(move |_| *id);
                }

                unreachable!();
            }).reduce(|a, b| a | b).unwrap() - spaces() +
            tag("->") * spaces() * call(move || self.type_parser(true, true));
    }

    fn binary_operation_header_parser(&self) -> Parser<char, (((((String, Type), usize), String), Type), Type)> {
        return spaces() * tag("op").discard() * spaces_1() * 
            sym('(') * spaces() *
            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - 
            spaces() * sym(')') * spaces() +
            self.binary_ops.iter()
            .map(|i| {
                if let Operator::Binary{representation: r, id, ..} = i {
                    return tag(r.as_str()).map(move |_| *id);
                }

                unreachable!();
            }).reduce(|a, b| a | b).unwrap() - spaces() +
            sym('(') * spaces() *
            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - 
            spaces() * sym(')') * spaces() +
            tag("->") * spaces() * call(move || self.type_parser(true, true));
    }

    fn nary_operation_header_parser(&self) -> Parser<char, (((String, Type), (usize, Vec<(String, Type)>)), Type)> {
        return spaces() * tag("op").discard() * spaces_1() * 
            sym('(') * spaces() *
            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - 
            spaces() * sym(')') * spaces() +
            self.nary_ops.iter()
            .map(|i| {
                if let Operator::Nary{open_rep: or, close_rep: cr, id, ..} = i {
                    return tag(or.as_str()).map(move |_| *id) - spaces() + 
                        list(
                            self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)), 
                            spaces() * sym(',') * spaces()
                        ) - spaces() * tag(cr.as_str()).map(move |_| *id);
                }

                unreachable!();
            }).reduce(|a, b| a | b).unwrap() - spaces() +
            tag("->") * spaces() * call(move || self.type_parser(true, true));
    }

    fn unary_prefix_operation_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.unary_prefix_operation_header_parser() + self.code_block_parser())
            .map(|((((id, n), t), r), b)| NessaExpr::PrefixOperationDefinition(id, n, t, r, b))
    }

    fn unary_postfix_operation_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.unary_postfix_operation_header_parser() + self.code_block_parser())
            .map(|((((n, t), id), r), b)| NessaExpr::PostfixOperationDefinition(id, n, t, r, b))
    }

    fn binary_operation_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.binary_operation_header_parser() + self.code_block_parser())
            .map(|((((((n1, t1), id), n2), t2), r), b)| NessaExpr::BinaryOperationDefinition(id, (n1, t1), (n2, t2), r, b))
    }

    fn nary_operation_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.nary_operation_header_parser() + self.code_block_parser())
            .map(|((((n, t), (id, a)), r), b)| NessaExpr::NaryOperationDefinition(id, (n, t), a, r, b))
    }

    fn operation_definition_parser(&self) -> Parser<char, NessaExpr> {
        return self.unary_prefix_operation_definition_parser()
            | self.unary_postfix_operation_definition_parser()
            | self.binary_operation_definition_parser()
            | self.nary_operation_definition_parser();
    }

    fn class_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("class").discard() * spaces_1() * self.identifier_parser() - spaces() +
            (
                spaces() * sym('<') * spaces() *
                list(self.identifier_parser(), spaces() * sym(',') * spaces()) -
                spaces() * sym('>') * spaces()
            ).opt() - spaces() * sym('{') * spaces() +
            list(
                self.identifier_parser() + spaces() * sym(':').discard() * spaces() * call(move || self.type_parser(true, true)) - spaces() * sym(';'), 
                spaces()
            ) -
            spaces() * sym('}') * spaces()
        
        ).map(|((n, t), a)| NessaExpr::ClassDefinition(n, t.unwrap_or_default(), a)).map(|mut c| match &mut c {
            NessaExpr::ClassDefinition(_, temp, a) => {
                a.into_iter().for_each(|(_, t)| t.compile_templates(&temp));
                return c;
            },

            _ => unreachable!()
        });
    }

    fn if_header_parser(&self) -> Parser<char, NessaExpr> {
        return spaces() * tag("if").discard() * spaces_1() * call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()));
    }

    fn if_else_header_parser(&self) -> Parser<char, NessaExpr> {
        return spaces() * tag("else").discard() * spaces_1() * tag("if").discard() * spaces_1() * call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()));
    }

    fn else_header_parser(&self) -> Parser<char, ()> {
        return spaces() * tag("else").discard() * spaces();
    }

    fn unary_prefix_operator_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("unary").discard() * spaces_1() * tag("prefix").discard() * spaces_1() * tag("op").discard() * spaces_1() * 
            self.string_parser() +
            spaces() * sym('(') * spaces() * 
            is_a(|i: char| i.is_digit(10)).repeat(1..).map(|i| i.iter().collect::<String>().parse().unwrap()) - 
            spaces() * sym(')') * spaces() * sym(';')

        ).map(|(n, p)| NessaExpr::PrefixOperatorDefinition(n, p));
    }

    fn unary_postfix_operator_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("unary").discard() * spaces_1() * tag("postfix").discard() * spaces_1() * tag("op").discard() * spaces_1() * 
            self.string_parser() +
            spaces() * sym('(') * spaces() * 
            is_a(|i: char| i.is_digit(10)).repeat(1..).map(|i| i.iter().collect::<String>().parse().unwrap()) - 
            spaces() * sym(')') * spaces() * sym(';')

        ).map(|(n, p)| NessaExpr::PostfixOperatorDefinition(n, p));
    }

    fn binary_operator_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("binary").discard() * spaces_1() * tag("op").discard() * spaces_1() * 
            self.string_parser() +
            spaces() * sym('(') * spaces() * 
            is_a(|i: char| i.is_digit(10)).repeat(1..).map(|i| i.iter().collect::<String>().parse().unwrap()) - 
            spaces() * sym(')') * spaces() * sym(';')

        ).map(|(n, p)| NessaExpr::BinaryOperatorDefinition(n, p));
    }

    fn nary_operator_parser(&self) -> Parser<char, NessaExpr> {
        return (
            spaces() * tag("nary").discard() * spaces_1() * tag("op").discard() * spaces_1() * 
            tag("from").discard() * spaces_1() * self.string_parser() - spaces_1() +
            tag("to").discard() * spaces_1() * self.string_parser() - spaces_1() +
            spaces() * sym('(') * spaces() * 
            is_a(|i: char| i.is_digit(10)).repeat(1..).map(|i| i.iter().collect::<String>().parse().unwrap()) - 
            spaces() * sym(')') * spaces() * sym(';')

        ).map(|((o, c), p)| NessaExpr::NaryOperatorDefinition(o, c, p));
    }

    fn for_header_parser(&self) -> Parser<char, (String, NessaExpr)> {
        return spaces() * tag("for").discard() * spaces_1() * self.identifier_parser() - spaces_1() * tag("in").discard() * spaces_1() +
            call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new()));
    }

    fn code_block_parser(&self) -> Parser<char, Vec<NessaExpr>> {
        return spaces() * sym('{') * spaces() *
            list(call(move || self.nessa_line_parser()), spaces()) -
            spaces() - sym('}') - spaces();
    }

    fn function_definition_parser(&self) -> Parser<char, NessaExpr> {
        return (self.function_header_parser() + self.code_block_parser())
            .map(|(((n, t), (mut a, mut r)), mut b)| {
                let u_t = t.unwrap_or_default();
                
                a.iter_mut().for_each(|(_, i)| i.compile_templates(&u_t));
                r.compile_templates(&u_t);
                b.iter_mut().for_each(|e| e.compile_types(&u_t));

                NessaExpr::FunctionDefinition(n, u_t, a, r, b)
            })
    }

    fn if_parser(&self) -> Parser<char, NessaExpr> {
        return (
            self.if_header_parser() + self.code_block_parser() + 
            (self.if_else_header_parser() + self.code_block_parser()).repeat(0..) +
            (self.else_header_parser() * self.code_block_parser()).opt()
        
        ).map(|(((h, w), b), e)| NessaExpr::If(Box::new(h), w, b, e))
    }

    fn for_parser(&self) -> Parser<char, NessaExpr> {
        return (self.for_header_parser() + self.code_block_parser())
            .map(|((i, c), b)| NessaExpr::For(i, Box::new(c), b))
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
            let n_ex_2 = n_ex.clone();
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
                (sym('<') * spaces() * list(call(move || self.type_parser(true, true)), spaces() * sym(',')  * spaces()) - spaces() * sym('>')).opt() +
                tag(or.as_str()).map(move |_| id) +
                spaces() *
                list(call(move || self.nessa_expr_parser(b_ex.clone(), n_ex.clone())), spaces() * sym(',') * spaces()) -
                tag(cr.as_str()).discard()
            
            ).map(|(((a, t), id), b)| NessaExpr::NaryOperation(*id, t.unwrap_or_default(), Box::new(a), b));
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
    
    fn operator_defition_parser(&self) -> Parser<char, NessaExpr> {
        return self.unary_prefix_operator_parser()
            | self.unary_postfix_operator_parser()
            | self.binary_operator_parser()
            | self.nary_operator_parser();
    }

    fn nessa_expr_parser(&self, b_ex: HashSet<usize>, n_ex: HashSet<usize>) -> Parser<char, NessaExpr> {
        return call(move || self.parenthesized_expr_parser())
            | call(move || self.operation_parser(b_ex.clone(), n_ex.clone()))
            | self.literal_parser()
            | self.variable_parser();
    }

    fn nessa_line_parser(&self) -> Parser<char, NessaExpr> {
        return call(move || self.variable_definition_parser())
            | call(move || self.variable_assignment_parser())
            | call(move || self.return_parser())
            | call(move || self.if_parser())
            | call(move || self.for_parser())
            | call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new())) - spaces() - sym(';');
    }

    fn nessa_global_parser(&self) -> Parser<char, NessaExpr> {
        return call(move || self.variable_definition_parser())
            | call(move || self.variable_assignment_parser())
            | call(move || self.return_parser())
            | call(move || self.if_parser())
            | call(move || self.for_parser())
            | call(move || self.class_definition_parser())
            | call(move || self.function_definition_parser())
            | call(move || self.operation_definition_parser())
            | call(move || self.nessa_expr_parser(HashSet::new(), HashSet::new())) - spaces() - sym(';');
    }

    pub fn nessa_parser(&self) -> Parser<char, Vec<NessaExpr>> {
        return spaces() * list(self.nessa_global_parser(), spaces()) - spaces();
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

        let basic_func_str = "Number => String".chars().collect::<Vec<_>>();
        let complex_func_str = "(Number, Array<Bool>) => Map<Number, *>".chars().collect::<Vec<_>>();

        let parser = ctx.type_parser(true, true);
        
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

        let basic_func = parser.parse(&basic_func_str).unwrap();
        let complex_func = parser.parse(&complex_func_str).unwrap();

        assert_eq!(basic_func, Type::Function(Box::new(Type::Basic(0)), Box::new(Type::Basic(1))));
        assert_eq!(complex_func, Type::Function(
            Box::new(Type::And(vec!(
                Type::Basic(0),
                Type::Template(3, vec!(Type::Basic(2)))
            ))), 
            Box::new(Type::Template(4, vec!(
                Type::Basic(0),
                Type::Wildcard
            )))
        ));
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
        let template_func_str = "funct<Number>(5)".chars().collect::<Vec<_>>();

        let parser = ctx.operation_parser(HashSet::new(), HashSet::new());

        let number = parser.parse(&number_str).unwrap();
        let var = parser.parse(&var_str).unwrap();
        let n_var = parser.parse(&n_var_str).unwrap();
        let n_call = parser.parse(&n_call_str).unwrap();
        let template_func = parser.parse(&template_func_str).unwrap();

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
            vec!(),
            Box::new(NessaExpr::Literal(Object::new(Number::from(5)))),
            vec!(
                NessaExpr::BinaryOperation(
                    0, 
                    Box::new(NessaExpr::UnaryOperation(0, Box::new(NessaExpr::NameReference("b".into())))),
                    Box::new(NessaExpr::UnaryOperation(1, Box::new(NessaExpr::Literal(Object::new(Number::from(10)))))),
                )
            )
        ));
        assert_eq!(template_func, NessaExpr::NaryOperation(
            0, 
            vec!(Type::Basic(0)),
            Box::new(NessaExpr::NameReference("funct".into())),
            vec!(
                NessaExpr::Literal(Object::new(Number::from(5)))
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

        assert_eq!(number_header, (("test".into(), None), (vec!(("a".into(), Type::Basic(0))), Type::Basic(0))));
        assert_eq!(ref_header, (("test_2".into(), None), (vec!(("arg".into(), Type::Ref(Box::new(Type::Basic(0))))), Type::MutRef(Box::new(Type::Basic(0))))));
        assert_eq!(two_args_header, (
            ("test_3".into(), None),
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
            ("test_4".into(), None), 
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

    #[test]
    fn function_definition_and_flow_control_parsing() {
        let ctx = standard_ctx();

        let test_1_str = "
        fn test() -> Number {
            let res = 5;

            for i in arr {
                return 7;
            }

            return res;
        }
        ".chars().collect::<Vec<_>>();

        let test_2_str = "
        fn test_2(arg: &Number) -> Number | String {
            let r: Number = arg + 1;

            if r + 1 {
                return \"a\";
            
            } else if arg + 2 {
                r = r + 1;    
            
            } else {
                return 5;    
            }

            return r;
        }
        ".chars().collect::<Vec<_>>();

        let test_3_str = "
        fn test_3<K, V>(key: 'K, value: 'V) -> Map<'K, 'V> {
            let a: 'V | 'K = value + key;
            return a;
        }
        ".chars().collect::<Vec<_>>();

        let parser = ctx.function_definition_parser();

        let test_1 = parser.parse(&test_1_str).unwrap();
        let test_2 = parser.parse(&test_2_str).unwrap();
        let test_3 = parser.parse(&test_3_str).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::FunctionDefinition(
                "test".into(),
                vec!(),
                vec!(),
                Type::Basic(0),
                vec!(
                    NessaExpr::VariableDefinition("res".into(), Type::Wildcard, Box::new(NessaExpr::Literal(Object::new(Number::from(5))))),
                    NessaExpr::For(
                        "i".into(),
                        Box::new(NessaExpr::NameReference("arr".into())),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(7)))))
                        )
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::NameReference("res".into())))
                )
            ) 
        );
        assert_eq!(
            test_2,
            NessaExpr::FunctionDefinition(
                "test_2".into(),
                vec!(),
                vec!(
                    (
                        "arg".into(), 
                        Type::Ref(Box::new(Type::Basic(0)))
                    )
                ),
                Type::Or(vec!(
                    Type::Basic(0),
                    Type::Basic(1)
                )),
                vec!(
                    NessaExpr::VariableDefinition(
                        "r".into(), 
                        Type::Basic(0), 
                        Box::new(NessaExpr::BinaryOperation(
                            0,
                            Box::new(NessaExpr::NameReference("arg".into())),
                            Box::new(NessaExpr::Literal(Object::new(Number::from(1))))
                        ))
                    ),
                    NessaExpr::If(
                        Box::new(NessaExpr::BinaryOperation(
                            0,
                            Box::new(NessaExpr::NameReference("r".into())),
                            Box::new(NessaExpr::Literal(Object::new(Number::from(1))))
                        )),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new("a".to_string()))))
                        ),
                        vec!(
                            (
                                NessaExpr::BinaryOperation(
                                    0,
                                    Box::new(NessaExpr::NameReference("arg".into())),
                                    Box::new(NessaExpr::Literal(Object::new(Number::from(2))))
                                ),
                                vec!(
                                    NessaExpr::VariableAssignment(
                                        "r".into(),
                                        Box::new(NessaExpr::BinaryOperation(
                                            0,
                                            Box::new(NessaExpr::NameReference("r".into())),
                                            Box::new(NessaExpr::Literal(Object::new(Number::from(1))))
                                        ))
                                    )
                                )
                            )
                        ),
                        Some(vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(5)))))
                        ))
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::NameReference("r".into())))
                )
            ) 
        );
        assert_eq!(
            test_3,
            NessaExpr::FunctionDefinition(
                "test_3".into(),
                vec!("K".into(), "V".into()),
                vec!(
                    ("key".into(), Type::TemplateParam(0)),
                    ("value".into(), Type::TemplateParam(1))
                ),
                Type::Template(4, vec!(Type::TemplateParam(0), Type::TemplateParam(1))),
                vec!(
                    NessaExpr::VariableDefinition(
                        "a".into(), 
                        Type::Or(vec!(
                            Type::TemplateParam(0),
                            Type::TemplateParam(1)
                        )), 
                        Box::new(NessaExpr::BinaryOperation(
                            0,
                            Box::new(NessaExpr::NameReference("value".into())),
                            Box::new(NessaExpr::NameReference("key".into())),
                        ))
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::NameReference("a".into())))
                )
            ) 
        );
    }

    #[test]
    fn operator_definition_and_flow_control_parsing() {
        let ctx = standard_ctx();

        let prefix_str = "unary prefix op \"~\" (200);".chars().collect::<Vec<_>>();
        let postfix_str = "unary postfix op \"&\" (300);".chars().collect::<Vec<_>>();
        let binary_str = "binary op \"$\" (400);".chars().collect::<Vec<_>>();
        let nary_str = "nary op from \"`\" to \"´\" (500);".chars().collect::<Vec<_>>();

        let parser = ctx.operator_defition_parser();

        let prefix = parser.parse(&prefix_str).unwrap();
        let postfix = parser.parse(&postfix_str).unwrap();
        let binary = parser.parse(&binary_str).unwrap();
        let nary = parser.parse(&nary_str).unwrap();

        assert_eq!(prefix, NessaExpr::PrefixOperatorDefinition("~".into(), 200));
        assert_eq!(postfix, NessaExpr::PostfixOperatorDefinition("&".into(), 300));
        assert_eq!(binary, NessaExpr::BinaryOperatorDefinition("$".into(), 400));
        assert_eq!(nary, NessaExpr::NaryOperatorDefinition("`".into(), "´".into(), 500));
    }

    #[test]
    fn operation_definition_and_flow_control_parsing() {
        let ctx = standard_ctx();

        let test_1_str = "
        op !(arg: Bool) -> Bool {
            if arg {
                return false;
            }

            return true;
        }
        ".chars().collect::<Vec<_>>();

        let test_2_str = "
        op (arg: Bool)? -> Number | Bool {
            if arg {
                return 5;
            }

            for i in arr {
                return i;
            }

            return true;
        }
        ".chars().collect::<Vec<_>>();

        let test_3_str = "
        op (a: Bool) + (b: Bool) -> Number {
            if a {
                if b {
                    return 2;
                }

                return 1;
            }

            if b {
                return 1;
            }

            return 0;
        }
        ".chars().collect::<Vec<_>>();

        let test_4_str = "
        op (a: Number)[b: Number, c: Number] -> Number {
            return a + b * c;
        }
        ".chars().collect::<Vec<_>>();

        let parser = ctx.operation_definition_parser();

        let test_1 = parser.parse(&test_1_str).unwrap();
        let test_2 = parser.parse(&test_2_str).unwrap();
        let test_3 = parser.parse(&test_3_str).unwrap();
        let test_4 = parser.parse(&test_4_str).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::PrefixOperationDefinition(
                1,
                "arg".into(),
                Type::Basic(2),
                Type::Basic(2),
                vec!(
                    NessaExpr::If(
                        Box::new(NessaExpr::NameReference("arg".into())),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(false))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test_2,
            NessaExpr::PostfixOperationDefinition(
                2,
                "arg".into(),
                Type::Basic(2),
                Type::Or(vec!(
                    Type::Basic(0),
                    Type::Basic(2)
                )),
                vec!(
                    NessaExpr::If(
                        Box::new(NessaExpr::NameReference("arg".into())),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(5)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::For(
                        "i".into(),
                        Box::new(NessaExpr::NameReference("arr".into())),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::NameReference("i".into())))
                        )
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test_3,
            NessaExpr::BinaryOperationDefinition(
                0,
                ("a".into(), Type::Basic(2)),
                ("b".into(), Type::Basic(2)),
                Type::Basic(0),
                vec!(
                    NessaExpr::If(
                        Box::new(NessaExpr::NameReference("a".into())),
                        vec!(
                            NessaExpr::If(
                                Box::new(NessaExpr::NameReference("b".into())),
                                vec!(
                                    NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(2)))))
                                ),
                                vec!(),
                                None
                            ),
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::If(
                        Box::new(NessaExpr::NameReference("b".into())),
                        vec!(
                            NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Box::new(NessaExpr::Literal(Object::new(Number::from(0)))))
                )
            ) 
        );

        assert_eq!(
            test_4,
            NessaExpr::NaryOperationDefinition(
                1,
                ("a".into(), Type::Basic(0)),
                vec!(
                    ("b".into(), Type::Basic(0)),
                    ("c".into(), Type::Basic(0))
                ),
                Type::Basic(0),
                vec!(
                    NessaExpr::Return(Box::new(NessaExpr::BinaryOperation(
                        0,
                        Box::new(NessaExpr::NameReference("a".into())),
                        Box::new(NessaExpr::BinaryOperation(
                            1,
                            Box::new(NessaExpr::NameReference("b".into())),
                            Box::new(NessaExpr::NameReference("c".into()))
                        )
                    )))
                )
            ))
        );
    }

    #[test]
    fn class_definition_parsing() {
        let ctx = standard_ctx();

        let dice_roll_str = "
        class DiceRoll {
            faces: Number;
            rolls: Number;
        }
        ".chars().collect::<Vec<_>>();

        let sync_lists_str = "
        class SyncLists<K, V> {
            from: Array<'K>;
            to: Array<'V>;
        }
        ".chars().collect::<Vec<_>>();

        let parser = ctx.class_definition_parser();

        let dice_roll = parser.parse(&dice_roll_str).unwrap();
        let sync_lists = parser.parse(&sync_lists_str).unwrap();

        assert_eq!(dice_roll, NessaExpr::ClassDefinition(
            "DiceRoll".into(),
            vec!(),
            vec!(
                ("faces".into(), Type::Basic(0)),
                ("rolls".into(), Type::Basic(0))
            )
        ));

        assert_eq!(sync_lists, NessaExpr::ClassDefinition(
            "SyncLists".into(),
            vec!("K".into(), "V".into()),
            vec!(
                ("from".into(), Type::Template(3, vec!(Type::TemplateParam(0)))),
                ("to".into(), Type::Template(3, vec!(Type::TemplateParam(1))))
            )
        ));
    }
}