use std::collections::HashMap;

use nom::{
    IResult,
    combinator::{map, map_res, opt, eof},
    bytes::complete::{take_while, take_while1, tag},
    sequence::{tuple, delimited, terminated},
    branch::alt,
    character::complete::{multispace0, multispace1},
    multi::{separated_list0, separated_list1}
};

use bit_set::BitSet;

use crate::operations::Operator;
use crate::object::Object;
use crate::number::Number;
use crate::types::Type;
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
    Variable(usize, String, Type),
    CompiledVariableDefinition(usize, String, Type, Box<NessaExpr>),
    CompiledVariableAssignment(usize, String, Box<NessaExpr>),
    FunctionCall(usize, Vec<Type>, Vec<NessaExpr>),
    CompiledFor(usize, String, Box<NessaExpr>, Vec<NessaExpr>),

    CompiledFunctionDefinition(usize, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>, usize),
    CompiledPrefixOperationDefinition(usize, String, Type, Type, Vec<NessaExpr>, usize),
    CompiledPostfixOperationDefinition(usize, String, Type, Type, Vec<NessaExpr>, usize),
    CompiledBinaryOperationDefinition(usize, (String, Type), (String, Type), Type, Vec<NessaExpr>, usize),
    CompiledNaryOperationDefinition(usize, (String, Type), Vec<(String, Type)>, Type, Vec<NessaExpr>, usize),

    // Uncompiled
    Literal(Object),
    NameReference(String),

    UnaryOperation(usize, Box<NessaExpr>),
    BinaryOperation(usize, Box<NessaExpr>, Box<NessaExpr>),
    NaryOperation(usize, Vec<Type>, Box<NessaExpr>, Vec<NessaExpr>),

    VariableDefinition(String, Type, Box<NessaExpr>),
    VariableAssignment(String, Box<NessaExpr>),
    FunctionDefinition(usize, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>),
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
    pub fn compile_types(&mut self, templates: &Vec<String>) {
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

impl NessaContext {
    /*
        ╒═══════════════════╕
        │ Auxiliary methods │
        ╘═══════════════════╛
    */
    
    fn get_type_id(&self, name: String) -> Result<usize, String> {
        return self.type_templates.iter().filter(|t| t.name == name).next().map(|i| i.id).ok_or(format!("No type with name {}", name));
    }
    
    fn get_function_id(&self, name: String) -> usize {
        return self.functions.iter().filter(|t| t.name == name).next().unwrap().id;
    }

    fn identifier_parser<'a>(&self, input: &'a str) -> IResult<&'a str, String> {
        return map(
            tuple((
                take_while1(|c: char| c == '_' || c.is_alphabetic()),
                take_while(|c: char| c == '_' || c.is_alphanumeric())
            )),
            |(a, b)| format!("{}{}", a, b)
        )(input);
    }

    /*
        ╒═════════════════╕
        │ Type subparsers │
        ╘═════════════════╛
    */

    fn wildcard_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(tag("*"), |_| Type::Wildcard)(input);
    }

    fn empty_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(tag("()"), |_| Type::Empty)(input);
    }

    fn basic_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map_res(|input| self.identifier_parser(input), |n| Result::<_, String>::Ok(Type::Basic(self.get_type_id(n)?)))(input);
    }

    fn template_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(
            tuple((
                tag("'"),
                |input| self.identifier_parser(input)
            )), 
            |(_, n)| Type::TemplateParamStr(n)
        )(input);
    }

    fn constant_reference_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(
            tuple((
                tag("&"),
                |input| self.type_parser(input)
            )), 
            |(_, t)| Type::Ref(Box::new(t))
        )(input);
    }

    fn mutable_reference_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(
            tuple((
                tag("&&"),
                |input| self.type_parser(input)
            )), 
            |(_, t)| Type::MutRef(Box::new(t))
        )(input);
    }

    fn or_type_parser<'a>(&self, input: &'a str, func: bool) -> IResult<&'a str, Type> {
        return map(
            separated_list1(
                tuple((multispace0, tag("|"), multispace0)), 
                |input| self.type_parser_wrapper(input, func, false)
            ),
            |t| if t.len() > 1 { Type::Or(t) } else { t[0].clone() }
        )(input);
    }

    fn and_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map(
            delimited(
                tag("("),
                separated_list1(
                    tuple((multispace0, tag(","), multispace0)), 
                    |input| self.type_parser(input)
                ),
                tag(")")
            ),
            |t| if t.len() > 1 { Type::And(t) } else { t[0].clone() }
        )(input);
    }

    fn parametric_type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return map_res(
            tuple((
                |input| self.identifier_parser(input),
                multispace0,
                tag("<"),
                multispace0,
                separated_list1(
                    tuple((multispace0, tag(","), multispace0)), 
                    |input| self.type_parser(input)
                ),
                multispace0,
                tag(">")
            )),
            |(n, _, _, _, t, _, _)| Result::<_, String>::Ok(Type::Template(self.get_type_id(n)?, t))
        )(input);
    }

    fn function_type_parser<'a>(&self, input: &'a str, or: bool) -> IResult<&'a str, Type> {
        return map(
            tuple((
                |input| self.type_parser_wrapper(input, false, or),
                multispace0,
                tag("=>"),
                multispace0,
                |input| self.type_parser_wrapper(input, false, or)
            )),
            |(f, _, _, _, t)| Type::Function(Box::new(f), Box::new(t))
        )(input);
    }

    fn type_parser_wrapper<'a>(&self, input: &'a str, func: bool, or: bool) -> IResult<&'a str, Type> {
        return match (func, or) {
            (true, true) => alt((
                |input| self.function_type_parser(input, or),
                |input| self.empty_type_parser(input),
                |input| self.wildcard_type_parser(input),
                |input| self.mutable_reference_type_parser(input),
                |input| self.constant_reference_type_parser(input),
                |input| self.parametric_type_parser(input),
                |input| self.and_type_parser(input),
                |input| self.or_type_parser(input, func),
                |input| self.template_type_parser(input),
                |input| self.basic_type_parser(input)
            ))(input),

            (false, true) => alt((
                |input| self.empty_type_parser(input),
                |input| self.wildcard_type_parser(input),
                |input| self.mutable_reference_type_parser(input),
                |input| self.constant_reference_type_parser(input),
                |input| self.parametric_type_parser(input),
                |input| self.and_type_parser(input),
                |input| self.or_type_parser(input, func),
                |input| self.template_type_parser(input),
                |input| self.basic_type_parser(input)
            ))(input),

            (true, false) => alt((
                |input| self.function_type_parser(input, or),
                |input| self.empty_type_parser(input),
                |input| self.wildcard_type_parser(input),
                |input| self.mutable_reference_type_parser(input),
                |input| self.constant_reference_type_parser(input),
                |input| self.parametric_type_parser(input),
                |input| self.and_type_parser(input),
                |input| self.template_type_parser(input),
                |input| self.basic_type_parser(input)
            ))(input),
            
            (false, false) => alt((
                |input| self.empty_type_parser(input),
                |input| self.wildcard_type_parser(input),
                |input| self.mutable_reference_type_parser(input),
                |input| self.constant_reference_type_parser(input),
                |input| self.parametric_type_parser(input),
                |input| self.and_type_parser(input),
                |input| self.template_type_parser(input),
                |input| self.basic_type_parser(input)
            ))(input),
        };
    }

    fn type_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Type> {
        return self.type_parser_wrapper(input, true, true);
    }

    /*
        ╒═════════════════╕
        │ Expr subparsers │
        ╘═════════════════╛
    */

    fn bool_parser<'a>(&self, input: &'a str) -> IResult<&'a str, bool> {
        return alt((
            map(tag("true"), |_| true),
            map(tag("false"), |_| false),
        ))(input);
    }

    fn number_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Number> {
        return map(
            tuple((
                opt(tag("-")),
                take_while1(|c: char| c.is_digit(10)),
                opt(tuple((
                    tag("."),
                    take_while1(|c: char| c.is_digit(10))
                )))
            )),
            |(s, n, d)| Number::from(format!("{}{}{}", s.unwrap_or_default(), n, d.unwrap_or_default().1).as_str())
        )(input);
    }

    fn string_parser<'a>(&self, input: &'a str) -> IResult<&'a str, String> {
        return map(
            delimited(
                tag("\""), 
                take_while(|c| c != '\"'), 
                tag("\"")
            ),
            String::from
        )(input);
    }
    
    fn literal_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            alt((
                map(|input| self.bool_parser(input), |b| Object::new(b)),
                map(|input| self.number_parser(input), |n| Object::new(n)),
                map(|input| self.string_parser(input), |s| Object::new(s))
            )),
            |o| NessaExpr::Literal(o)
        )(input);
    }
    
    fn variable_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            |input| self.identifier_parser(input),
            NessaExpr::NameReference
        )(input);
    }
    
    fn prefix_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag(rep),
                multispace0,
                |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, cache)
            )),
            |(_, _, e)| NessaExpr::UnaryOperation(id, Box::new(e))
        )(input);
    }
    
    fn postfix_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = cache_post.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            };
       
        } else{
            let mut post_cpy = post.clone();
            post_cpy.insert(id);
    
            let res = map(
                tuple((
                    |input| self.nessa_expr_parser_wrapper(input, bi, nary, &post_cpy, cache_bin, cache_nary, cache_post, cache),
                    multispace0,
                    tag(rep)
                )),
                |(e, _, _)| NessaExpr::UnaryOperation(id, Box::new(e))
            )(input);

            cache_post.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            });

            return res;
        }
    }
    
    fn binary_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = cache_bin.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            };
       
        } else{
            let mut bi_cpy = bi.clone();
            bi_cpy.insert(id);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_cpy, nary, post, cache_bin, cache_nary, cache_post, cache)?;
    
            let res = map(
                tuple((
                    multispace0,
                    tag(rep),
                    multispace0,
                    |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, cache)
                )),
                |(_, _, _, b)| NessaExpr::BinaryOperation(id, Box::new(a.clone()), Box::new(b))
            )(input);

            cache_bin.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            });

            return res;
        }
    }
    
    fn nary_operation_parser<'a>(&self, input: &'a str, id: usize, open: &str, close: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = cache_nary.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            };
       
        } else{
            let mut nary_cpy = nary.clone();
            nary_cpy.insert(id);

            // Comparison operators
            let mut bi_no_cmp = bi.clone();
            bi_no_cmp.insert(4);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_no_cmp, &nary_cpy, post, cache_bin, cache_nary, cache_post, cache)?;
    
            let res = map(
                tuple((
                    multispace0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                multispace0,
                                separated_list1(
                                    tuple((multispace0, tag(","), multispace0)), 
                                    |input| self.type_parser(input)
                                ),
                                multispace0,
                                tag(">"),
                                multispace0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    tag(open),
                    multispace0,
                    separated_list0(
                        tuple((multispace0, tag(","), multispace0)),
                        |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, cache)
                    ),
                    multispace0,
                    tag(close)
                )),
                |(_, t, _, _, b, _, _)| NessaExpr::NaryOperation(id, t.unwrap_or_default(), Box::new(a.clone()), b)
            )(input);

            cache_nary.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            });

            return res;
        }
    }
    
    fn get_bi_bitset(&self) -> BitSet {
        return BitSet::with_capacity(self.binary_ops.len());
    }
    
    fn get_n_bitset(&self) -> BitSet {
        return BitSet::with_capacity(self.nary_ops.len());
    }
    
    fn get_unary_bitset(&self) -> BitSet {
        return BitSet::with_capacity(self.unary_ops.len());
    }

    fn operation_parser<'a>(&self, input: &'a str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = cache.get(&(input.len(), bi.clone(), nary.clone(), post.clone())) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            };
       
        } else{
            for o in self.sorted_ops.iter().rev() {
                if let Operator::Unary{id, representation, prefix, ..} = o {
                    let res = if *prefix {
                         self.prefix_operation_parser(input, *id, representation, bi, nary, post, cache_bin, cache_nary, cache_post, cache)

                    } else{
                        if !post.contains(*id) {
                            self.postfix_operation_parser(input, *id, representation, bi, nary, post, cache_bin, cache_nary, cache_post, cache)
                        
                        } else {
                            continue;
                        }
                    };
    
                    if res.is_ok() {
                        cache.insert((input.len(), bi.clone(), nary.clone(), post.clone()), match &res {
                            Ok(a) => Ok(a.clone()),
                            Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
                        });

                        return res;
                    }
                    
                } else if let Operator::Binary{id, representation, ..} = o {
                    if !bi.contains(*id) {
                        let res = self.binary_operation_parser(input, *id, representation, bi, nary, post, cache_bin, cache_nary, cache_post, cache);
    
                        if res.is_ok() {
                            cache.insert((input.len(), bi.clone(), nary.clone(), post.clone()), match &res {
                                Ok(a) => Ok(a.clone()),
                                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
                            });

                            return res;
                        }   
                    }
    
                } else if let Operator::Nary{id, open_rep, close_rep, ..} = o {
                    if !nary.contains(*id) {
                        let res = self.nary_operation_parser(input, *id, open_rep, close_rep, bi, nary, post, cache_bin, cache_nary, cache_post, cache);
    
                        if res.is_ok() {
                            cache.insert((input.len(), bi.clone(), nary.clone(), post.clone()), match &res {
                                Ok(a) => Ok(a.clone()),
                                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
                            });

                            return res;
                        }   
                    }
    
                } else{
                    unreachable!();
                }
            }

            cache.insert((input.len(), bi.clone(), nary.clone(), post.clone()), Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt))));
    
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
        }
    }
    
    fn variable_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("let"),
                multispace0,
                |input| self.identifier_parser(input),
                multispace0,
                opt(
                    tuple((
                        tag(":"),
                        multispace0,
                        |input| self.type_parser(input),
                        multispace0
                    ))
                ),
                tag("="),
                multispace0,
                |input| self.nessa_expr_parser(input),
                multispace0,
                tag(";")
            )),
            |(_, _, n, _, t, _, _, e, _, _)| NessaExpr::VariableDefinition(n, t.unwrap_or(("", "", Type::Wildcard, "")).2, Box::new(e))
        )(input);
    }
    
    fn variable_assignment_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.identifier_parser(input),
                multispace0,
                tag("="),
                multispace0,
                |input| self.nessa_expr_parser(input),
                multispace0,
                tag(";")
            )),
            |(n, _, _, _, e, _, _)| NessaExpr::VariableAssignment(n, Box::new(e))
        )(input);
    }
    
    fn return_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("return"),
                multispace0,
                |input| self.nessa_expr_parser(input),
                multispace0,
                tag(";")
            )),
            |(_, _, e, _, _)| NessaExpr::Return(Box::new(e))
        )(input);
    }
    
    fn if_header_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("if"),
                multispace1,
                |input| self.nessa_expr_parser(input)
            )),
            |(_, _, e)| e
        )(input);
    }
    
    fn else_if_header_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("else"),
                multispace1,
                tag("if"),
                multispace1,
                |input| self.nessa_expr_parser(input)
            )),
            |(_, _, _, _, e)| e
        )(input);
    }
    
    fn if_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.if_header_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
                separated_list0(
                    multispace0,
                    map(
                        tuple((
                            multispace0,
                            |input| self.else_if_header_parser(input),
                            multispace0,
                            |input| self.code_block_parser(input)
                        )),
                        |(_, eih, _, eib)| (eih, eib)
                    )
                ),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("else"),
                            multispace0,
                            |input| self.code_block_parser(input)
                        )),
                        |(_, _, _, e)| e   
                    )
                )
            )),
            |(ih, _, ib, ei, e)| NessaExpr::If(Box::new(ih), ib, ei, e)
        )(input);
    }
    
    fn for_header_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (String, NessaExpr)> {
        return map(
            tuple((
                tag("for"),
                multispace1,
                |input| self.identifier_parser(input),
                multispace1,
                tag("in"),
                multispace1,
                |input| self.nessa_expr_parser(input)
            )),
            |(_, _, n, _, _, _, e)| (n, e)
        )(input);
    }
    
    fn for_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.for_header_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((n, c), _, b)| NessaExpr::For(n, Box::new(c), b)
        )(input);
    }

    fn function_header_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (String, Option<Vec<String>>, Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("fn"),
                multispace1,
                |input| self.identifier_parser(input),
                multispace0,
                opt(
                    map(
                        tuple((
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                            multispace0,
                        )),
                        |(_, _, t, _, _, _)| t
                    )
                ),
                multispace0,
                tag("("),
                multispace0,
                separated_list0(
                    tuple((multispace0, tag(","), multispace0)), 
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    ))
                ),
                multispace0,
                tag(")"),
                multispace0,
                tag("->"),
                multispace0,
                |input| self.type_parser(input)
            )),
            |(_, _, n, _, t, _, _, _, a, _, _, _, _, _, r)| (n, t, a, r)
        )(input);
    }

    fn function_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.function_header_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((n, t, mut a, mut r), _, mut b)| {
                let u_t = t.unwrap_or_default();

                a.iter_mut().for_each(|(_, i)| i.compile_templates(&u_t));
                r.compile_templates(&u_t);
                b.iter_mut().for_each(|e| e.compile_types(&u_t));

                NessaExpr::FunctionDefinition(self.get_function_id(n), u_t, a, r, b)
            }
        )(input);
    }

    fn prefix_operator_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("unary"),
                multispace1,
                tag("prefix"),
                multispace1,
                tag("op"),
                multispace1,
                |input| self.string_parser(input),
                multispace0,
                map(
                    delimited(
                        tuple((tag("("), multispace0)),
                        take_while1(|c: char| c.is_digit(10)),
                        tuple((multispace0, tag(")")))
                    ),
                    |s: &str| s.parse::<usize>().unwrap()
                ),
                multispace0,
                tag(";")
            )),
            |(_, _, _, _, _, _, n, _, p, _, _)| NessaExpr::PrefixOperatorDefinition(n, p)
        )(input);
    }

    fn postfix_operator_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("unary"),
                multispace1,
                tag("postfix"),
                multispace1,
                tag("op"),
                multispace1,
                |input| self.string_parser(input),
                multispace0,
                map(
                    delimited(
                        tuple((tag("("), multispace0)),
                        take_while1(|c: char| c.is_digit(10)),
                        tuple((multispace0, tag(")")))
                    ),
                    |s: &str| s.parse::<usize>().unwrap()
                ),
                multispace0,
                tag(";")
            )),
            |(_, _, _, _, _, _, n, _, p, _, _)| NessaExpr::PostfixOperatorDefinition(n, p)
        )(input);
    }

    fn binary_operator_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("binary"),
                multispace1,
                tag("op"),
                multispace1,
                |input| self.string_parser(input),
                multispace0,
                map(
                    delimited(
                        tuple((tag("("), multispace0)),
                        take_while1(|c: char| c.is_digit(10)),
                        tuple((multispace0, tag(")")))
                    ),
                    |s: &str| s.parse::<usize>().unwrap()
                ),
                multispace0,
                tag(";")
            )),
            |(_, _, _, _, n, _, p, _, _)| NessaExpr::BinaryOperatorDefinition(n, p)
        )(input);
    }

    fn nary_operator_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("nary"),
                multispace1,
                tag("op"),
                multispace1,
                tag("from"),
                multispace1,
                |input| self.string_parser(input),
                multispace1,
                tag("to"),
                multispace1,
                |input| self.string_parser(input),
                multispace0,
                map(
                    delimited(
                        tuple((tag("("), multispace0)),
                        take_while1(|c: char| c.is_digit(10)),
                        tuple((multispace0, tag(")")))
                    ),
                    |s: &str| s.parse::<usize>().unwrap()
                ),
                multispace0,
                tag(";")
            )),
            |(_, _, _, _, _, _, f, _, _, _, t, _, p, _, _)| NessaExpr::NaryOperatorDefinition(f, t, p)
        )(input);
    }

    fn operator_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.prefix_operator_definition_parser(input),
            |input| self.postfix_operator_definition_parser(input),
            |input| self.binary_operator_definition_parser(input),
            |input| self.nary_operator_definition_parser(input)
        ))(input)
    }

    fn prefix_operator_parser<'a>(&self, input: &'a str) -> IResult<&'a str, usize> {
        for o in &self.unary_ops {
            if let Operator::Unary{id, representation, prefix, ..} = o {
                if *prefix {
                    let res = map(tag(representation.as_str()), |_| *id)(input);

                    if res.is_ok() {
                        return res;
                    }
                }
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }

    fn postfix_operator_parser<'a>(&self, input: &'a str) -> IResult<&'a str, usize> {
        for o in &self.unary_ops {
            if let Operator::Unary{id, representation, prefix, ..} = o {
                if !*prefix {
                    let res = map(tag(representation.as_str()), |_| *id)(input);

                    if res.is_ok() {
                        return res;
                    }
                }
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }

    fn binary_operator_parser<'a>(&self, input: &'a str) -> IResult<&'a str, usize> {
        for o in &self.binary_ops {
            if let Operator::Binary{id, representation, ..} = o {
                let res = map(tag(representation.as_str()), |_| *id)(input);

                if res.is_ok() {
                    return res;
                }
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }

    fn nary_operator_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, Vec<(String, Type)>)> {
        for o in &self.nary_ops {
            if let Operator::Nary{id, open_rep, close_rep, ..} = o {
                let res = map(
                    tuple((
                        tag(open_rep.as_str()),
                        multispace0,
                        separated_list0(
                            tuple((multispace0, tag(","), multispace0)), 
                            tuple((
                                |input| self.identifier_parser(input),
                                map(
                                    opt(
                                        map(
                                            tuple((
                                                multispace0,
                                                tag(":"),
                                                multispace0,
                                                |input| self.type_parser(input),
                                                multispace0
                                            )),
                                            |(_, _, _, t, _)| t
                                        )
                                    ),
                                    |t| t.unwrap_or(Type::Wildcard)
                                )
                            ))
                        ),
                        multispace0,
                        tag(close_rep.as_str())
                    )),
                    |(_, _, a, _, _)| (*id, a)
                )(input);

                if res.is_ok() {
                    return res;
                }
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }

    fn prefix_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                multispace1,
                |input| self.prefix_operator_parser(input),
                multispace0,
                delimited(
                    tuple((tag("("), multispace0)),
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    tuple((multispace0, tag(")")))
                ),
                multispace0,
                tag("->"),
                multispace0,
                |input| self.type_parser(input)
            )),
            |(_, _, id, _, (n, t), _, _, _, r)| (id, n, t, r)
        )(input);
    }

    fn postfix_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                multispace1,
                delimited(
                    tuple((tag("("), multispace0)),
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    tuple((multispace0, tag(")")))
                ),
                multispace0,
                |input| self.postfix_operator_parser(input),
                multispace0,
                tag("->"),
                multispace0,
                |input| self.type_parser(input)
            )),
            |(_, _, (n, t), _, id, _, _, _, r)| (id, n, t, r)
        )(input);
    }

    fn binary_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, (String, Type), (String, Type), Type)> {
        return map(
            tuple((
                tag("op"),
                multispace1,
                delimited(
                    tuple((tag("("), multispace0)),
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    tuple((multispace0, tag(")")))
                ),
                multispace0,
                |input| self.binary_operator_parser(input),
                multispace0,
                delimited(
                    tuple((tag("("), multispace0)),
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    tuple((multispace0, tag(")")))
                ),
                multispace0,
                tag("->"),
                multispace0,
                |input| self.type_parser(input)
            )),
            |(_, _, a, _, id, _, b, _, _, _, r)| (id, a, b, r)
        )(input);
    }

    fn nary_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, (String, Type), Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("op"),
                multispace1,
                delimited(
                    tuple((tag("("), multispace0)),
                    tuple((
                        |input| self.identifier_parser(input),
                        map(
                            opt(
                                map(
                                    tuple((
                                        multispace0,
                                        tag(":"),
                                        multispace0,
                                        |input| self.type_parser(input),
                                        multispace0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    tuple((multispace0, tag(")")))
                ),
                multispace0,
                |input| self.nary_operator_parser(input),
                multispace0,
                tag("->"),
                multispace0,
                |input| self.type_parser(input)
            )),
            |(_, _, a, _, (id, b), _, _, _, r)| (id, a, b, r)
        )(input);
    }

    fn prefix_operation_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.prefix_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((id, n, t, r), _, b)| NessaExpr::PrefixOperationDefinition(id, n, t, r, b)
        )(input);
    }

    fn postfix_operation_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.postfix_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((id, n, t, r), _, b)| NessaExpr::PostfixOperationDefinition(id, n, t, r, b)
        )(input);
    }

    fn binary_operation_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.binary_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((id, a1, a2, r), _, b)| NessaExpr::BinaryOperationDefinition(id, a1, a2, r, b)
        )(input);
    }

    fn nary_operation_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.nary_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input),
            )),
            |((id, a1, a2, r), _, b)| NessaExpr::NaryOperationDefinition(id, a1, a2, r, b)
        )(input);
    }

    fn operation_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.prefix_operation_definition_parser(input),
            |input| self.postfix_operation_definition_parser(input),
            |input| self.binary_operation_definition_parser(input),
            |input| self.nary_operation_definition_parser(input)
        ))(input);
    }

    fn code_block_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Vec<NessaExpr>> {
        return delimited(
            tuple((tag("{"), multispace0)),
            separated_list0(multispace0, |input| self.nessa_line_parser(input)),
            tuple((multispace0, tag("}")))
        )(input);
    }

    fn class_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("class"),
                multispace1,
                |input| self.identifier_parser(input),
                multispace0,
                opt(
                    map(
                        tuple((
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                            multispace0,
                        )),
                        |(_, _, t, _, _, _)| t
                    )
                ),
                tag("{"),
                multispace0,
                separated_list0(
                    multispace0,
                    map(
                        tuple((
                            |input| self.identifier_parser(input),
                            map(
                                opt(
                                    map(
                                        tuple((
                                            multispace0,
                                            tag(":"),
                                            multispace0,
                                            |input| self.type_parser(input),
                                            multispace0
                                        )),
                                        |(_, _, _, t, _)| t
                                    )
                                ),
                                |t| t.unwrap_or(Type::Wildcard)
                            ),
                            multispace0,
                            tag(";")
                        )),
                        |(a, b, _, _)| (a, b)
                    )
                ),
                multispace0,
                tag("}")
            )),
            |(_, _, n, _, t, _, _, mut f, _, _)| {
                let u_t = t.unwrap_or_default();

                f.iter_mut().for_each(|(_, tp)| tp.compile_templates(&u_t));

                NessaExpr::ClassDefinition(n, u_t, f)
            }
        )(input);
    }

    fn nessa_expr_parser_wrapper<'a>(&self, input: &'a str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_nary: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache_post: &mut HashMap<usize, IResult<&'a str, NessaExpr>>, cache: &mut HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.operation_parser(input, bi, nary, post, cache_bin, cache_nary, cache_post, cache),
            |input| self.literal_parser(input),
            |input| self.variable_parser(input)
        ))(input);
    }

    fn nessa_expr_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return self.nessa_expr_parser_wrapper(
            input, 
            &self.get_bi_bitset(), &self.get_n_bitset(), &self.get_unary_bitset(), 
            &mut HashMap::new(), &mut HashMap::new(), &mut HashMap::new(), &mut HashMap::new()
        );
    }

    fn nessa_line_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.variable_definition_parser(input),
            |input| self.variable_assignment_parser(input),
            |input| self.return_parser(input),
            |input| self.for_parser(input),
            |input| self.if_parser(input),
            |input| terminated(|input| self.nessa_expr_parser(input), tuple((multispace0, tag(";"))))(input)
        ))(input);
    }

    fn nessa_global_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.variable_definition_parser(input),
            |input| self.variable_assignment_parser(input),
            |input| self.return_parser(input),
            |input| self.for_parser(input),
            |input| self.if_parser(input),
            |input| self.function_definition_parser(input),
            |input| self.operator_definition_parser(input),
            |input| self.operation_definition_parser(input),
            |input| self.class_definition_parser(input),
            |input| terminated(|input| self.nessa_expr_parser(input), tuple((multispace0, tag(";"))))(input)
        ))(input);
    }

    pub fn nessa_operators_parser<'a>(&self, mut input: &'a str) -> IResult<&'a str, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.operator_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                let mut chars = input.chars();
                chars.next();

                input = chars.as_str();
            }
        }

        return Ok(("", ops));
    }

    pub fn nessa_function_headers_parser<'a>(&self, mut input: &'a str) -> IResult<&'a str, Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.function_header_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                let mut chars = input.chars();
                chars.next();

                input = chars.as_str();
            }
        }

        return Ok(("", ops));
    }

    pub fn nessa_operations_parser<'a>(&self, mut input: &'a str) -> IResult<&'a str, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.operation_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                let mut chars = input.chars();
                chars.next();

                input = chars.as_str();
            }
        }

        return Ok(("", ops));
    }

    pub fn nessa_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Vec<NessaExpr>> {
        return delimited(
            multispace0,
            separated_list0(multispace0, |input| self.nessa_global_parser(input)),
            tuple((multispace0, eof))
        )(input);
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
    use crate::object::*;
    use crate::number::*;

    #[test]
    fn type_parsing() {
        let ctx = standard_ctx();

        let wildcard_str = "*";
        let empty_str = "()";

        let number_str = "Number";
        let number_ref_str = "&Number";
        let string_mut_str = "&&String";
        let wildcard_mut_str = "&&*";

        let or_str = "Number | &&String";
        let and_str = "(Number, &&String, &Bool)";
        let and_one_str = "(Number)";

        let array_str = "Array<Number>";
        let map_str = "Map<Number, String>";
        let map_refs_str = "&Map<&Number, &&String>";

        let basic_func_str = "Number => String";
        let complex_func_str = "(Number, Array<Bool>) => Map<Number, *>";

        let (_, wildcard) = ctx.type_parser(wildcard_str).unwrap();
        let (_, empty) = ctx.type_parser(empty_str).unwrap();

        assert_eq!(wildcard, Type::Wildcard);
        assert_eq!(empty, Type::Empty);

        let (_, number) = ctx.type_parser(number_str).unwrap();
        let (_, number_ref) = ctx.type_parser(number_ref_str).unwrap();
        let (_, string_mut) = ctx.type_parser(string_mut_str).unwrap();
        let (_, wildcard_mut) = ctx.type_parser(wildcard_mut_str).unwrap();

        assert_eq!(number, Type::Basic(0));
        assert_eq!(number_ref, Type::Ref(Box::new(Type::Basic(0))));
        assert_eq!(string_mut, Type::MutRef(Box::new(Type::Basic(1))));
        assert_eq!(wildcard_mut, Type::MutRef(Box::new(Type::Wildcard)));

        let (_, or) = ctx.type_parser(or_str).unwrap();
        let (_, and) = ctx.type_parser(and_str).unwrap();
        let (_, and_one) = ctx.type_parser(and_one_str).unwrap();

        assert_eq!(or, Type::Or(vec!(Type::Basic(0), Type::MutRef(Box::new(Type::Basic(1))))));
        assert_eq!(and, Type::And(vec!(Type::Basic(0), Type::MutRef(Box::new(Type::Basic(1))), Type::Ref(Box::new(Type::Basic(2))))));
        assert_eq!(and_one, Type::Basic(0));

        let (_, array) = ctx.type_parser(array_str).unwrap();
        let (_, map) = ctx.type_parser(map_str).unwrap();
        let (_, map_refs) = ctx.type_parser(map_refs_str).unwrap();

        assert_eq!(array, Type::Template(3, vec!(Type::Basic(0))));
        assert_eq!(map, Type::Template(4, vec!(Type::Basic(0), Type::Basic(1))));
        assert_eq!(map_refs, Type::Ref(Box::new(Type::Template(4, vec!(Type::Ref(Box::new(Type::Basic(0))), Type::MutRef(Box::new(Type::Basic(1))))))));
        
        let (_, basic_func) = ctx.type_parser(basic_func_str).unwrap();
        let (_, complex_func) = ctx.type_parser(complex_func_str).unwrap();

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

        let number_str = "123";
        let bool_v_str = "true";
        let string_str = "\"test\"";

        let (_, number) = ctx.literal_parser(number_str).unwrap();
        let (_, bool_v) = ctx.literal_parser(bool_v_str).unwrap();
        let (_, string) = ctx.literal_parser(string_str).unwrap();

        assert_eq!(number, NessaExpr::Literal(Object::new(Number::from(123))));
        assert_eq!(bool_v, NessaExpr::Literal(Object::new(true)));
        assert_eq!(string, NessaExpr::Literal(Object::new("test".to_string())));
    }

    #[test]
    fn variable_definition_parsing() {
        let ctx = standard_ctx();

        let def_1_str = "let var: Number = a;";
        let def_2_str = "let foo: Array<Number | &String> = 5;";
        let def_3_str = "let bar = \"test\";";
        let def_4_str = "let foobar = false;";

        let (_, def_1) = ctx.variable_definition_parser(def_1_str).unwrap();
        let (_, def_2) = ctx.variable_definition_parser(def_2_str).unwrap();
        let (_, def_3) = ctx.variable_definition_parser(def_3_str).unwrap();
        let (_, def_4) = ctx.variable_definition_parser(def_4_str).unwrap();

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

        let number_str = "-10";
        let var_str = "-!a";
        let n_var_str = "-5 + a?";
        let n_call_str = "5(-b + !10)";
        let template_func_str = "funct<Number>(5)";

        let (_, number) = ctx.nessa_expr_parser(number_str).unwrap();
        let (_, var) = ctx.nessa_expr_parser(var_str).unwrap();
        let (_, n_var) = ctx.nessa_expr_parser(n_var_str).unwrap();
        let (_, n_call) = ctx.nessa_expr_parser(n_call_str).unwrap();
        let (_, template_func) = ctx.nessa_expr_parser(template_func_str).unwrap();

        assert_eq!(number, NessaExpr::UnaryOperation(0, Box::new(NessaExpr::Literal(Object::new(Number::from(10))))));
        assert_eq!(
            var, 
            NessaExpr::UnaryOperation(0, 
            Box::new(NessaExpr::UnaryOperation(1, Box::new(NessaExpr::NameReference("a".into()))))
        ));
        assert_eq!(n_var, NessaExpr::BinaryOperation(
            0, 
            Box::new(NessaExpr::UnaryOperation(0, Box::new(NessaExpr::Literal(Object::new(Number::from(5)))))),
            Box::new(NessaExpr::UnaryOperation(2, Box::new(NessaExpr::NameReference("a".into())))),
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

        let number_header_str = "fn test(a: Number) -> Number";
        let ref_header_str = "fn test_2(arg: &Number) -> &&Number";
        let two_args_header_str = "fn test_3(arg_1: &Number, arg_2: String | Number) -> Number | String";
        let complex_args_header_str = "fn test_4(a: String | &Number, b: &Array<(Bool, Number)>, c: &&*) -> Map<Number, String>";

        let (_, number_header) = ctx.function_header_parser(number_header_str).unwrap();
        let (_, ref_header) = ctx.function_header_parser(ref_header_str).unwrap();
        let (_, two_args_header) = ctx.function_header_parser(two_args_header_str).unwrap();
        let (_, complex_args_header) = ctx.function_header_parser(complex_args_header_str).unwrap();

        assert_eq!(number_header, ("test".into(), None, vec!(("a".into(), Type::Basic(0))), Type::Basic(0)));
        assert_eq!(ref_header, ("test_2".into(), None, vec!(("arg".into(), Type::Ref(Box::new(Type::Basic(0))))), Type::MutRef(Box::new(Type::Basic(0)))));
        assert_eq!(two_args_header, (
            "test_3".into(), 
            None,
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
        ));
        assert_eq!(complex_args_header, (
            "test_4".into(), 
            None, 
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
        ));
    }

    #[test]
    fn function_definition_and_flow_control_parsing() {
        let ctx = standard_ctx();

        let test_1_str = "fn inc() -> Number {
            let res = 5;

            for i in arr {
                return 7;
            }

            return res;
        }";

        let test_2_str = "fn inc(arg: &Number) -> Number | String {
            let r: Number = arg + 1;

            if r + 1 {
                return \"a\";
            
            } else if arg + 2 {
                r = r + 1;    
            
            } else {
                return 5;    
            }

            return r;
        }";

        let test_3_str = "fn inc<K, V>(key: 'K, value: 'V) -> Map<'K, 'V> {
            let a: 'V | 'K = value + key;
            return a;
        }";

        let (_, test_1) = ctx.function_definition_parser(test_1_str).unwrap();
        let (_, test_2) = ctx.function_definition_parser(test_2_str).unwrap();
        let (_, test_3) = ctx.function_definition_parser(test_3_str).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::FunctionDefinition(
                0,
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
                0,
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
                0,
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
    fn operator_definition_parsing() {
        let ctx = standard_ctx();

        let prefix_str = "unary prefix op \"~\" (200);";
        let postfix_str = "unary postfix op \"&\" (300);";
        let binary_str = "binary op \"$\" (400);";
        let nary_str = "nary op from \"`\" to \"´\" (500);";

        let (_, prefix) = ctx.operator_definition_parser(prefix_str).unwrap();
        let (_, postfix) = ctx.operator_definition_parser(postfix_str).unwrap();
        let (_, binary) = ctx.operator_definition_parser(binary_str).unwrap();
        let (_, nary) = ctx.operator_definition_parser(nary_str).unwrap();

        assert_eq!(prefix, NessaExpr::PrefixOperatorDefinition("~".into(), 200));
        assert_eq!(postfix, NessaExpr::PostfixOperatorDefinition("&".into(), 300));
        assert_eq!(binary, NessaExpr::BinaryOperatorDefinition("$".into(), 400));
        assert_eq!(nary, NessaExpr::NaryOperatorDefinition("`".into(), "´".into(), 500));
    }

    #[test]
    fn operation_definition_and_flow_control_parsing() {
        let ctx = standard_ctx();

        let test_1_str = "op !(arg: Bool) -> Bool {
            if arg {
                return false;
            }

            return true;
        }";

        let test_2_str = "op (arg: Bool)? -> Number | Bool {
            if arg {
                return 5;
            }

            for i in arr {
                return i;
            }

            return true;
        }";

        let test_3_str = "op (a: Bool) + (b: Bool) -> Number {
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
        }";

        let test_4_str = "op (a: Number)[b: Number, c: Number] -> Number {
            return a + b * c;
        }";

        let (_, test_1) = ctx.operation_definition_parser(test_1_str).unwrap();
        let (_, test_2) = ctx.operation_definition_parser(test_2_str).unwrap();
        let (_, test_3) = ctx.operation_definition_parser(test_3_str).unwrap();
        let (_, test_4) = ctx.operation_definition_parser(test_4_str).unwrap();

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
                            2,
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

        let dice_roll_str = "class DiceRoll {
            faces: Number;
            rolls: Number;
        }";

        let sync_lists_str = "class SyncLists<K, V> {
            from: Array<'K>;
            to: Array<'V>;
        }";

        let (_, dice_roll) = ctx.class_definition_parser(dice_roll_str).unwrap();
        let (_, sync_lists) = ctx.class_definition_parser(sync_lists_str).unwrap();

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