use std::collections::HashMap;
use std::cell::RefCell;

use nom::{
    IResult,
    combinator::{map, map_res, opt, eof, value},
    bytes::complete::{take_while, take_while1, tag, escaped_transform},
    sequence::{tuple, delimited, terminated},
    branch::alt,
    character::complete::{multispace0, multispace1, satisfy},
    multi::{separated_list0, separated_list1}
};

use bit_set::BitSet;

use crate::operations::Operator;
use crate::object::Object;
use crate::number::Number;
use crate::types::*;
use crate::operations::*;
use crate::context::NessaContext;
use crate::patterns::*;

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
    CompiledVariableAssignment(usize, String, Type, Box<NessaExpr>),
    FunctionCall(usize, Vec<Type>, Vec<NessaExpr>),
    CompiledFor(usize, usize, String, Box<NessaExpr>, Vec<NessaExpr>),

    CompiledLambda(Option<usize>, Vec<(String, Type)>, Type, Vec<NessaExpr>, usize),
    CompiledFunctionDefinition(usize, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>, usize),
    CompiledPrefixOperationDefinition(usize, Vec<String>, String, Type, Type, Vec<NessaExpr>, usize),
    CompiledPostfixOperationDefinition(usize, Vec<String>, String, Type, Type, Vec<NessaExpr>, usize),
    CompiledBinaryOperationDefinition(usize, Vec<String>, (String, Type), (String, Type), Type, Vec<NessaExpr>, usize),
    CompiledNaryOperationDefinition(usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type, Vec<NessaExpr>, usize),

    // Uncompiled
    Literal(Object),
    Tuple(Vec<NessaExpr>),
    Lambda(Vec<(String, Type)>, Type, Vec<NessaExpr>),
    NameReference(String),

    UnaryOperation(usize, Vec<Type>, Box<NessaExpr>),
    BinaryOperation(usize, Vec<Type>, Box<NessaExpr>, Box<NessaExpr>),
    NaryOperation(usize, Vec<Type>, Box<NessaExpr>, Vec<NessaExpr>),

    VariableDefinition(String, Type, Box<NessaExpr>),
    VariableAssignment(String, Box<NessaExpr>),
    FunctionDefinition(usize, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>),
    PrefixOperatorDefinition(String, usize),
    PostfixOperatorDefinition(String, usize),
    BinaryOperatorDefinition(String, usize),
    NaryOperatorDefinition(String, String, usize),
    ClassDefinition(String, Vec<String>,Vec<(String, Type)>, Vec<Pattern>),

    PrefixOperationDefinition(usize, Vec<String>, String, Type, Type, Vec<NessaExpr>),
    PostfixOperationDefinition(usize, Vec<String>, String, Type, Type, Vec<NessaExpr>),
    BinaryOperationDefinition(usize, Vec<String>, (String, Type), (String, Type), Type, Vec<NessaExpr>),
    NaryOperationDefinition(usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type, Vec<NessaExpr>),

    If(Box<NessaExpr>, Vec<NessaExpr>, Vec<(NessaExpr, Vec<NessaExpr>)>, Option<Vec<NessaExpr>>),
    While(Box<NessaExpr>, Vec<NessaExpr>),
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

            NessaExpr::Tuple(e) => {
                e.iter_mut().for_each(|i| i.compile_types(templates));
            }

            NessaExpr::UnaryOperation(_, _, e) => e.compile_types(templates),
            NessaExpr::BinaryOperation(_, _, a, b) => {
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
            
            NessaExpr::While(c, b) |
            NessaExpr::For(_, c, b) => {
                c.compile_types(templates);
                b.iter_mut().for_each(|i| i.compile_types(templates));
            },

            NessaExpr::Return(e) => e.compile_types(templates),

            _ => {}
        }
    }
}

type ParserCache<'a> = HashMap<usize, IResult<&'a str, NessaExpr>>;
type OperatorCache<'a> = RefCell<HashMap<(usize, BitSet, BitSet, BitSet), IResult<&'a str, NessaExpr>>>;

fn get_from_cache<'a>(cache: &OperatorCache<'a>, input: &'a str, key: (usize, BitSet, BitSet, BitSet)) -> Option<IResult<&'a str, NessaExpr>> {
    if let Some(r) = cache.borrow().get(&key) {
        return Some(match r {
            Ok(a) => Ok(a.clone()),
            Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
        });
    }

    return None;
}

fn store_in_cache<'a>(cache: &OperatorCache<'a>, input: &'a str, key: (usize, BitSet, BitSet, BitSet), value: &IResult<&'a str, NessaExpr>) {
    cache.borrow_mut().insert(key, match value {
        Ok(a) => Ok(a.clone()),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
    });
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
            |(f, _, _, _, t)| Type::Function(None, Box::new(f), Box::new(t))
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
        return delimited(
            tag("\""), 
            alt((
                escaped_transform(
                    satisfy(|i| i != '"' && i != '\\'), 
                    '\\', 
                    alt((
                        value("\n", tag("n")),
                        value("\t", tag("t")),
                        value("\"", tag("\"")),
                        value("\\", tag("\\"))
                    ))
                ),
                value(String::new(), tag(""))
            )),
            tag("\"")
        )(input);
    }

    pub fn parse_literal_type<'a>(&self, c_type: &TypeTemplate, input: &'a str) -> IResult<&'a str, Object> {
        for p in &c_type.patterns {
            let res = map_res(
                |input| p.extract(input),
                |args| Result::<Object, String>::Ok(Object::new(TypeInstance {
                    id: c_type.id,
                    params: vec!(),
                    attributes: c_type.attributes.iter().map(|(n, t)| {
                        if !args.contains_key(n) {
                            return Err(format!("NDL extraction results map does not contain the attribute {}", n));
                        }

                        if let Type::Basic(t_id) = t {  
                            return self.type_templates[*t_id].parser.unwrap()(self, &self.type_templates[*t_id], &args[n][0].into());
                        }

                        if let Type::Template(3, t) = t {  
                            if let &[Type::Basic(t_id)] = &t[..] {
                                return args[n].iter().cloned()
                                    .map(|arg| self.type_templates[t_id].parser.unwrap()(self, &self.type_templates[t_id], &arg.into()))
                                    .collect::<Result<Vec<_>, _>>()
                                    .map(|r| Object::new((Type::Basic(t_id), r)));
                            }

                            unimplemented!();
                        }

                        unimplemented!();

                    }).collect::<Result<Vec<Object>, String>>()?
                }))
            )(input);

            if res.is_ok() {
                return res;
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }

    fn custom_literal_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Object> {
        for c in &self.type_templates {
            let res = self.parse_literal_type(c, input);

            if res.is_ok() {
                return res;
            }
        }

        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
    }
    
    fn literal_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            alt((
                |input| self.custom_literal_parser(input),
                map(|input| self.bool_parser(input), |b| Object::new(b)),
                map(|input| self.number_parser(input), |n| Object::new(n)),
                map(|input| self.string_parser(input), |s| Object::new(s)),
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
    
    fn prefix_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag(rep),
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
                |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache)
            )),
            |(_, _, t, e)| NessaExpr::UnaryOperation(id, t.unwrap_or_default(), Box::new(e))
        )(input);
    }
    
    fn postfix_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
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
                    |input| self.nessa_expr_parser_wrapper(input, bi, nary, &post_cpy, cache_bin, cache_nary, cache_post, op_cache),
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
                    tag(rep)
                )),
                |(e, _, t, _)| NessaExpr::UnaryOperation(id, t.unwrap_or_default(), Box::new(e))
            )(input);

            cache_post.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            });

            return res;
        }
    }
    
    fn binary_operation_parser<'a>(&self, input: &'a str, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = cache_bin.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            };
       
        } else{
            let mut bi_cpy = bi.clone();
            bi_cpy.insert(id);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_cpy, nary, post, cache_bin, cache_nary, cache_post, op_cache)?;
    
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
                    tag(rep),
                    multispace0,
                    |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache)
                )),
                |(_, t, _, _, b)| NessaExpr::BinaryOperation(id, t.unwrap_or_default(), Box::new(a.clone()), Box::new(b))
            )(input);

            cache_bin.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
            });

            return res;
        }
    }
    
    fn nary_operation_parser<'a>(&self, input: &'a str, id: usize, open: &str, close: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
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
            bi_no_cmp.insert(LT_BINOP_ID);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_no_cmp, &nary_cpy, post, cache_bin, cache_nary, cache_post, op_cache)?;
    
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
                        |input| self.nessa_expr_parser_wrapper(input, &mut self.get_bi_bitset(), &mut self.get_n_bitset(), &mut self.get_unary_bitset(), cache_bin, cache_nary, cache_post, op_cache)
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

    fn operation_parser<'a>(&self, input: &'a str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        if let Some(r) = get_from_cache(op_cache, input, (input.len(), bi.clone(), nary.clone(), post.clone())) {
            return r;
       
        } else{
            let mut post_cpy = post.clone();
            let mut bi_cpy = bi.clone();
            let mut nary_cpy = nary.clone();

            for o in self.sorted_ops.iter().rev() {
                if let Operator::Unary{id, representation, prefix, ..} = o {
                    let res = if *prefix {
                         self.prefix_operation_parser(input, *id, representation, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache)

                    } else{
                        if !post.contains(*id) {
                            self.postfix_operation_parser(input, *id, representation, bi, nary, &post_cpy, cache_bin, cache_nary, cache_post, op_cache)
                        
                        } else {
                            continue;
                        }
                    };
    
                    if res.is_ok() {
                        store_in_cache(op_cache, input, (input.len(), bi.clone(), nary.clone(), post.clone()), &res);

                        return res;
                    
                    } else if !*prefix{
                        post_cpy.insert(*id);
                    }
                    
                } else if let Operator::Binary{id, representation, ..} = o {
                    if !bi.contains(*id) {
                        let res = self.binary_operation_parser(input, *id, representation, &bi_cpy, nary, post, cache_bin, cache_nary, cache_post, op_cache);
    
                        if res.is_ok() {
                            store_in_cache(op_cache, input, (input.len(), bi_cpy.clone(), nary.clone(), post.clone()), &res);

                            return res;
                            
                        } else {
                            bi_cpy.insert(*id);
                        }
                    }
    
                } else if let Operator::Nary{id, open_rep, close_rep, ..} = o {
                    if !nary.contains(*id) {
                        let res = self.nary_operation_parser(input, *id, open_rep, close_rep, bi, &nary_cpy, post, cache_bin, cache_nary, cache_post, op_cache);
    
                        if res.is_ok() {
                            store_in_cache(op_cache, input, (input.len(), bi.clone(), nary.clone(), post.clone()), &res);

                            return res;
                        
                        } else {
                            nary_cpy.insert(*id);
                        }
                    }
    
                } else{
                    unreachable!();
                }
            }

            store_in_cache(op_cache, input, (input.len(), bi.clone(), nary.clone(), post.clone()), &Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt))));
    
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)));
        }
    }
    
    fn variable_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
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
                |input| self.nessa_expr_parser(input, op_cache),
                multispace0,
                tag(";")
            )),
            |(_, _, n, _, t, _, _, e, _, _)| NessaExpr::VariableDefinition(n, t.unwrap_or(("", "", Type::Wildcard, "")).2, Box::new(e))
        )(input);
    }
    
    fn variable_assignment_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.identifier_parser(input),
                multispace0,
                tag("="),
                multispace0,
                |input| self.nessa_expr_parser(input, op_cache),
                multispace0,
                tag(";")
            )),
            |(n, _, _, _, e, _, _)| NessaExpr::VariableAssignment(n, Box::new(e))
        )(input);
    }
    
    fn return_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("return"),
                multispace0,
                |input| self.nessa_expr_parser(input, op_cache),
                multispace0,
                tag(";")
            )),
            |(_, _, e, _, _)| NessaExpr::Return(Box::new(e))
        )(input);
    }
    
    fn if_header_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("if"),
                multispace1,
                |input| self.nessa_expr_parser(input, op_cache)
            )),
            |(_, _, e)| e
        )(input);
    }
    
    fn while_header_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("while"),
                multispace1,
                |input| self.nessa_expr_parser(input, op_cache)
            )),
            |(_, _, e)| e
        )(input);
    }
    
    fn else_if_header_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("else"),
                multispace1,
                tag("if"),
                multispace1,
                |input| self.nessa_expr_parser(input, op_cache)
            )),
            |(_, _, _, _, e)| e
        )(input);
    }
    
    fn if_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.if_header_parser(input, op_cache),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
                separated_list0(
                    multispace0,
                    map(
                        tuple((
                            multispace0,
                            |input| self.else_if_header_parser(input, op_cache),
                            multispace0,
                            |input| self.code_block_parser(input, op_cache)
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
                            |input| self.code_block_parser(input, op_cache)
                        )),
                        |(_, _, _, e)| e   
                    )
                )
            )),
            |(ih, _, ib, ei, e)| NessaExpr::If(Box::new(ih), ib, ei, e)
        )(input);
    }
    
    fn for_header_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, (String, NessaExpr)> {
        return map(
            tuple((
                tag("for"),
                multispace1,
                |input| self.identifier_parser(input),
                multispace1,
                tag("in"),
                multispace1,
                |input| self.nessa_expr_parser(input, op_cache)
            )),
            |(_, _, n, _, _, _, e)| (n, e)
        )(input);
    }
    
    fn while_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.while_header_parser(input, op_cache),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |(c, _, b)| NessaExpr::While(Box::new(c), b)
        )(input);
    }
    
    fn for_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.for_header_parser(input, op_cache),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |((n, c), _, b)| NessaExpr::For(n, Box::new(c), b)
        )(input);
    }

    fn function_header_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (String, Option<Vec<String>>, Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("fn"),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                multispace1,
                |input| self.identifier_parser(input),
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
            |(_, t, _, n, _, _, _, a, _, _, _, _, _, r)| (n, t, a, r)
        )(input);
    }

    fn function_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.function_header_parser(input),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
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

    fn prefix_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, Vec<String>, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">")
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
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
            |(_, tm, _, id, _, (n, t), _, _, _, r)| (id, tm.unwrap_or_default(), n, t, r)
        )(input);
    }

    fn postfix_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, Vec<String>, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
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
            |(_, tm, _, (n, t), _, id, _, _, _, r)| (id, tm.unwrap_or_default(), n, t, r)
        )(input);
    }

    fn binary_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, Vec<String>, (String, Type), (String, Type), Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
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
            |(_, tm, _, a, _, id, _, b, _, _, _, r)| (id, tm.unwrap_or_default(), a, b, r)
        )(input);
    }

    fn nary_operation_header_definition_parser<'a>(&self, input: &'a str) -> IResult<&'a str, (usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            multispace0,
                            tag("<"),
                            multispace0,
                            separated_list1(
                                tuple((multispace0, tag(","), multispace0)), 
                                |input| self.identifier_parser(input)
                            ),
                            multispace0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
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
            |(_, tm, _, a, _, (id, b), _, _, _, r)| (id, tm.unwrap_or_default(), a, b, r)
        )(input);
    }

    fn prefix_operation_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.prefix_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |((id, tm, n, mut t, mut r), _, mut b)| {
                t.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::PrefixOperationDefinition(id, tm, n, t, r, b)
            }
        )(input);
    }

    fn postfix_operation_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.postfix_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |((id, tm, n, mut t, mut r), _, mut b)| {
                t.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::PostfixOperationDefinition(id, tm, n, t, r, b)
            }
        )(input);
    }

    fn binary_operation_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.binary_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |((id, tm, mut a1, mut a2, mut r), _, mut b)| {
                a1.1.compile_templates(&tm);
                a2.1.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::BinaryOperationDefinition(id, tm, a1, a2, r, b)
            }
        )(input);
    }

    fn nary_operation_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                |input| self.nary_operation_header_definition_parser(input),
                multispace0,
                |input| self.code_block_parser(input, op_cache),
            )),
            |((id, tm, mut a1, mut a2, mut r), _, mut b)| {
                a1.1.compile_templates(&tm);
                a2.iter_mut().for_each(|(_, i)| i.compile_templates(&tm));
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::NaryOperationDefinition(id, tm, a1, a2, r, b)
            }
        )(input);
    }

    fn operation_definition_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.prefix_operation_definition_parser(input, op_cache),
            |input| self.postfix_operation_definition_parser(input, op_cache),
            |input| self.binary_operation_definition_parser(input, op_cache),
            |input| self.nary_operation_definition_parser(input, op_cache)
        ))(input);
    }

    fn code_block_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, Vec<NessaExpr>> {
        return delimited(
            tuple((tag("{"), multispace0)),
            separated_list0(multispace0, |input| self.nessa_line_parser(input, op_cache)),
            tuple((multispace0, tag("}")))
        )(input);
    }

    fn inline_class_syntax_parser<'a>(&self, input: &'a str) -> IResult<&'a str, Pattern> {
        return map(
            tuple((
                tag("syntax"),
                multispace1,
                tag("from"),
                multispace1,
                |input| parse_ndl_pattern(input, true, true),
                multispace0,
                tag(";")
            )),
            |(_, _, _, _, p, _, _)| p
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
                    |input| self.inline_class_syntax_parser(input)
                ),
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
            |(_, _, n, _, t, _, _, p, _, mut f, _, _)| {
                let u_t = t.unwrap_or_default();

                f.iter_mut().for_each(|(_, tp)| tp.compile_templates(&u_t));

                NessaExpr::ClassDefinition(n, u_t, f, p)
            }
        )(input);
    }

    fn tuple_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
                tag("("),
                multispace0,
                separated_list0(
                    tuple((multispace0, tag(","), multispace0)),
                    |input| self.nessa_expr_parser(input, &RefCell::default())
                ),
                multispace0,
                tag(")")
            )),
            |(_, _, mut e, _, _)| {
                if e.len() == 1 {
                    return e.pop().unwrap();

                } else {
                    return NessaExpr::Tuple(e);
                }
            }
        )(input);
    }

    fn lambda_parser<'a>(&self, input: &'a str) -> IResult<&'a str, NessaExpr> {
        return map(
            tuple((
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
                opt(
                    map(
                        tuple((
                            tag("->"),
                            multispace0,
                            |input| self.type_parser(input),
                            multispace0,
                        )),
                        |(_, _, t, _)| t
                    ),
                ),
                alt((
                    |input| self.code_block_parser(input, &RefCell::default()),
                    map(
                        |input| self.nessa_expr_parser(input, &RefCell::default()),
                        |e| vec!(NessaExpr::Return(Box::new(e))) // Implicit return
                    )
                ))
            )),
            |(_, _, a, _, _, _, r, b)| NessaExpr::Lambda(a, r.unwrap_or(Type::Wildcard), b)
        )(input);
    }

    fn nessa_expr_parser_wrapper<'a>(&self, input: &'a str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.tuple_parser(input),
            |input| self.lambda_parser(input),
            |input| self.operation_parser(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache),
            |input| self.literal_parser(input),
            |input| self.variable_parser(input)
        ))(input);
    }

    fn nessa_expr_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return self.nessa_expr_parser_wrapper(
            input, 
            &self.get_bi_bitset(), &self.get_n_bitset(), &self.get_unary_bitset(), 
            &mut HashMap::new(), &mut HashMap::new(), &mut HashMap::new(), op_cache
        );
    }

    fn nessa_line_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.variable_definition_parser(input, op_cache),
            |input| self.variable_assignment_parser(input, op_cache),
            |input| self.return_parser(input, op_cache),
            |input| self.while_parser(input, op_cache),
            |input| self.for_parser(input, op_cache),
            |input| self.if_parser(input, op_cache),
            |input| terminated(|input| self.nessa_expr_parser(input, op_cache), tuple((multispace0, tag(";"))))(input)
        ))(input);
    }

    fn nessa_global_parser<'a>(&self, input: &'a str, op_cache: &OperatorCache<'a>) -> IResult<&'a str, NessaExpr> {
        return alt((
            |input| self.variable_definition_parser(input, op_cache),
            |input| self.variable_assignment_parser(input, op_cache),
            |input| self.return_parser(input, op_cache),
            |input| self.while_parser(input, op_cache),
            |input| self.for_parser(input, op_cache),
            |input| self.if_parser(input, op_cache),
            |input| self.function_definition_parser(input, op_cache),
            |input| self.operator_definition_parser(input),
            |input| self.operation_definition_parser(input, op_cache),
            |input| self.class_definition_parser(input),
            |input| terminated(|input| self.nessa_expr_parser(input, op_cache), tuple((multispace0, tag(";"))))(input)
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
            if let Ok((i, o)) = self.operation_definition_parser(input, &RefCell::default()) {
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

    pub fn nessa_class_parser<'a>(&self, mut input: &'a str) -> IResult<&'a str, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.class_definition_parser(input) {
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
            separated_list0(multispace0, |input| self.nessa_global_parser(input, &RefCell::default())),
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
        let map_str = "Map<(Number), String>";
        let map_refs_str = "&Map<&Number, &&String>";

        let basic_func_str = "Number => (String)";
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

        assert_eq!(basic_func, Type::Function(None, Box::new(Type::Basic(0)), Box::new(Type::Basic(1))));
        assert_eq!(complex_func, Type::Function(
            None,
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
        let mut ctx = standard_ctx();

        let number_str = "123";
        let bool_v_str = "true";
        let string_str = "\"test\"";
        let escaped_string_str = "\"test\\ntest2\\ttest3\\\"\\\\\"";

        let (_, number) = ctx.literal_parser(number_str).unwrap();
        let (_, bool_v) = ctx.literal_parser(bool_v_str).unwrap();
        let (_, string) = ctx.literal_parser(string_str).unwrap();
        let (_, escaped_string) = ctx.literal_parser(escaped_string_str).unwrap();

        assert_eq!(number, NessaExpr::Literal(Object::new(Number::from(123))));
        assert_eq!(bool_v, NessaExpr::Literal(Object::new(true)));
        assert_eq!(string, NessaExpr::Literal(Object::new("test".to_string())));
        assert_eq!(escaped_string, NessaExpr::Literal(Object::new("test\ntest2\ttest3\"\\".to_string())));

        ctx.define_type("Dice".into(), vec!(), vec!(
            ("rolls".into(), Type::Basic(0)),
            ("sides".into(), Type::Basic(0))
        ), vec!(
            Pattern::And(vec!(
                Pattern::Arg(Box::new(Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None)), "rolls".into()),
                Pattern::Str("D".into()),
                Pattern::Arg(Box::new(Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None)), "sides".into()),
            ))
        ), Some(
            |ctx, c_type, s| {
                if let Ok((_, o)) = ctx.parse_literal_type(c_type, s.as_str()) {
                    return Ok(o);
                }

                return Err(format!("Unable to parse {} from {}", c_type.name, s));
            }
        )).unwrap();
        

        let dice_str = "2D20";

        let (_, dice) = ctx.literal_parser(dice_str).unwrap();

        let id = ctx.type_templates.iter().filter(|i| i.name == "Dice").next().unwrap().id;

        assert_eq!(dice, NessaExpr::Literal(Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Number::from(2)),
                Object::new(Number::from(20))
            )
        })));

        assert_eq!(ctx.type_templates[6].parser.unwrap()(&ctx, &ctx.type_templates[id], &"2D20".into()), Ok(Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Number::from(2)),
                Object::new(Number::from(20))
            )
        })));

        ctx.define_type("InnerDice".into(), vec!(), vec!(
            ("inner_dice".into(), Type::Basic(id))
        ), vec!(
            Pattern::And(vec!(
                Pattern::Str("[".into()),
                Pattern::Arg(
                    Box::new(
                        Pattern::And(vec!(
                            Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None),
                            Pattern::Str("D".into()),
                            Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None),
                        ))
                    ),
                    "inner_dice".into(),
                ),
                Pattern::Str("]".into())
            ))
        ), None).unwrap();

        let inner_dice_str = "[2D20]";
        
        let (_, inner_dice) = ctx.literal_parser(inner_dice_str).unwrap();

        let inner_id = ctx.type_templates.iter().filter(|i| i.name == "InnerDice").next().unwrap().id;

        assert_eq!(inner_dice, NessaExpr::Literal(Object::new(TypeInstance {
            id: inner_id,
            params: vec!(),
            attributes: vec!(
                Object::new(TypeInstance {
                    id: id,
                    params: vec!(),
                    attributes: vec!(
                        Object::new(Number::from(2)),
                        Object::new(Number::from(20))
                    )
                })
            )
        })));
    }

    #[test]
    fn variable_definition_parsing() {
        let ctx = standard_ctx();

        let def_1_str = "let var: Number = a;";
        let def_2_str = "let foo: Array<Number | &String> = 5;";
        let def_3_str = "let bar = \"test\";";
        let def_4_str = "let foobar = false;";
        let def_5_str = "let lambda = (a: Number, b: Number) -> Bool { return a < b; };";
        let def_6_str = "let lambda = (n: Number) -> Number n * 2;";
        let def_7_str = "let lambda = (n: Number) n + 1;";

        let (_, def_1) = ctx.variable_definition_parser(def_1_str, &RefCell::default()).unwrap();
        let (_, def_2) = ctx.variable_definition_parser(def_2_str, &RefCell::default()).unwrap();
        let (_, def_3) = ctx.variable_definition_parser(def_3_str, &RefCell::default()).unwrap();
        let (_, def_4) = ctx.variable_definition_parser(def_4_str, &RefCell::default()).unwrap();
        let (_, def_5) = ctx.variable_definition_parser(def_5_str, &RefCell::default()).unwrap();
        let (_, def_6) = ctx.variable_definition_parser(def_6_str, &RefCell::default()).unwrap();
        let (_, def_7) = ctx.variable_definition_parser(def_7_str, &RefCell::default()).unwrap();

        assert_eq!(def_1, NessaExpr::VariableDefinition("var".into(), Type::Basic(0), Box::new(NessaExpr::NameReference("a".into()))));
        assert_eq!(def_2, NessaExpr::VariableDefinition(
            "foo".into(), 
            Type::Template(3, vec!(Type::Or(vec!(Type::Basic(0), Type::Ref(Box::new(Type::Basic(1))))))), 
            Box::new(NessaExpr::Literal(Object::new(Number::from(5))))
        ));
        assert_eq!(def_3, NessaExpr::VariableDefinition("bar".into(), Type::Wildcard, Box::new(NessaExpr::Literal(Object::new("test".to_string())))));
        assert_eq!(def_4, NessaExpr::VariableDefinition("foobar".into(), Type::Wildcard, Box::new(NessaExpr::Literal(Object::new(false)))));
        assert_eq!(def_5, NessaExpr::VariableDefinition(
            "lambda".into(), 
            Type::Wildcard, 
            Box::new(NessaExpr::Lambda(
                vec!(
                    ("a".into(), Type::Basic(0)),
                    ("b".into(), Type::Basic(0))
                ),
                Type::Basic(2),
                vec!(
                    NessaExpr::Return(Box::new(
                        NessaExpr::BinaryOperation(
                            LT_BINOP_ID, 
                            vec!(),
                            Box::new(NessaExpr::NameReference("a".into())),
                            Box::new(NessaExpr::NameReference("b".into()))
                        )
                    ))
                )
            ))
        ));
        assert_eq!(def_6, NessaExpr::VariableDefinition(
            "lambda".into(), 
            Type::Wildcard, 
            Box::new(NessaExpr::Lambda(
                vec!(
                    ("n".into(), Type::Basic(0))
                ),
                Type::Basic(0),
                vec!(
                    NessaExpr::Return(Box::new(
                        NessaExpr::BinaryOperation(
                            2, 
                            vec!(),
                            Box::new(NessaExpr::NameReference("n".into())),
                            Box::new(NessaExpr::Literal(Object::new(Number::from(2))))
                        )
                    ))
                )
            ))
        ));
        assert_eq!(def_7, NessaExpr::VariableDefinition(
            "lambda".into(), 
            Type::Wildcard, 
            Box::new(NessaExpr::Lambda(
                vec!(
                    ("n".into(), Type::Basic(0))
                ),
                Type::Wildcard,
                vec!(
                    NessaExpr::Return(Box::new(
                        NessaExpr::BinaryOperation(
                            0, 
                            vec!(),
                            Box::new(NessaExpr::NameReference("n".into())),
                            Box::new(NessaExpr::Literal(Object::new(Number::from(1))))
                        )
                    ))
                )
            ))
        ));
    }

    #[test]
    fn operation_parsing() {
        let ctx = standard_ctx();

        let number_str = "-10";
        let var_str = "-!a";
        let n_var_str = "-5 + a?";
        let n_call_str = "5(-b + !10)";
        let template_func_str = "funct<Number>(5)";
        let template_prefix_str = "!<Number>7";
        let template_postfix_str = "false<&String>?";
        let template_binary_str = "\"test\" <String, Bool>+ true";

        let (_, number) = ctx.nessa_expr_parser(number_str, &RefCell::default()).unwrap();
        let (_, var) = ctx.nessa_expr_parser(var_str, &RefCell::default()).unwrap();
        let (_, n_var) = ctx.nessa_expr_parser(n_var_str, &RefCell::default()).unwrap();
        let (_, n_call) = ctx.nessa_expr_parser(n_call_str, &RefCell::default()).unwrap();
        let (_, template_func) = ctx.nessa_expr_parser(template_func_str, &RefCell::default()).unwrap();
        let (_, template_prefix) = ctx.nessa_expr_parser(template_prefix_str, &RefCell::default()).unwrap();
        let (_, template_postfix) = ctx.nessa_expr_parser(template_postfix_str, &RefCell::default()).unwrap();
        let (_, template_binary) = ctx.nessa_expr_parser(template_binary_str, &RefCell::default()).unwrap();

        assert_eq!(number, NessaExpr::UnaryOperation(0, vec!(), Box::new(NessaExpr::Literal(Object::new(Number::from(10))))));
        assert_eq!(
            var, 
            NessaExpr::UnaryOperation(0, vec!(), 
            Box::new(NessaExpr::UnaryOperation(1, vec!(), Box::new(NessaExpr::NameReference("a".into()))))
        ));
        assert_eq!(n_var, NessaExpr::BinaryOperation(
            0, 
            vec!(),
            Box::new(NessaExpr::UnaryOperation(0, vec!(), Box::new(NessaExpr::Literal(Object::new(Number::from(5)))))),
            Box::new(NessaExpr::UnaryOperation(3, vec!(), Box::new(NessaExpr::NameReference("a".into())))),
        ));
        assert_eq!(n_call, NessaExpr::NaryOperation(
            0, 
            vec!(),
            Box::new(NessaExpr::Literal(Object::new(Number::from(5)))),
            vec!(
                NessaExpr::BinaryOperation(
                    0, 
                    vec!(),
                    Box::new(NessaExpr::UnaryOperation(0, vec!(), Box::new(NessaExpr::NameReference("b".into())))),
                    Box::new(NessaExpr::UnaryOperation(1, vec!(), Box::new(NessaExpr::Literal(Object::new(Number::from(10)))))),
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
        assert_eq!(
            template_prefix, 
            NessaExpr::UnaryOperation(
                1, 
                vec!(Type::Basic(0)), 
                Box::new(NessaExpr::Literal(Object::new(Number::from(7))))
            )
        );
        assert_eq!(
            template_postfix, 
            NessaExpr::UnaryOperation(
                3, 
                vec!(Type::Ref(Box::new(Type::Basic(1)))), 
                Box::new(NessaExpr::Literal(Object::new(false)))
            )
        );
        assert_eq!(
            template_binary, 
            NessaExpr::BinaryOperation(
                0, 
                vec!(Type::Basic(1), Type::Basic(2)), 
                Box::new(NessaExpr::Literal(Object::new("test".to_string()))),
                Box::new(NessaExpr::Literal(Object::new(true)))
            )
        );
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

        let test_3_str = "fn<K, V> inc(key: 'K, value: 'V) -> Map<'K, 'V> {
            let a: 'V | 'K = value + key;
            return a;
        }";

        let (_, test_1) = ctx.function_definition_parser(test_1_str, &RefCell::default()).unwrap();
        let (_, test_2) = ctx.function_definition_parser(test_2_str, &RefCell::default()).unwrap();
        let (_, test_3) = ctx.function_definition_parser(test_3_str, &RefCell::default()).unwrap();

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
                            vec!(),
                            Box::new(NessaExpr::NameReference("arg".into())),
                            Box::new(NessaExpr::Literal(Object::new(Number::from(1))))
                        ))
                    ),
                    NessaExpr::If(
                        Box::new(NessaExpr::BinaryOperation(
                            0,
                            vec!(),
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
                                    vec!(),
                                    Box::new(NessaExpr::NameReference("arg".into())),
                                    Box::new(NessaExpr::Literal(Object::new(Number::from(2))))
                                ),
                                vec!(
                                    NessaExpr::VariableAssignment(
                                        "r".into(),
                                        Box::new(NessaExpr::BinaryOperation(
                                            0,
                                            vec!(),
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
                            vec!(),
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

        let test_4_str = "op (a: Number)[b: Number, c: Number] -> (Number, Bool) {
            return (a + b * c, true);
        }";

        let (_, test_1) = ctx.operation_definition_parser(test_1_str, &RefCell::default()).unwrap();
        let (_, test_2) = ctx.operation_definition_parser(test_2_str, &RefCell::default()).unwrap();
        let (_, test_3) = ctx.operation_definition_parser(test_3_str, &RefCell::default()).unwrap();
        let (_, test_4) = ctx.operation_definition_parser(test_4_str, &RefCell::default()).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::PrefixOperationDefinition(
                1,
                vec!(),
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
                3,
                vec!(),
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
                vec!(),
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
                vec!(),
                ("a".into(), Type::Basic(0)),
                vec!(
                    ("b".into(), Type::Basic(0)),
                    ("c".into(), Type::Basic(0))
                ),
                Type::And(vec!(Type::Basic(0), Type::Basic(2))),
                vec!(
                    NessaExpr::Return(Box::new(
                        NessaExpr::Tuple(vec!(
                            NessaExpr::BinaryOperation(
                                0,
                                vec!(),
                                Box::new(NessaExpr::NameReference("a".into())),
                                Box::new(NessaExpr::BinaryOperation(
                                    2,
                                    vec!(),
                                    Box::new(NessaExpr::NameReference("b".into())),
                                    Box::new(NessaExpr::NameReference("c".into()))
                                )
                            )),
                            NessaExpr::Literal(Object::new(true))
                        ))
                    ))
                )
            )
        );

        let test_template_1_str = "op<T> !(arg: 'T) -> 'T {
            if arg {
                return false;
            }

            return true;
        }";

        let test_template_2_str = "op<T> (arg: 'T)? -> Number | 'T {
            if arg {
                return 5;
            }

            for i in arr {
                return i;
            }

            return true;
        }";

        let test_template_3_str = "op<T, G> (a: 'T) + (b: 'T) -> 'G {
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

        let test_template_4_str = "op<T, G> (a: 'T)[b: 'G, c: Number] -> ('T, Array<'G>) {
            return (a + b * c, true);
        }";

        let (_, test_template_1) = ctx.operation_definition_parser(test_template_1_str, &RefCell::default()).unwrap();
        let (_, test_template_2) = ctx.operation_definition_parser(test_template_2_str, &RefCell::default()).unwrap();
        let (_, test_template_3) = ctx.operation_definition_parser(test_template_3_str, &RefCell::default()).unwrap();
        let (_, test_template_4) = ctx.operation_definition_parser(test_template_4_str, &RefCell::default()).unwrap();

        assert_eq!(
            test_template_1,
            NessaExpr::PrefixOperationDefinition(
                1,
                vec!("T".into()),
                "arg".into(),
                Type::TemplateParam(0),
                Type::TemplateParam(0),
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
            test_template_2,
            NessaExpr::PostfixOperationDefinition(
                3,
                vec!("T".into()),
                "arg".into(),
                Type::TemplateParam(0),
                Type::Or(vec!(
                    Type::Basic(0),
                    Type::TemplateParam(0)
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
            test_template_3,
            NessaExpr::BinaryOperationDefinition(
                0,
                vec!("T".into(), "G".into()),
                ("a".into(), Type::TemplateParam(0)),
                ("b".into(), Type::TemplateParam(0)),
                Type::TemplateParam(1),
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
            test_template_4,
            NessaExpr::NaryOperationDefinition(
                1,
                vec!("T".into(), "G".into()),
                ("a".into(), Type::TemplateParam(0)),
                vec!(
                    ("b".into(), Type::TemplateParam(1)),
                    ("c".into(), Type::Basic(0))
                ),
                Type::And(vec!(Type::TemplateParam(0), Type::Template(3, vec!(Type::TemplateParam(1))))),
                vec!(
                    NessaExpr::Return(Box::new(
                        NessaExpr::Tuple(vec!(
                            NessaExpr::BinaryOperation(
                                0,
                                vec!(),
                                Box::new(NessaExpr::NameReference("a".into())),
                                Box::new(NessaExpr::BinaryOperation(
                                    2,
                                    vec!(),
                                    Box::new(NessaExpr::NameReference("b".into())),
                                    Box::new(NessaExpr::NameReference("c".into()))
                                )
                            )),
                            NessaExpr::Literal(Object::new(true))
                        ))
                    ))
                )
            )
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
            syntax from 'test';
            syntax from [[a-h] | d];
            syntax from Arg(['-'], Sign) Arg(1{d}, Int) ['.' Arg(1{d}, Dec)];

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
            ),
            vec!()
        ));

        assert_eq!(sync_lists, NessaExpr::ClassDefinition(
            "SyncLists".into(),
            vec!("K".into(), "V".into()),
            vec!(
                ("from".into(), Type::Template(3, vec!(Type::TemplateParam(0)))),
                ("to".into(), Type::Template(3, vec!(Type::TemplateParam(1))))
            ),
            vec!(
                Pattern::Str("test".into()),
                Pattern::Optional(Box::new(Pattern::Or(vec!(
                    Pattern::Range('a', 'h'),
                    Pattern::Symbol('d')
                )))),
                Pattern::And(vec!(
                    Pattern::Arg(
                        Box::new(Pattern::Optional(
                            Box::new(Pattern::Str("-".into()))
                        )),
                        "Sign".into()
                    ),
                    Pattern::Arg(
                        Box::new(
                            Pattern::Repeat(
                                Box::new(Pattern::Symbol('d')),
                                Some(1),
                                None
                            ),
                        ),
                        "Int".into()
                    ),
                    Pattern::Optional(Box::new(
                        Pattern::And(vec!(
                            Pattern::Str(".".into()),
                            Pattern::Arg(
                                Box::new(Pattern::Repeat(
                                    Box::new(Pattern::Symbol('d')),
                                    Some(1),
                                    None
                                )),
                                "Dec".into()
                            )
                        ))
                    ))
                ))
            )
        ));
    }
}