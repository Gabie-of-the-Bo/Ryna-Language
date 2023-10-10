use std::collections::{ HashMap, HashSet };
use std::cell::RefCell;

use nom::bytes::complete::take_until;
use nom::combinator::cut;
use nom::error::{VerboseError, VerboseErrorKind, context};
use nom::sequence::preceded;
use nom::{
    IResult,
    combinator::{map, map_res, opt, eof, value},
    bytes::complete::{take_while, take_while1, tag, escaped_transform},
    sequence::{tuple, delimited, terminated},
    branch::alt,
    character::complete::{multispace1, satisfy},
    multi::{separated_list1, separated_list0}
};

use nom_locate::LocatedSpan;

use bit_set::BitSet;

use crate::config::ImportMap;
use crate::functions::Function;
use crate::interfaces::{InterfaceConstraint, Interface};
use crate::macros::{NessaMacro, parse_nessa_macro};
use crate::operations::Operator;
use crate::object::Object;
use crate::number::Integer;
use crate::types::*;
use crate::operations::*;
use crate::context::NessaContext;
use crate::patterns::*;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type PResult<'a, T> = IResult<Span<'a>, T, VerboseError<Span<'a>>>;

pub fn verbose_error<'a>(input: Span<'a>, msg: &'static str) -> nom::Err<VerboseError<Span<'a>>> {
    return nom::Err::Error(VerboseError { 
        errors: vec!(
            (input, VerboseErrorKind::Context(msg))
        ) 
    });
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub span: String
}

impl Location {
    pub fn new(line: usize, column: usize, span: String) -> Self {
        return Location { line, column, span };
    }

    pub fn none() -> Self {
        return Self::new(0, 0, "".into());
    }
}

impl PartialEq for Location {
    fn eq(&self, _: &Self) -> bool {
        return true; // Always equal
    }
}

fn normal_comment<'a>(input: Span<'a>) -> PResult<'a, Span<'a>> {
    return delimited(
        tag("//"),
        take_until("\n"),
        tag("\n")
    )(input);
}

fn block_comment<'a>(input: Span<'a>) -> PResult<'a, Span<'a>> {
    return delimited(
        tag("/*"),
        take_until("*/"),
        tag("*/")
    )(input);
}

fn separator<'a>(input: Span<'a>) -> PResult<'a, Span<'a>> {
    return alt((
        multispace1,
        block_comment,
        normal_comment
    ))(input);
}

fn skip_token<'a>(input: Span<'a>) -> PResult<'a, ()> {
    return alt((
        map(block_comment, |_| ()),
        map(normal_comment, |_| ()),
        map(multispace1, |_| ()),
        map(|input| identifier_parser(input), |_| ()),
        map(|input| string_parser(input), |_| ()),
        map(satisfy(|_| true), |_| ()), // skip one char
    ))(input);
}

pub fn empty0<'a>(mut input: Span<'a>) -> PResult<'a, ()> {
    loop {
        match separator(input) {
            Ok((i, _)) => input = i,
            Err(_) => break,
        }
    }
    
    return Ok((input, ()));
}

pub fn empty1<'a>(mut input: Span<'a>) -> PResult<'a, ()> {
    input = separator(input)?.0;

    loop {
        match separator(input) {
            Ok((i, _)) => input = i,
            Err(_) => break,
        }
    }
    
    return Ok((input, ()));
}

// Parser combinator that allows to store the precise location of an element
pub fn located<'a, O, P1: FnMut(Span<'a>) -> PResult<'a, O>>(mut parser: P1) -> impl FnMut(Span<'a>) -> PResult<'a, (Location, O)> {
    return move |input| {
        let (rest, res) = parser(input)?;

        let line = input.location_line() as usize;
        let column = input.get_column();
        let span = &input[..(input.len() - rest.len())];

        return Ok((rest, (Location::new(line, column, span.to_string()), res)));
    };
}

pub fn many_separated0<
    'a, OP, OS, 
    P: FnMut(Span<'a>) -> PResult<'a, OP>, 
    S: FnMut(Span<'a>) -> PResult<'a, OS>
>(mut separator: S, mut parser: P) -> impl FnMut(Span<'a>) -> PResult<'a, Vec<OP>> {
    return move |mut input| {
        let mut res = vec!();
        let mut first = true;
        
        loop {
            if !first {
                let (new_input, _) = separator(input)?;
                input = new_input;    
            }

            match parser(input) {
                Ok((new_input, elem)) => {
                    res.push(elem);
                    input = new_input;
                },
    
                Err(_) => return Ok((input, res))
            }

            first = false;
        }
    } 
}

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum NessaExpr {
    // Compiled
    FunctionName(Location, usize),
    Variable(Location, usize, String, Type),
    CompiledVariableDefinition(Location, usize, String, Type, Box<NessaExpr>),
    CompiledVariableAssignment(Location, usize, String, Type, Box<NessaExpr>),
    FunctionCall(Location, usize, Vec<Type>, Vec<NessaExpr>),
    CompiledFor(Location, usize, usize, String, Box<NessaExpr>, Vec<NessaExpr>),

    CompiledLambda(Location, usize, Vec<(String, Type)>, Type, Vec<NessaExpr>),

    // Macro
    Macro(Location, String, Pattern, NessaMacro),

    // Uncompiled
    Literal(Location, Object),
    Tuple(Location, Vec<NessaExpr>),
    Lambda(Location, Vec<(String, Type)>, Type, Vec<NessaExpr>),
    NameReference(Location, String),

    UnaryOperation(Location, usize, Vec<Type>, Box<NessaExpr>),
    BinaryOperation(Location, usize, Vec<Type>, Box<NessaExpr>, Box<NessaExpr>),
    NaryOperation(Location, usize, Vec<Type>, Box<NessaExpr>, Vec<NessaExpr>),

    VariableDefinition(Location, String, Type, Box<NessaExpr>),
    VariableAssignment(Location, String, Box<NessaExpr>),
    FunctionDefinition(Location, usize, Vec<String>, Vec<(String, Type)>, Type, Vec<NessaExpr>),
    PrefixOperatorDefinition(Location, String, usize),
    PostfixOperatorDefinition(Location, String, usize),
    BinaryOperatorDefinition(Location, String, bool, usize),
    NaryOperatorDefinition(Location, String, String, usize),
    ClassDefinition(Location, String, Vec<String>, Vec<(String, Type)>, Option<Type>, Vec<Pattern>),
    InterfaceDefinition(Location, String, Vec<String>, Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>),
    InterfaceImplementation(Location, Vec<String>, Type, String, Vec<Type>),

    PrefixOperationDefinition(Location, usize, Vec<String>, String, Type, Type, Vec<NessaExpr>),
    PostfixOperationDefinition(Location, usize, Vec<String>, String, Type, Type, Vec<NessaExpr>),
    BinaryOperationDefinition(Location, usize, Vec<String>, (String, Type), (String, Type), Type, Vec<NessaExpr>),
    NaryOperationDefinition(Location, usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type, Vec<NessaExpr>),

    If(Location, Box<NessaExpr>, Vec<NessaExpr>, Vec<(NessaExpr, Vec<NessaExpr>)>, Option<Vec<NessaExpr>>),
    While(Location, Box<NessaExpr>, Vec<NessaExpr>),
    For(Location, String, Box<NessaExpr>, Vec<NessaExpr>),
    Return(Location, Box<NessaExpr>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportType {
    Interface, Class, Fn, Prefix, Postfix, Binary, Nary, Syntax, Outer
}

pub fn get_op_chain(expr: NessaExpr, id: usize) -> (Vec<NessaExpr>, Vec<Vec<Type>>) {
    return match expr {
        NessaExpr::BinaryOperation(_, id2, t, a, b) if id == id2 => {
            let (mut chain_a, mut t_a) = get_op_chain(*a, id);
            let (chain_b, t_b) = get_op_chain(*b, id);

            t_a.push(t);
            t_a.extend(t_b);
            chain_a.extend(chain_b);

            (chain_a, t_a)
        }

        _ => (vec!(expr), vec!())

    }
}

pub fn to_right_assoc(expr: NessaExpr) -> NessaExpr {
    let (l, id) = match &expr {
        NessaExpr::BinaryOperation(l, id, _, _, _) => (l.clone(), *id),
        _ => unreachable!()
    };

    let (exprs, ts) = get_op_chain(expr, id);
    let mut chain = exprs.into_iter();
    let mut res = chain.next().unwrap();
    
    if chain.len() > 0 {
        for (e, t) in chain.zip(ts) {
            res = NessaExpr::BinaryOperation(
                l.clone(), id, t,
                Box::new(res), Box::new(e)
            );    
        }
    }

    return res
}

pub fn identifier_parser<'a>(input: Span<'a>) -> PResult<'a, String> {
    return map(
        tuple((
            take_while1(|c: char| c == '_' || c.is_alphabetic()),
            take_while(|c: char| c == '_' || c.is_alphanumeric())
        )),
        |(a, b)| format!("{}{}", a, b)
    )(input);
}

fn string_parser<'a>(input: Span<'a>) -> PResult<'a, String> {
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

fn module_header_parser<'a>(input: Span<'a>) -> PResult<'a, (String, String)> {
    return map(
        tuple((
            tag("module"),
            empty1,
            context("Expected module name after 'module'", cut(identifier_parser)),
            empty0,
            context("Expected '[' after module name", cut(tag("["))),
            empty0,
            context(
                "Expected version number after module name",
                cut(separated_list1(
                    tag("."), 
                    take_while1(|c: char| c.is_digit(10))
                ))
            ),
            empty0,
            context("Expected ']' after version number", cut(tag("]")))
        )),
        |(_, _, n, _, _, _, v, _, _)| (n, v.into_iter().map(|i| i.to_string()).collect::<Vec<_>>().join("."))
    )(input)
}

fn module_import_parser<'a>(input: Span<'a>) -> PResult<'a, (String, ImportType, HashSet<String>)> {
    return map(
        tuple((
            tag("import"),
            empty1,
            context(
                "Expected import type after 'import' keyword",
                cut(alt((
                    value(ImportType::Interface, tag("interface")),
                    value(ImportType::Class, tag("class")),
                    value(ImportType::Fn, tag("fn")),
                    value(ImportType::Syntax, tag("syntax")),
                    value(ImportType::Prefix, tuple((tag("prefix"), empty1, tag("op")))),
                    value(ImportType::Postfix, tuple((tag("postfix"), empty1, tag("op")))),
                    value(ImportType::Binary, tuple((tag("binary"), empty1, tag("op")))),
                    value(ImportType::Nary, tuple((tag("nary"), empty1, tag("op")))),
                )))
            ),
            empty1,
            context(
                "Expected a String, an identifier or a brace-enclosed list of identifiers after import type",
                cut(alt((
                    map(
                        tuple((
                            alt((
                                string_parser,
                                identifier_parser
                            )),
                            empty1
                        )),
                        |(s, _)| vec!(s)
                    ),
                    map(
                        tuple((
                            tag("{"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)),
                                alt((
                                    string_parser,
                                    identifier_parser
                                ))
                            ),
                            empty0,
                            tag("}"),
                            empty0
                        )),
                        |(_, _, v, _, _, _)| v
                    )
                )))
            ),
            context("Expected 'from' after import type", cut(tag("from"))),
            empty1,
            context("Expected identifier after 'from' in import statement", cut(identifier_parser)),
            empty0,
            context("Expected ';' at the end of import statement", cut(tag(";")))
        )),
        |(_, _, t, _, v, _, _, n, _, _)| (n, t, v.into_iter().collect())
    )(input)
}

pub fn nessa_info_parser<'a>(input: Span<'a>) -> PResult<'a, ()> {
    return delimited(
        empty0,
        alt((
            value((), module_import_parser),
            value((), module_header_parser)
        )),
        empty0
    )(input);
}

pub fn nessa_module_header_parser<'a>(mut input: Span<'a>) -> PResult<'a, Vec<(String, String)>> {
    let mut ops = vec!();

    while input.len() > 0 {
        if let Ok((i, o)) = module_header_parser(input) {
            input = i;
            ops.push(o);
        
        } else {
            input = skip_token(input)?.0;
        }
    }

    return Ok(("".into(), ops));
}

pub fn nessa_module_imports_parser<'a>(mut input: Span<'a>) -> PResult<'a, ImportMap> {
    let mut ops: HashMap<String, HashMap<ImportType, HashSet<String>>> = HashMap::new();

    while input.len() > 0 {
        if let Ok((i, (n, t, v))) = module_import_parser(input) {
            input = i;
            ops.entry(n).or_default().entry(t).or_default().extend(v);
        
        } else {
            input = skip_token(input)?.0;
        }
    }

    return Ok(("".into(), ops));
}

impl NessaExpr {
    pub fn compile_types(&mut self, templates: &Vec<String>) {
        match self {
            NessaExpr::VariableDefinition(_, _, t, e) => {
                t.compile_templates(templates);
                e.compile_types(templates);
            }

            NessaExpr::Tuple(_, e) => {
                e.iter_mut().for_each(|i| i.compile_types(templates));
            }

            NessaExpr::UnaryOperation(_, _, t, e) => {
                t.iter_mut().for_each(|i| i.compile_templates(templates));
                e.compile_types(templates);
            },

            NessaExpr::BinaryOperation(_, _, t, a, b) => {
                t.iter_mut().for_each(|i| i.compile_templates(templates));
                a.compile_types(templates);
                b.compile_types(templates);
            },
            NessaExpr::NaryOperation(_, _, t, a, b) => {
                t.iter_mut().for_each(|i| i.compile_templates(templates));
                a.compile_types(templates);
                b.iter_mut().for_each(|i| i.compile_types(templates));
            },

            NessaExpr::If(_, h, ib, ei, eb) => {
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
            
            NessaExpr::While(_, c, b) |
            NessaExpr::For(_, _, c, b) => {
                c.compile_types(templates);
                b.iter_mut().for_each(|i| i.compile_types(templates));
            },

            NessaExpr::Return(_, e) => e.compile_types(templates),

            _ => {}
        }
    }
}

type ParserCache<'a> = HashMap<usize, PResult<'a, NessaExpr>>;
type OperatorCache<'a> = RefCell<HashMap<(usize, BitSet, BitSet, BitSet), PResult<'a, NessaExpr>>>;

fn get_from_cache<'a>(cache: &OperatorCache<'a>, input: Span<'a>, key: (usize, BitSet, BitSet, BitSet)) -> Option<PResult<'a, NessaExpr>> {
    if let Some(r) = cache.borrow().get(&key) {
        return Some(match r {
            Ok(a) => Ok(a.clone()),
            Err(_) => Err(verbose_error(input, "Unable to parse"))
        });
    }

    return None;
}

fn store_in_cache<'a>(cache: &OperatorCache<'a>, input: Span<'a>, key: (usize, BitSet, BitSet, BitSet), value: &PResult<'a, NessaExpr>) {
    cache.borrow_mut().insert(key, match value {
        Ok(a) => Ok(a.clone()),
        Err(_) => Err(verbose_error(input, "Unable to parse"))
    });
}

impl NessaContext {
    /*
        ╒═══════════════════╕
        │ Auxiliary methods │
        ╘═══════════════════╛
    */
    
    pub fn get_type_id(&self, name: String) -> Result<usize, String> {
        return self.cache.class_id.get(name, |name| {
            self.type_templates.iter().filter(|t| t.name == name).next().map(|i| i.id).ok_or(format!("No type with name {}", name))
        });
    }
    
    pub fn get_interface_id(&self, name: String) -> Result<usize, String> {
        return self.cache.interface_id.get(name, |name| {
            self.interfaces.iter().filter(|t| t.name == name).next().map(|i| i.id).ok_or(format!("No interface with name {}", name))
        });
    }
    
    pub fn get_function_id(&self, name: String) -> Result<usize, String> {
        return self.cache.function_id.get(name, |name| {
            self.functions.iter().filter(|t| t.name == name).next().map(|i| i.id).ok_or(format!("No function with name {}", name))
        });
    }
    
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        return match self.get_function_id(name.to_string()) {
            Ok(id) => return Some(&self.functions[id]),
            Err(_) => None,
        };
    }
    
    pub fn get_interface(&self, name: &str) -> Option<&Interface> {
        return match self.get_interface_id(name.to_string()) {
            Ok(id) => return Some(&self.interfaces[id]),
            Err(_) => None,
        };
    }
    
    pub fn get_type_template(&self, name: &str) -> Option<&TypeTemplate> {
        return match self.get_type_id(name.to_string()) {
            Ok(id) => return Some(&self.type_templates[id]),
            Err(_) => None,
        };
    }

    /*
        ╒═════════════════╕
        │ Type subparsers │
        ╘═════════════════╛
    */

    fn wildcard_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(tag("*"), |_| Type::Wildcard)(input);
    }

    fn empty_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(tag("()"), |_| Type::Empty)(input);
    }

    fn self_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(tag("Self"), |_| Type::SelfType)(input);
    }

    fn basic_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map_res(identifier_parser, |n| Result::<_, String>::Ok(Type::Basic(self.get_type_id(n)?)))(input);
    }

    fn interface_parser<'a>(&self, input: Span<'a>) -> PResult<'a, usize> {
        return map_res(identifier_parser, |n| Result::<_, String>::Ok(self.get_interface_id(n)?))(input);
    }

    fn template_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(
            tuple((
                tag("'"),
                context("Invalid template identifier", cut(identifier_parser)),
                opt(tuple((
                    empty0,
                    tag("["),
                    empty0,
                    separated_list1(
                        tuple((empty0, tag(","), empty0)), 
                        tuple((
                            context("Invalid interface name", cut(|input| self.interface_parser(input))),
                            opt(delimited(
                                tuple((tag("<"), empty0)),
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    |input| self.type_parser(input)
                                ),
                                tuple((empty0, tag(">"))),
                            ))
                        ))
                    ),
                    empty0,
                    tag("]"),
                )))
            )), 
            |(_, n, w)| Type::TemplateParamStr(n, w.map(|(_, _, _, v, _, _)| {
                v.into_iter().map(|(id, t)| InterfaceConstraint::new(id, t.unwrap_or_default())).collect()
            }).unwrap_or_default())
        )(input);
    }

    fn constant_reference_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(
            tuple((
                tag("&"),
                context("Invalid constant refence type", cut(|input| self.type_parser(input)))
            )), 
            |(_, t)| Type::Ref(Box::new(t))
        )(input);
    }

    fn mutable_reference_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(
            tuple((
                tag("&&"),
                context("Invalid mutable reference type", cut(|input| self.type_parser(input)))
            )), 
            |(_, t)| Type::MutRef(Box::new(t))
        )(input);
    }

    fn or_type_parser<'a>(&self, input: Span<'a>, func: bool) -> PResult<'a, Type> {
        return map(
            separated_list1(
                tuple((empty0, tag("|"), empty0)), 
                |input| self.type_parser_wrapper(input, func, false)
            ),
            |t| if t.len() > 1 { Type::Or(t) } else { t[0].clone() }
        )(input);
    }

    fn and_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map(
            delimited(
                tag("("),
                separated_list1(
                    tuple((empty0, tag(","), empty0)), 
                    |input| self.type_parser(input)
                ),
                tag(")")
            ),
            |t| if t.len() > 1 { Type::And(t) } else { t[0].clone() }
        )(input);
    }

    fn parametric_type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return map_res(
            tuple((
                identifier_parser,
                empty0,
                tag("<"),
                empty0,
                separated_list1(
                    tuple((empty0, tag(","), empty0)), 
                    |input| self.type_parser(input)
                ),
                empty0,
                context("Expected '>' at the end of parametric type", cut(tag(">")))
            )),
            |(n, _, _, _, t, _, _)| Result::<_, String>::Ok(Type::Template(self.get_type_id(n)?, t))
        )(input);
    }

    fn function_type_parser<'a>(&self, input: Span<'a>, or: bool) -> PResult<'a, Type> {
        return map(
            tuple((
                |input| self.type_parser_wrapper(input, false, or),
                empty0,
                tag("=>"),
                empty0,
                context("Invalid return on function type", cut(|input| self.type_parser_wrapper(input, false, or)))
            )),
            |(f, _, _, _, t)| Type::Function(Box::new(f), Box::new(t))
        )(input);
    }

    fn type_parser_wrapper<'a>(&self, input: Span<'a>, func: bool, or: bool) -> PResult<'a, Type> {
        return match (func, or) {
            (true, true) => alt((
                |input| self.function_type_parser(input, or),
                |input| self.empty_type_parser(input),
                |input| self.self_type_parser(input),
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
                |input| self.self_type_parser(input),
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
                |input| self.self_type_parser(input),
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
                |input| self.self_type_parser(input),
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

    pub fn type_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Type> {
        return self.type_parser_wrapper(input, true, true);
    }

    /*
        ╒═════════════════╕
        │ Expr subparsers │
        ╘═════════════════╛
    */

    fn bool_parser<'a>(&self, input: Span<'a>) -> PResult<'a, bool> {
        return alt((
            map(tag("true"), |_| true),
            map(tag("false"), |_| false),
        ))(input);
    }

    fn integer_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Integer> {
        return map(
            tuple((
                opt(tag("-")),
                take_while1(|c: char| c.is_digit(10))
            )),
            |(s, n)| Integer::from(format!("{}{}", s.unwrap_or(Span::new("")), n).as_str())
        )(input);
    }

    fn float_parser<'a>(&self, input: Span<'a>) -> PResult<'a, f64> {
        return map(
            tuple((
                opt(tag("-")),
                take_while1(|c: char| c.is_digit(10)),
                tuple((
                    tag("."),
                    take_while1(|c: char| c.is_digit(10))
                ))
            )),
            |(s, n, d)| format!("{}{}{}{}", s.unwrap_or(Span::new("")), n, d.0, d.1).parse().unwrap()
        )(input);
    }

    pub fn parse_literal_type<'a>(&self, c_type: &TypeTemplate, input: Span<'a>) -> PResult<'a, Object> {
        for p in &c_type.patterns {
            let res = map_res(
                |input| p.extract(input, self),
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

                        if let Type::Template(ARR_ID, t) = t {  
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

        return Err(verbose_error(input, "Unable to parse"));
    }

    fn custom_literal_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Object> {
        for c in &self.type_templates {
            let res = self.parse_literal_type(c, input);

            if res.is_ok() {
                return res;
            }
        }

        return Err(verbose_error(input, "Unable to parse"));
    }
    
    fn literal_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                alt((
                    |input| self.custom_literal_parser(input),
                    map(|input| self.bool_parser(input), |b| Object::new(b)),
                    map(|input| self.float_parser(input), |n| Object::new(n)),
                    map(|input| self.integer_parser(input), |n| Object::new(n)),
                    map(string_parser, |s| Object::new(s)),
                ))
            ),
            |(l, o)| NessaExpr::Literal(l, o)
        )(input);
    }
    
    fn custom_syntax_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, NessaExpr> {
        for (_, p, m) in &self.macros {
            if let Ok((new_input, args)) = p.extract(input, self) {
                let span = &input[..input.len() - new_input.len()];
                let loc = Location::new(input.location_line() as usize, input.get_column(), span.to_string());

                input = new_input;
                
                match m.expand(&args) {
                    Ok(code) => {
                        let parsed_code = cut(
                            many_separated0(
                                empty0, 
                                |input| self.nessa_line_parser(input, &RefCell::default()))
                        )(Span::new(&code));
                        
                        match parsed_code {
                            Ok((rest, lines)) if rest.trim().len() == 0 => {
                                return Ok((
                                    input, 
                                    NessaExpr::NaryOperation(
                                        loc.clone(), 
                                        CALL_OP, 
                                        vec!(), 
                                        Box::new(NessaExpr::Lambda(loc, vec!(), Type::InferenceMarker, lines)),
                                        vec!()
                                    )
                                ));
                            },

                            Ok(_) |
                            Err(nom::Err::Error(_)) |
                            Err(nom::Err::Failure(_)) => {                                
                                return Err(nom::Err::Failure(VerboseError { errors: vec!((
                                    input, 
                                    VerboseErrorKind::Context("Error while parsing expanded code")
                                )) }));
                            }

                            _ => unreachable!()
                        }
                    },

                    Err(_) => {
                        return Err(verbose_error(input, "Unable to parse"))
                    }
                }
            }
        }

        return Err(verbose_error(input, "Unable to parse"))
    }
    
    fn variable_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(identifier_parser),
            |(l, v)| NessaExpr::NameReference(l, v)
        )(input);
    }
    
    fn prefix_operation_parser<'a>(&self, input: Span<'a>, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag(rep),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    |input| self.type_parser(input)
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache)
                ))
            ),
            |(l, (_, _, t, e))| NessaExpr::UnaryOperation(l, id, t.unwrap_or_default(), Box::new(e))
        )(input);
    }
    
    fn postfix_operation_parser<'a>(&self, input: Span<'a>, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        if let Some(r) = cache_post.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
            };
       
        } else{
            let mut post_cpy = post.clone();
            post_cpy.insert(id);
    
            let res = map(
                located(
                    tuple((
                        |input| self.nessa_expr_parser_wrapper(input, bi, nary, &post_cpy, cache_bin, cache_nary, cache_post, op_cache),
                        empty0,
                        opt(
                            map(
                                tuple((
                                    tag("<"),
                                    empty0,
                                    separated_list1(
                                        tuple((empty0, tag(","), empty0)), 
                                        |input| self.type_parser(input)
                                    ),
                                    empty0,
                                    tag(">"),
                                    empty0,
                                )),
                                |(_, _, t, _, _, _)| t
                            )
                        ),
                        tag(rep)
                    ))
                ),
                |(l, (e, _, t, _))| NessaExpr::UnaryOperation(l, id, t.unwrap_or_default(), Box::new(e))
            )(input);

            cache_post.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
            });

            return res;
        }
    }
    
    fn binary_operation_parser<'a>(&self, input: Span<'a>, id: usize, rep: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        if let Some(r) = cache_bin.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
            };
       
        } else{
            let mut bi_cpy = bi.clone();
            bi_cpy.insert(id);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_cpy, nary, post, cache_bin, cache_nary, cache_post, op_cache)?;
    
            let res = map(
                located(
                    tuple((
                        empty0,
                        opt(
                            map(
                                tuple((
                                    tag("<"),
                                    empty0,
                                    separated_list1(
                                        tuple((empty0, tag(","), empty0)), 
                                        |input| self.type_parser(input)
                                    ),
                                    empty0,
                                    tag(">"),
                                    empty0,
                                )),
                                |(_, _, t, _, _, _)| t
                            )
                        ),
                        tag(rep),
                        empty0,
                        |input| self.nessa_expr_parser_wrapper(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache)
                    ))
                ),
                |(l, (_, t, _, _, b))| {
                    let mut res = NessaExpr::BinaryOperation(l, id, t.unwrap_or_default(), Box::new(a.clone()), Box::new(b));
                    
                    if self.binary_ops[id].is_right_associative() {
                        res = to_right_assoc(res);
                    }

                    res
                }
            )(input);

            cache_bin.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
            });

            return res;
        }
    }
    
    fn nary_operation_parser<'a>(&self, input: Span<'a>, id: usize, open: &str, close: &str, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        if let Some(r) = cache_nary.get(&input.len()) {
            return match r {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
            };
       
        } else{
            let mut nary_cpy = nary.clone();
            nary_cpy.insert(id);

            // Comparison operators
            let mut bi_no_cmp = bi.clone();
            bi_no_cmp.insert(LT_BINOP_ID);
    
            let (input, a) = self.nessa_expr_parser_wrapper(input, &bi_no_cmp, &nary_cpy, post, cache_bin, cache_nary, cache_post, op_cache)?;
    
            let res = map(
                located(
                    tuple((
                        empty0,
                        opt(
                            map(
                                tuple((
                                    tag("<"),
                                    empty0,
                                    separated_list1(
                                        tuple((empty0, tag(","), empty0)), 
                                        |input| self.type_parser(input)
                                    ),
                                    empty0,
                                    tag(">"),
                                    empty0,
                                )),
                                |(_, _, t, _, _, _)| t
                            )
                        ),
                        tag(open),
                        empty0,
                        separated_list0(
                            tuple((empty0, tag(","), empty0)),
                            |input| self.nessa_expr_parser_wrapper(input, &mut self.get_bi_bitset(), &mut self.get_n_bitset(), &mut self.get_unary_bitset(), cache_bin, cache_nary, cache_post, op_cache)
                        ),
                        empty0,
                        tag(close)
                    ))
                ),
                |(l, (_, t, _, _, b, _, _))| NessaExpr::NaryOperation(l, id, t.unwrap_or_default(), Box::new(a.clone()), b)
            )(input);

            cache_nary.insert(input.len(), match &res {
                Ok(a) => Ok(a.clone()),
                Err(_) => Err(verbose_error(input, "Unable to parse"))
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

    fn operation_parser<'a>(&self, input: Span<'a>, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
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

            store_in_cache(op_cache, input, (input.len(), bi.clone(), nary.clone(), post.clone()), &Err(verbose_error(input, "Unable to parse")));
    
            return Err(verbose_error(input, "Unable to parse"));
        }
    }
    
    fn variable_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("let"),
                    empty0,
                    identifier_parser,
                    empty0,
                    opt(
                        tuple((
                            tag(":"),
                            empty0,
                            context("Invalid type on variable definition", cut(|input| self.type_parser(input))),
                            empty0
                        ))
                    ),
                    tag("="),
                    empty0,
                    context("Invalid right handside on variable definition", cut(|input| self.nessa_expr_parser(input, op_cache))),
                    empty0,
                    context("Expected ';' at the end of variable definition", cut(tag(";")))
                ))
            ),
            |(l, (_, _, n, _, t, _, _, e, _, _))| NessaExpr::VariableDefinition(l, n, t.unwrap_or(("".into(), (), Type::InferenceMarker, ())).2, Box::new(e))
        )(input);
    }
    
    fn variable_assignment_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    identifier_parser,
                    empty0,
                    tag("="),
                    empty0,
                    context("Invalid right handside on variable assignment", cut(|input| self.nessa_expr_parser(input, op_cache))),
                    empty0,
                    context("Expected ';' at the end of variable assignment", cut(tag(";")))
                ))
            ),
            |(l, (n, _, _, _, e, _, _))| NessaExpr::VariableAssignment(l, n, Box::new(e))
        )(input);
    }
    
    fn return_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("return"),
                    empty0,
                    opt(
                        terminated(
                            |input| self.nessa_expr_parser(input, op_cache),
                            empty0
                        )
                    ),
                    context("Expected ';' at the end of return statement", cut(tag(";")))
                ))
            ),
            |(l, (_, _, e, _))| NessaExpr::Return(l.clone(), Box::new(e.unwrap_or_else(|| NessaExpr::Literal(l, Object::empty()))))
        )(input);
    }

    fn macro_header_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (String, Pattern)> {
        map(
            tuple((
                tag("syntax"),
                empty1,
                context("Expected identifier after 'syntax' in syntax definition", cut(identifier_parser)),
                empty1,
                context("Expected 'from' after identifier in syntax definition", cut(tag("from"))),
                empty1,
                cut(|input| parse_ndl_pattern(input, true, true)),
                empty0
            )),
            |(_, _,  n, _, _, _, p, _)| (n, p)
        )(input)
    }

    fn if_header_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            tuple((
                tag("if"),
                empty1,
                context("Invalid if condition", cut(|input| self.nessa_expr_parser(input, op_cache)))
            )),
            |(_, _, e)| e
        )(input);
    }
    
    fn while_header_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            tuple((
                tag("while"),
                empty1,
                context("Invalid while condition", cut(|input| self.nessa_expr_parser(input, op_cache)))
            )),
            |(_, _, e)| e
        )(input);
    }
    
    fn else_if_header_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            tuple((
                tag("else"),
                empty1,
                tag("if"),
                empty1,
                context("Invalid else if condition", cut(|input| self.nessa_expr_parser(input, op_cache)))
            )),
            |(_, _, _, _, e)| e
        )(input);
    }
    
    fn macro_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.macro_header_parser(input),
                    empty0,
                    cut(|input| self.macro_body_parser(input)),
                ))
            ),
            |(l, ((n, p), _, m))| NessaExpr::Macro(l, n, p, m)
        )(input);
    }
    
    fn if_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.if_header_parser(input, op_cache),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                    separated_list0(
                        empty0,
                        map(
                            tuple((
                                empty0,
                                |input| self.else_if_header_parser(input, op_cache),
                                empty0,
                                cut(|input| self.code_block_parser(input, op_cache))
                            )),
                            |(_, eih, _, eib)| (eih, eib)
                        )
                    ),
                    opt(
                        map(
                            tuple((
                                empty0,
                                tag("else"),
                                empty0,
                                cut(|input| self.code_block_parser(input, op_cache))
                            )),
                            |(_, _, _, e)| e   
                        )
                    )
                ))
            ),
            |(l, (ih, _, ib, ei, e))| NessaExpr::If(l, Box::new(ih), ib, ei, e)
        )(input);
    }
    
    fn for_header_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, (String, NessaExpr)> {
        return map(
            tuple((
                tag("for"),
                empty1,
                context("Invalid for iterator identifier", cut(identifier_parser)),
                empty1,
                context("Expected 'in' after for iterator identifier", cut(tag("in"))),
                empty1,
                cut(|input| self.nessa_expr_parser(input, op_cache))
            )),
            |(_, _, n, _, _, _, e)| (n, e)
        )(input);
    }
    
    fn while_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.while_header_parser(input, op_cache),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, (c, _, b))| NessaExpr::While(l, Box::new(c), b)
        )(input);
    }
    
    fn for_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.for_header_parser(input, op_cache),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((n, c), _, b))| NessaExpr::For(l, n, Box::new(c), b)
        )(input);
    }

    fn function_header_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (String, Option<Vec<String>>, Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("fn"),
                opt(
                    map(
                        tuple((
                            empty0,
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                empty1,
                context("Invalid function name", cut(identifier_parser)),
                empty0,
                context("Expected '(' after function name in function definition", cut(tag("("))),
                empty0,
                separated_list0(
                    tuple((empty0, tag(","), empty0)), 
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    ))
                ),
                empty0,
                context("Expected ')' after parameters in function definition", cut(tag(")"))),
                opt(
                    preceded(
                        tuple((empty0, tag("->"), empty0)),
                        context("Expected return type after '->' in function definition", cut(|input| self.type_parser(input)))
                    )
                )
            )),
            |(_, t, _, n, _, _, _, a, _, _, r)| (n, t, a, r.unwrap_or(Type::Empty))
        )(input);
    }

    fn function_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.function_header_parser(input),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((n, t, mut a, mut r), _, mut b))| {
                let u_t = t.unwrap_or_default();

                a.iter_mut().for_each(|(_, i)| i.compile_templates(&u_t));
                r.compile_templates(&u_t);
                b.iter_mut().for_each(|e| e.compile_types(&u_t));

                NessaExpr::FunctionDefinition(l, self.get_function_id(n).unwrap(), u_t, a, r, b)
            }
        )(input);
    }

    fn prefix_operator_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("unary"),
                    empty1,
                    tag("prefix"),
                    empty1,
                    context("Expected 'op' after operator type in unary operator definition", cut(tag("op"))),
                    empty1,
                    context("Expected operator representation after 'op' in unary operator definition", cut(string_parser)),
                    empty0,
                    map(
                        context(
                            "Expected precedence after operator representation in operator definition", 
                            cut(delimited(
                                tuple((tag("("), empty0)),
                                take_while1(|c: char| c.is_digit(10)),
                                tuple((empty0, tag(")")))
                            ))
                        ),
                        |s: Span<'a>| s.parse::<usize>().unwrap()
                    ),
                    empty0,
                    context("Expected ';' at the end of operator definition", cut(tag(";")))
                ))
            ),
            |(l, (_, _, _, _, _, _, n, _, p, _, _))| NessaExpr::PrefixOperatorDefinition(l, n, p)
        )(input);
    }

    fn postfix_operator_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("unary"),
                    empty1,
                    tag("postfix"),
                    empty1,
                    context("Expected 'op' after operator type in unary operator definition", cut(tag("op"))),
                    empty1,
                    context("Expected operator representation after 'op' in unary operator definition", cut(string_parser)),
                    empty0,
                    map(
                        context(
                            "Expected precedence after operator representation in operator definition",
                            cut(delimited(
                                tuple((tag("("), empty0)),
                                take_while1(|c: char| c.is_digit(10)),
                                tuple((empty0, tag(")")))
                            ))
                        ),
                        |s: Span<'a>| s.parse::<usize>().unwrap()
                    ),
                    empty0,
                    context("Expected ';' at the end of operator definition", cut(tag(";")))
                ))
            ),
            |(l, (_, _, _, _, _, _, n, _, p, _, _))| NessaExpr::PostfixOperatorDefinition(l, n, p)
        )(input);
    }

    fn binary_operator_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("binary"),
                    opt(
                        tuple((
                            empty1,
                            tag("right")
                        ))
                    ),
                    empty1,
                    context("Expected 'op' after operator type in binary operator definition", cut(tag("op"))),
                    empty1,
                    context("Expected operator representation after 'op' in binary operator definition", cut(string_parser)),
                    empty0,
                    map(
                        context(
                            "Expected precedence after operator representation in operator definition",
                            cut(delimited(
                                tuple((tag("("), empty0)),
                                take_while1(|c: char| c.is_digit(10)),
                                tuple((empty0, tag(")")))
                            ))
                        ),
                        |s: Span<'a>| s.parse::<usize>().unwrap()
                    ),
                    empty0,
                    context("Expected ';' at the end of operator definition", cut(tag(";")))
                ))
            ),
            |(l, (_, f, _, _, _, n, _, p, _, _))| NessaExpr::BinaryOperatorDefinition(l, n, f.is_some(), p)
        )(input);
    }

    fn nary_operator_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("nary"),
                    empty1,
                    context("Expected 'op' after operator type in n-ary operator definition", cut(tag("op"))),
                    empty1,
                    context("Expected 'from' after 'op' in n-ary operator definition", cut(tag("from"))),
                    empty1,
                    context("Expected operator opening after 'from' in n-ary operator definition", cut(string_parser)),
                    empty1,
                    context("Expected 'to' after operator opening in n-ary operator definition", cut(tag("to"))),
                    empty1,
                    context("Expected operator closing after 'to' in n-ary operator definition", cut(string_parser)),
                    empty0,
                    map(
                        context(
                            "Expected precedence after operator representation in operator definition",
                            cut(delimited(
                                tuple((tag("("), empty0)),
                                take_while1(|c: char| c.is_digit(10)),
                                tuple((empty0, tag(")")))
                            ))
                        ),
                        |s: Span<'a>| s.parse::<usize>().unwrap()
                    ),
                    empty0,
                    context("Expected ';' at the end of operator definition", cut(tag(";")))
                ))
            ),
            |(l, (_, _, _, _, _, _, f, _, _, _, t, _, p, _, _))| NessaExpr::NaryOperatorDefinition(l, f, t, p)
        )(input);
    }

    fn operator_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return alt((
            |input| self.prefix_operator_definition_parser(input),
            |input| self.postfix_operator_definition_parser(input),
            |input| self.binary_operator_definition_parser(input),
            |input| self.nary_operator_definition_parser(input)
        ))(input)
    }

    fn prefix_operator_parser<'a>(&self, input: Span<'a>) -> PResult<'a, usize> {
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

        return Err(verbose_error(input, "Unable to parse"));
    }

    fn postfix_operator_parser<'a>(&self, input: Span<'a>) -> PResult<'a, usize> {
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

        return Err(verbose_error(input, "Unable to parse"));
    }

    fn binary_operator_parser<'a>(&self, input: Span<'a>) -> PResult<'a, usize> {
        for o in &self.binary_ops {
            if let Operator::Binary{id, representation, ..} = o {
                let res = map(tag(representation.as_str()), |_| *id)(input);

                if res.is_ok() {
                    return res;
                }
            }
        }

        return Err(verbose_error(input, "Unable to parse"));
    }

    fn nary_operator_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (usize, Vec<(String, Type)>)> {
        for o in &self.nary_ops {
            if let Operator::Nary{id, open_rep, close_rep, ..} = o {
                let res = map(
                    tuple((
                        tag(open_rep.as_str()),
                        empty0,
                        separated_list0(
                            tuple((empty0, tag(","), empty0)), 
                            tuple((
                                identifier_parser,
                                map(
                                    opt(
                                        map(
                                            tuple((
                                                empty0,
                                                tag(":"),
                                                empty0,
                                                cut(|input| self.type_parser(input)),
                                                empty0
                                            )),
                                            |(_, _, _, t, _)| t
                                        )
                                    ),
                                    |t| t.unwrap_or(Type::Wildcard)
                                )
                            ))
                        ),
                        empty0,
                        tag(close_rep.as_str())
                    )),
                    |(_, _, a, _, _)| (*id, a)
                )(input);

                if res.is_ok() {
                    return res;
                }
            }
        }

        return Err(verbose_error(input, "Unable to parse"));
    }

    fn prefix_operation_header_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (usize, Vec<String>, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            empty0,
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">")
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                empty1,
                |input| self.prefix_operator_parser(input),
                empty0,
                delimited(
                    context("Expected '(' in operation definition", cut(tuple((tag("("), empty0)))),
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    context("Expected ')' in operation definition", cut(tuple((empty0, tag(")")))))
                ),
                opt(
                    preceded(
                        tuple((empty0, tag("->"), empty0)),
                        context("Expected return type after '->' in operation definition", cut(|input| self.type_parser(input)))
                    )
                )
            )),
            |(_, tm, _, id, _, (n, t), r)| (id, tm.unwrap_or_default(), n, t, r.unwrap_or(Type::Empty))
        )(input);
    }

    fn postfix_operation_header_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (usize, Vec<String>, String, Type, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            empty0,
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                empty1,
                delimited(
                    context("Expected '(' in operation definition", cut(tuple((tag("("), empty0)))),
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    context("Expected '(' in operation definition", cut(tuple((empty0, tag(")")))))
                ),
                empty0,
                |input| self.postfix_operator_parser(input),
                opt(
                    preceded(
                        tuple((empty0, tag("->"), empty0)),
                        context("Expected return type after '->' in operation definition", cut(|input| self.type_parser(input)))
                    )
                )
            )),
            |(_, tm, _, (n, t), _, id, r)| (id, tm.unwrap_or_default(), n, t, r.unwrap_or(Type::Empty))
        )(input);
    }

    fn binary_operation_header_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (usize, Vec<String>, (String, Type), (String, Type), Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            empty0,
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                empty1,
                delimited(
                    context("Expected '(' in operation definition", cut(tuple((tag("("), empty0)))),
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    context("Expected ')' in operation definition", cut(tuple((empty0, tag(")")))))
                ),
                empty0,
                |input| self.binary_operator_parser(input),
                empty0,
                delimited(
                    context("Expected '(' in operation definition", cut(tuple((tag("("), empty0)))),
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    context("Expected ')' in operation definition", cut(tuple((empty0, tag(")")))))
                ),
                opt(
                    preceded(
                        tuple((empty0, tag("->"), empty0)),
                        context("Expected return type after '->' in operation definition", cut(|input| self.type_parser(input)))
                    )
                )
            )),
            |(_, tm, _, a, _, id, _, b, r)| (id, tm.unwrap_or_default(), a, b, r.unwrap_or(Type::Empty))
        )(input);
    }

    fn nary_operation_header_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, (usize, Vec<String>, (String, Type), Vec<(String, Type)>, Type)> {
        return map(
            tuple((
                tag("op"),
                opt(
                    map(
                        tuple((
                            empty0,
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                        )),
                        |(_, _, _, t, _, _)| t
                    )
                ),
                empty1,
                delimited(
                    context("Expected '(' in operation definition", cut(tuple((tag("("), empty0)))),
                    tuple((
                        identifier_parser,
                        map(
                            opt(
                                map(
                                    tuple((
                                        empty0,
                                        tag(":"),
                                        empty0,
                                        cut(|input| self.type_parser(input)),
                                        empty0
                                    )),
                                    |(_, _, _, t, _)| t
                                )
                            ),
                            |t| t.unwrap_or(Type::Wildcard)
                        )
                    )),
                    context("Expected ')' in operation definition", cut(tuple((empty0, tag(")")))))
                ),
                empty0,
                |input| self.nary_operator_parser(input),
                opt(
                    preceded(
                        tuple((empty0, tag("->"), empty0)),
                        context("Expected return type after '->' in operation definition", cut(|input| self.type_parser(input)))
                    )
                )
            )),
            |(_, tm, _, a, _, (id, b), r)| (id, tm.unwrap_or_default(), a, b, r.unwrap_or(Type::Empty))
        )(input);
    }

    fn prefix_operation_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.prefix_operation_header_definition_parser(input),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((id, tm, n, mut t, mut r), _, mut b))| {
                t.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::PrefixOperationDefinition(l, id, tm, n, t, r, b)
            }
        )(input);
    }

    fn postfix_operation_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.postfix_operation_header_definition_parser(input),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((id, tm, n, mut t, mut r), _, mut b))| {
                t.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::PostfixOperationDefinition(l, id, tm, n, t, r, b)
            }
        )(input);
    }

    fn binary_operation_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.binary_operation_header_definition_parser(input),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((id, tm, mut a1, mut a2, mut r), _, mut b))| {
                a1.1.compile_templates(&tm);
                a2.1.compile_templates(&tm);
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::BinaryOperationDefinition(l, id, tm, a1, a2, r, b)
            }
        )(input);
    }

    fn nary_operation_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    |input| self.nary_operation_header_definition_parser(input),
                    empty0,
                    cut(|input| self.code_block_parser(input, op_cache)),
                ))
            ),
            |(l, ((id, tm, mut a1, mut a2, mut r), _, mut b))| {
                a1.1.compile_templates(&tm);
                a2.iter_mut().for_each(|(_, i)| i.compile_templates(&tm));
                r.compile_templates(&tm);
                b.iter_mut().for_each(|e| e.compile_types(&tm));

                NessaExpr::NaryOperationDefinition(l, id, tm, a1, a2, r, b)
            }
        )(input);
    }

    fn operation_definition_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return alt((
            |input| self.prefix_operation_definition_parser(input, op_cache),
            |input| self.postfix_operation_definition_parser(input, op_cache),
            |input| self.binary_operation_definition_parser(input, op_cache),
            |input| self.nary_operation_definition_parser(input, op_cache)
        ))(input);
    }

    fn code_block_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, Vec<NessaExpr>> {
        return delimited(
            tuple((tag("{"), empty0)),
            many_separated0(empty0, |input| self.nessa_line_parser(input, op_cache)),
            tuple((empty0, tag("}")))
        )(input);
    }

    fn macro_body_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaMacro> {
        return delimited(
            tuple((tag("{"), empty0)),
            parse_nessa_macro,
            tuple((empty0, tag("}")))
        )(input);
    }

    fn inline_class_syntax_parser<'a>(&self, input: Span<'a>) -> PResult<'a, Pattern> {
        return map(
            tuple((
                tag("syntax"),
                empty1,
                context("Expected 'from' after 'syntax' in class syntax definition", cut(tag("from"))),
                empty1,
                cut(|input| parse_ndl_pattern(input, true, true)),
                empty0,
                context("Expected ';' at the end of class syntax definition", cut(tag(";")))
            )),
            |(_, _, _, _, p, _, _)| p
        )(input);
    }

    fn alias_name_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, String> {
        return map(
            tuple((
                tag("type"),
                empty1,
                context("Invalid type identifier", cut(identifier_parser)),
                empty0,
                opt(
                    map(
                        tuple((
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                            empty0,
                        )),
                        |(_, _, t, _, _, _)| t
                    )
                ),
                context("Expected '=' after type name", cut(tag("=")))
            )),
            |(_, _, n, _, _, _)| n
        )(input);
    }

    fn alias_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("type"),
                    empty1,
                    context("Invalid type identifier", cut(identifier_parser)),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    identifier_parser
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    context("Expected '=' after type name", cut(tag("="))),
                    empty0,
                    cut(|input| self.type_parser(input)),
                    empty0,
                    context("Expected ';' at the end of type alias definition", cut(tag(";")))
                ))
            ),
            |(l, (_, _, n, _, tm, _, _, mut t, _, _))| {
                let u_t = tm.unwrap_or_default();

                t.compile_templates(&u_t);

                NessaExpr::ClassDefinition(l, n, u_t, vec!(), Some(t), vec!())
            }
        )(input);
    }

    fn class_name_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, String> {
        return map(
            tuple((
                tag("class"),
                empty1,
                context("Invalid class identifier", cut(identifier_parser)),
                empty0,
                opt(
                    map(
                        tuple((
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                            empty0,
                        )),
                        |(_, _, t, _, _, _)| t
                    )
                ),
                tag("{")
            )),
            |(_, _, n, _, _, _)| n
        )(input);
    }

    fn class_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("class"),
                    empty1,
                    context("Invalid class identifier", cut(identifier_parser)),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    identifier_parser
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    tag("{"),
                    empty0,
                    separated_list0(
                        empty0,
                        |input| self.inline_class_syntax_parser(input)
                    ),
                    empty0,
                    separated_list0(
                        empty0,
                        map(
                            tuple((
                                identifier_parser,
                                map(
                                    opt(
                                        map(
                                            tuple((
                                                empty0,
                                                tag(":"),
                                                empty0,
                                                cut(|input| self.type_parser(input)),
                                                empty0
                                            )),
                                            |(_, _, _, t, _)| t
                                        )
                                    ),
                                    |t| t.unwrap_or(Type::Wildcard)
                                ),
                                empty0,
                                context("Expected ';' at the end of class attribute definition", cut(tag(";")))
                            )),
                            |(a, b, _, _)| (a, b)
                        )
                    ),
                    empty0,
                    tag("}")
                ))
            ),
            |(l, (_, _, n, _, t, _, _, p, _, mut f, _, _))| {
                let u_t = t.unwrap_or_default();

                f.iter_mut().for_each(|(_, tp)| tp.compile_templates(&u_t));

                NessaExpr::ClassDefinition(l, n, u_t, f, None, p)
            }
        )(input);
    }

    fn interface_definition_name_parser<'a>(&self, input: Span<'a>) -> PResult<'a, String> {
        return map(
            tuple((
                tag("interface"),
                empty1,
                context("Invalid interface identifier", cut(identifier_parser)),
                empty0,
                opt(
                    map(
                        tuple((
                            tag("<"),
                            empty0,
                            separated_list1(
                                tuple((empty0, tag(","), empty0)), 
                                identifier_parser
                            ),
                            empty0,
                            tag(">"),
                            empty0,
                        )),
                        |(_, _, t, _, _, _)| t
                    )
                ),
                tag("{")
            )),
            |(_, _, n, _, _, _)| n
        )(input);
    }

    fn interface_definition_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("interface"),
                    empty1,
                    context("Invalid interface identifier", cut(identifier_parser)),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    identifier_parser
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    tag("{"),
                    empty0,
                    separated_list0(
                        empty0,
                        delimited(
                            empty0, 
                            |input| self.function_header_parser(input),
                            context("Expected ';' at the end of interface function signature", cut(tag(";")))
                        )
                    ),
                    empty0,
                    tag("}")
                ))
            ),
            |(l, (_, _, n, _, t, _, _, mut p, _, _))| {
                let u_t = t.unwrap_or_default();

                p.iter_mut().for_each(|(_, tm, args, ret)| {
                    let u_tm = tm.clone().unwrap_or_default();
                    let all_tm = u_t.iter().cloned().chain(u_tm).collect::<Vec<_>>();

                    args.iter_mut().for_each(|(_, tp)| {
                        tp.compile_templates(&all_tm);
                    });

                    ret.compile_templates(&all_tm);
                });

                NessaExpr::InterfaceDefinition(l, n, u_t, p)
            }
        )(input);
    }

    fn interface_implementation_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("implement"),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    identifier_parser
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    context("Invalid interface name", cut(identifier_parser)),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("<"),
                                empty0,
                                separated_list1(
                                    tuple((empty0, tag(","), empty0)), 
                                    cut(|input|self.type_parser(input))
                                ),
                                empty0,
                                tag(">"),
                                empty0,
                            )),
                            |(_, _, t, _, _, _)| t
                        )
                    ),
                    tag("for"),
                    empty1,
                    cut(|input|self.type_parser(input)),
                    empty0,
                    context("Expected ';' at the end of interface implementation", cut(tag(";")))
                ))
            ),
            |(l, (_, _, t, n, _, ts, _, _, mut tf, _, _))| {
                let u_tm = t.unwrap_or_default();
                let mut u_ts = ts.unwrap_or_default();
                tf.compile_templates(&u_tm);
                u_ts.iter_mut().for_each(|i| i.compile_templates(&u_tm));

                NessaExpr::InterfaceImplementation(l, u_tm, tf, n, u_ts)
            }
        )(input);
    }

    fn tuple_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("("),
                    empty0,
                    separated_list0(
                        tuple((empty0, tag(","), empty0)),
                        |input| self.nessa_expr_parser(input, &RefCell::default())
                    ),
                    empty0,
                    tag(")")
                ))
            ),
            |(l, (_, _, mut e, _, _))| {
                if e.len() == 1 {
                    return e.pop().unwrap();

                } else {
                    return NessaExpr::Tuple(l, e);
                }
            }
        )(input);
    }

    fn lambda_parser<'a>(&self, input: Span<'a>) -> PResult<'a, NessaExpr> {
        return map(
            located(
                tuple((
                    tag("("),
                    empty0,
                    separated_list0(
                        tuple((empty0, tag(","), empty0)), 
                        tuple((
                            identifier_parser,
                            map(
                                opt(
                                    map(
                                        tuple((
                                            empty0,
                                            tag(":"),
                                            empty0,
                                            cut(|input| self.type_parser(input)),
                                            empty0
                                        )),
                                        |(_, _, _, t, _)| t
                                    )
                                ),
                                |t| t.unwrap_or(Type::Wildcard)
                            )
                        ))
                    ),
                    empty0,
                    cut(tag(")")),
                    empty0,
                    opt(
                        map(
                            tuple((
                                tag("->"),
                                empty0,
                                cut(|input| self.type_parser(input)),
                                empty0,
                            )),
                            |(_, _, t, _)| t
                        ),
                    ),
                    alt((
                        |input| self.code_block_parser(input, &RefCell::default()),
                        map(
                            located(|input| self.nessa_expr_parser(input, &RefCell::default())),
                            |(l, e)| vec!(NessaExpr::Return(l, Box::new(e))) // Implicit return
                        )
                    ))
                ))   
            ),
            |(l, (_, _, a, _, _, _, r, b))| NessaExpr::Lambda(l, a, r.unwrap_or(Type::InferenceMarker), b)
        )(input);
    }

    fn nessa_expr_parser_wrapper<'a>(&self, input: Span<'a>, bi: &BitSet, nary: &BitSet, post: &BitSet, cache_bin: &mut ParserCache<'a>, cache_nary: &mut ParserCache<'a>, cache_post: &mut ParserCache<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return alt((
            |input| self.custom_syntax_parser(input),
            |input| self.tuple_parser(input),
            |input| self.lambda_parser(input),
            |input| self.operation_parser(input, bi, nary, post, cache_bin, cache_nary, cache_post, op_cache),
            |input| self.literal_parser(input),
            |input| self.variable_parser(input)
        ))(input);
    }

    pub fn nessa_expr_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return self.nessa_expr_parser_wrapper(
            input, 
            &self.get_bi_bitset(), &self.get_n_bitset(), &self.get_unary_bitset(), 
            &mut HashMap::new(), &mut HashMap::new(), &mut HashMap::new(), op_cache
        );
    }

    fn nessa_line_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
        return alt((
            |input| self.variable_definition_parser(input, op_cache),
            |input| self.variable_assignment_parser(input, op_cache),
            |input| self.return_parser(input, op_cache),
            |input| self.while_parser(input, op_cache),
            |input| self.for_parser(input, op_cache),
            |input| self.if_parser(input, op_cache),
            |input| terminated(|input| self.nessa_expr_parser(input, op_cache), cut(tuple((empty0, tag(";")))))(input)
        ))(input);
    }

    fn nessa_global_parser<'a>(&self, input: Span<'a>, op_cache: &OperatorCache<'a>) -> PResult<'a, NessaExpr> {
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
            |input| self.alias_definition_parser(input),
            |input| self.interface_definition_parser(input),
            |input| self.interface_implementation_parser(input),
            |input| self.macro_parser(input), 
            |input| terminated(|input| self.nessa_expr_parser(input, op_cache), cut(tuple((empty0, tag(";")))))(input)
        ))(input);
    }

    pub fn nessa_operators_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.operator_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_function_headers_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.function_header_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_operations_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.operation_definition_parser(input, &RefCell::default()) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_macros_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.macro_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_interface_implementation_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.interface_implementation_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_interface_definition_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.interface_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_interface_definition_names_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<String>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.interface_definition_name_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_class_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        let mut ops = vec!();

        while input.len() > 0 {
            if let Ok((i, o)) = self.class_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else if let Ok((i, o)) = self.alias_definition_parser(input) {
                input = i;
                ops.push(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_class_names_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, HashSet<String>> {
        let mut ops = HashSet::new();

        while input.len() > 0 {
            if let Ok((i, o)) = self.class_name_definition_parser(input) {
                input = i;
                ops.insert(o);
            
            } else if let Ok((i, o)) = self.alias_name_definition_parser(input) {
                input = i;
                ops.insert(o);
            
            } else {
                input = skip_token(input)?.0;
            }
        }

        return Ok(("".into(), ops));
    }

    pub fn nessa_parser<'a>(&self, mut input: Span<'a>) -> PResult<'a, Vec<NessaExpr>> {
        while let Ok((i, _)) = nessa_info_parser(input) {
            input = i;
        }

        let cache = RefCell::default();

        return delimited(
            empty0,
            many_separated0(empty0, |input| self.nessa_global_parser(input, &cache)),
            tuple((empty0, eof))
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
    use crate::ARR_OF;
    use crate::context::*;
    use crate::interfaces::ITERABLE_ID;
    use crate::interfaces::PRINTABLE_ID;
    use crate::parser::*;
    use crate::object::*;
    use crate::number::*;

    #[test]
    fn type_parsing() {
        let mut ctx = standard_ctx();

        ctx.define_type("Map".into(), vec!("Key".into(), "Value".into()), vec!(), None, vec!(), None).unwrap();
        let map_id = ctx.get_type_id("Map".into()).unwrap();

        let wildcard_str = "*";
        let empty_str = "()";

        let number_str = "Int";
        let number_ref_str = "&Int";
        let string_mut_str = "&&String";
        let wildcard_mut_str = "&&*";

        let or_str = "Int | &&String";
        let and_str = "(Int, &&String, &Bool)";
        let and_one_str = "(Int)";

        let array_str = "Array<Int>";
        let map_str = "Map<(Int), String>";
        let map_refs_str = "&Map<&Int, &&String>";

        let basic_func_str = "Int => (String)";
        let complex_func_str = "(Int, Array<Bool>) => Map<Int, *>";

        let template_str = "'T";
        let template_bounded_str = "'T [Printable, Iterable<Int>]";

        let (_, wildcard) = ctx.type_parser(Span::new(wildcard_str)).unwrap();
        let (_, empty) = ctx.type_parser(Span::new(empty_str)).unwrap();

        assert_eq!(wildcard, Type::Wildcard);
        assert_eq!(empty, Type::Empty);

        let (_, number) = ctx.type_parser(Span::new(number_str)).unwrap();
        let (_, number_ref) = ctx.type_parser(Span::new(number_ref_str)).unwrap();
        let (_, string_mut) = ctx.type_parser(Span::new(string_mut_str)).unwrap();
        let (_, wildcard_mut) = ctx.type_parser(Span::new(wildcard_mut_str)).unwrap();

        assert_eq!(number, INT);
        assert_eq!(number_ref, Type::Ref(Box::new(INT)));
        assert_eq!(string_mut, Type::MutRef(Box::new(STR)));
        assert_eq!(wildcard_mut, Type::MutRef(Box::new(Type::Wildcard)));

        let (_, or) = ctx.type_parser(Span::new(or_str)).unwrap();
        let (_, and) = ctx.type_parser(Span::new(and_str)).unwrap();
        let (_, and_one) = ctx.type_parser(Span::new(and_one_str)).unwrap();

        assert_eq!(or, Type::Or(vec!(INT, Type::MutRef(Box::new(STR)))));
        assert_eq!(and, Type::And(vec!(INT, Type::MutRef(Box::new(STR)), Type::Ref(Box::new(BOOL)))));
        assert_eq!(and_one, INT);

        let (_, array) = ctx.type_parser(Span::new(array_str)).unwrap();
        let (_, map) = ctx.type_parser(Span::new(map_str)).unwrap();
        let (_, map_refs) = ctx.type_parser(Span::new(map_refs_str)).unwrap();

        assert_eq!(array, ARR_OF!(INT));
        assert_eq!(map, Type::Template(map_id, vec!(INT, STR)));
        assert_eq!(map_refs, Type::Ref(Box::new(Type::Template(map_id, vec!(Type::Ref(Box::new(INT)), Type::MutRef(Box::new(STR)))))));
        
        let (_, basic_func) = ctx.type_parser(Span::new(basic_func_str)).unwrap();
        let (_, complex_func) = ctx.type_parser(Span::new(complex_func_str)).unwrap();

        assert_eq!(basic_func, Type::Function(Box::new(INT), Box::new(STR)));
        assert_eq!(complex_func, Type::Function(
            Box::new(Type::And(vec!(
                INT,
                ARR_OF!(BOOL)
            ))), 
            Box::new(Type::Template(map_id, vec!(
                INT,
                Type::Wildcard
            )))
        ));

        let (_, template) = ctx.type_parser(Span::new(template_str)).unwrap();
        let (_, template_bounded) = ctx.type_parser(Span::new(template_bounded_str)).unwrap();

        assert_eq!(template, Type::TemplateParamStr("T".into(), vec!()));
        assert_eq!(template_bounded, Type::TemplateParamStr("T".into(), vec!(
            InterfaceConstraint::new(PRINTABLE_ID, vec!()),
            InterfaceConstraint::new(ITERABLE_ID, vec!(INT))
        )));
    }

    #[test]
    fn literal_parsing() {
        let mut ctx = standard_ctx();

        let number_str = "123";
        let bool_v_str = "true";
        let string_str = "\"test\"";
        let escaped_string_str = "\"test\\ntest2\\ttest3\\\"\\\\\"";

        let (_, number) = ctx.literal_parser(Span::new(number_str)).unwrap();
        let (_, bool_v) = ctx.literal_parser(Span::new(bool_v_str)).unwrap();
        let (_, string) = ctx.literal_parser(Span::new(string_str)).unwrap();
        let (_, escaped_string) = ctx.literal_parser(Span::new(escaped_string_str)).unwrap();

        assert_eq!(number, NessaExpr::Literal(Location::none(), Object::new(Integer::from(123))));
        assert_eq!(bool_v, NessaExpr::Literal(Location::none(), Object::new(true)));
        assert_eq!(string, NessaExpr::Literal(Location::none(), Object::new("test".to_string())));
        assert_eq!(escaped_string, NessaExpr::Literal(Location::none(), Object::new("test\ntest2\ttest3\"\\".to_string())));

        ctx.define_type("Dice".into(), vec!(), vec!(
            ("rolls".into(), INT),
            ("sides".into(), INT)
        ), 
        None,
        vec!(
            Pattern::And(vec!(
                Pattern::Arg(Box::new(Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None)), "rolls".into()),
                Pattern::Str("D".into()),
                Pattern::Arg(Box::new(Pattern::Repeat(Box::new(Pattern::Symbol('d')), Some(1), None)), "sides".into()),
            ))
        ), Some(
            |ctx, c_type, s| {
                if let Ok((_, o)) = ctx.parse_literal_type(c_type, Span::new(s.as_str())) {
                    return Ok(o);
                }

                return Err(format!("Unable to parse {} from {}", c_type.name, s));
            }
        )).unwrap();
        

        let dice_str = "2D20";

        let (_, dice) = ctx.literal_parser(Span::new(dice_str)).unwrap();

        let id = ctx.get_type_id("Dice".into()).unwrap();

        assert_eq!(dice, NessaExpr::Literal(Location::none(), Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Integer::from(2)),
                Object::new(Integer::from(20))
            )
        })));

        assert_eq!(ctx.type_templates.last().unwrap().parser.unwrap()(&ctx, &ctx.type_templates[id], &"2D20".into()), Ok(Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Integer::from(2)),
                Object::new(Integer::from(20))
            )
        })));

        ctx.define_type("InnerDice".into(), vec!(), vec!(
            ("inner_dice".into(), Type::Basic(id))
        ),
        None,
        vec!(
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
        
        let (_, inner_dice) = ctx.literal_parser(Span::new(inner_dice_str)).unwrap();

        let inner_id = ctx.get_type_id("InnerDice".into()).unwrap();

        assert_eq!(inner_dice, NessaExpr::Literal(Location::none(), Object::new(TypeInstance {
            id: inner_id,
            params: vec!(),
            attributes: vec!(
                Object::new(TypeInstance {
                    id: id,
                    params: vec!(),
                    attributes: vec!(
                        Object::new(Integer::from(2)),
                        Object::new(Integer::from(20))
                    )
                })
            )
        })));
    }

    #[test]
    fn variable_definition_parsing() {
        let ctx = standard_ctx();

        let def_1_str = "let var: Int = a;";
        let def_str = "let foo: Array<Int | &String> = 5;";
        let def_3_str = "let bar = \"test\";";
        let def_4_str = "let foobar = false;";
        let def_5_str = "let lambda = (a: Int, b: Int) -> Bool { return a < b; };";
        let def_6_str = "let lambda = (n: Int) -> Int n * 2;";
        let def_7_str = "let lambda = (n: Int) n + 1;";

        let (_, def_1) = ctx.variable_definition_parser(Span::new(def_1_str), &RefCell::default()).unwrap();
        let (_, def) = ctx.variable_definition_parser(Span::new(def_str), &RefCell::default()).unwrap();
        let (_, def_3) = ctx.variable_definition_parser(Span::new(def_3_str), &RefCell::default()).unwrap();
        let (_, def_4) = ctx.variable_definition_parser(Span::new(def_4_str), &RefCell::default()).unwrap();
        let (_, def_5) = ctx.variable_definition_parser(Span::new(def_5_str), &RefCell::default()).unwrap();
        let (_, def_6) = ctx.variable_definition_parser(Span::new(def_6_str), &RefCell::default()).unwrap();
        let (_, def_7) = ctx.variable_definition_parser(Span::new(def_7_str), &RefCell::default()).unwrap();

        assert_eq!(def_1, NessaExpr::VariableDefinition(Location::none(), "var".into(), INT, Box::new(NessaExpr::NameReference(Location::none(), "a".into()))));
        assert_eq!(def, NessaExpr::VariableDefinition(Location::none(), 
            "foo".into(), 
            ARR_OF!(Type::Or(vec!(INT, STR.to_ref()))), 
            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5))))
        ));
        assert_eq!(def_3, NessaExpr::VariableDefinition(Location::none(), "bar".into(), Type::InferenceMarker, Box::new(NessaExpr::Literal(Location::none(), Object::new("test".to_string())))));
        assert_eq!(def_4, NessaExpr::VariableDefinition(Location::none(), "foobar".into(), Type::InferenceMarker, Box::new(NessaExpr::Literal(Location::none(), Object::new(false)))));
        assert_eq!(def_5, NessaExpr::VariableDefinition(Location::none(), 
            "lambda".into(), 
            Type::InferenceMarker, 
            Box::new(NessaExpr::Lambda(Location::none(), 
                vec!(
                    ("a".into(), INT),
                    ("b".into(), INT)
                ),
                BOOL,
                vec!(
                    NessaExpr::Return(Location::none(), Box::new(
                        NessaExpr::BinaryOperation(Location::none(), 
                            LT_BINOP_ID, 
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "a".into())),
                            Box::new(NessaExpr::NameReference(Location::none(), "b".into()))
                        )
                    ))
                )
            ))
        ));
        assert_eq!(def_6, NessaExpr::VariableDefinition(Location::none(), 
            "lambda".into(), 
            Type::InferenceMarker, 
            Box::new(NessaExpr::Lambda(Location::none(), 
                vec!(
                    ("n".into(), INT)
                ),
                INT,
                vec!(
                    NessaExpr::Return(Location::none(), Box::new(
                        NessaExpr::BinaryOperation(Location::none(), 
                            2, 
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "n".into())),
                            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(2))))
                        )
                    ))
                )
            ))
        ));
        assert_eq!(def_7, NessaExpr::VariableDefinition(Location::none(), 
            "lambda".into(), 
            Type::InferenceMarker, 
            Box::new(NessaExpr::Lambda(Location::none(), 
                vec!(
                    ("n".into(), INT)
                ),
                Type::InferenceMarker,
                vec!(
                    NessaExpr::Return(Location::none(), Box::new(
                        NessaExpr::BinaryOperation(Location::none(), 
                            0, 
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "n".into())),
                            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1))))
                        )
                    ))
                )
            ))
        ));
    }

    #[test]
    fn operation_parsing() {
        let mut ctx = standard_ctx();

        ctx.define_unary_operator("?".into(), false, 150).unwrap();

        let number_str = "-10";
        let var_str = "-!a";
        let n_var_str = "-5 + a?";
        let n_call_str = "5(-b + !10)";
        let template_func_str = "funct<Int>(5)";
        let template_prefix_str = "!<Int>7";
        let template_postfix_str = "false<&String>?";
        let template_binary_str = "\"test\" <String, Bool>+ true";

        let (_, number) = ctx.nessa_expr_parser(Span::new(number_str), &RefCell::default()).unwrap();
        let (_, var) = ctx.nessa_expr_parser(Span::new(var_str), &RefCell::default()).unwrap();
        let (_, n_var) = ctx.nessa_expr_parser(Span::new(n_var_str), &RefCell::default()).unwrap();
        let (_, n_call) = ctx.nessa_expr_parser(Span::new(n_call_str), &RefCell::default()).unwrap();
        let (_, template_func) = ctx.nessa_expr_parser(Span::new(template_func_str), &RefCell::default()).unwrap();
        let (_, template_prefix) = ctx.nessa_expr_parser(Span::new(template_prefix_str), &RefCell::default()).unwrap();
        let (_, template_postfix) = ctx.nessa_expr_parser(Span::new(template_postfix_str), &RefCell::default()).unwrap();
        let (_, template_binary) = ctx.nessa_expr_parser(Span::new(template_binary_str), &RefCell::default()).unwrap();

        assert_eq!(number, NessaExpr::UnaryOperation(Location::none(), 0, vec!(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(10))))));
        assert_eq!(
            var, 
            NessaExpr::UnaryOperation(Location::none(), 0, vec!(), 
            Box::new(NessaExpr::UnaryOperation(Location::none(), 1, vec!(), Box::new(NessaExpr::NameReference(Location::none(), "a".into()))))
        ));
        assert_eq!(n_var, NessaExpr::BinaryOperation(Location::none(), 
            0, 
            vec!(),
            Box::new(NessaExpr::UnaryOperation(Location::none(), 0, vec!(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))))),
            Box::new(NessaExpr::UnaryOperation(Location::none(), 3, vec!(), Box::new(NessaExpr::NameReference(Location::none(), "a".into())))),
        ));
        assert_eq!(n_call, NessaExpr::NaryOperation(Location::none(), 
            0, 
            vec!(),
            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))),
            vec!(
                NessaExpr::BinaryOperation(Location::none(), 
                    0, 
                    vec!(),
                    Box::new(NessaExpr::UnaryOperation(Location::none(), 0, vec!(), Box::new(NessaExpr::NameReference(Location::none(), "b".into())))),
                    Box::new(NessaExpr::UnaryOperation(Location::none(), 1, vec!(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(10)))))),
                )
            )
        ));
        assert_eq!(template_func, NessaExpr::NaryOperation(Location::none(), 
            0, 
            vec!(INT),
            Box::new(NessaExpr::NameReference(Location::none(), "funct".into())),
            vec!(
                NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))
            )
        ));
        assert_eq!(
            template_prefix, 
            NessaExpr::UnaryOperation(Location::none(), 
                1, 
                vec!(INT), 
                Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(7))))
            )
        );
        assert_eq!(
            template_postfix, 
            NessaExpr::UnaryOperation(Location::none(), 
                3, 
                vec!(Type::Ref(Box::new(STR))), 
                Box::new(NessaExpr::Literal(Location::none(), Object::new(false)))
            )
        );
        assert_eq!(
            template_binary, 
            NessaExpr::BinaryOperation(Location::none(), 
                0, 
                vec!(STR, BOOL), 
                Box::new(NessaExpr::Literal(Location::none(), Object::new("test".to_string()))),
                Box::new(NessaExpr::Literal(Location::none(), Object::new(true)))
            )
        );
    }

    #[test]
    fn function_header_parsing() {
        let mut ctx = standard_ctx();

        ctx.define_type("Map".into(), vec!("Key".into(), "Value".into()), vec!(), None, vec!(), None).unwrap();
        let map_id = ctx.get_type_id("Map".into()).unwrap();

        let number_header_str = "fn test(a: Int) -> Int";
        let ref_header_str = "fn test(arg: &Int) -> &&Int";
        let two_args_header_str = "fn test_3(arg_1: &Int, arg: String | Int) -> Int | String";
        let complex_args_header_str = "fn test_4(a: String | &Int, b: &Array<(Bool, Int)>, c: &&*) -> Map<Int, String>";

        let (_, number_header) = ctx.function_header_parser(Span::new(number_header_str)).unwrap();
        let (_, ref_header) = ctx.function_header_parser(Span::new(ref_header_str)).unwrap();
        let (_, two_args_header) = ctx.function_header_parser(Span::new(two_args_header_str)).unwrap();
        let (_, complex_args_header) = ctx.function_header_parser(Span::new(complex_args_header_str)).unwrap();

        assert_eq!(number_header, ("test".into(), None, vec!(("a".into(), INT)), INT));
        assert_eq!(ref_header, ("test".into(), None, vec!(("arg".into(), Type::Ref(Box::new(INT)))), Type::MutRef(Box::new(INT))));
        assert_eq!(two_args_header, (
            "test_3".into(), 
            None,
            vec!(
                ("arg_1".into(), Type::Ref(Box::new(INT))),
                ("arg".into(), Type::Or(vec!(
                    INT,
                    STR
                )))
            ),
            Type::Or(vec!(
                INT,
                STR
            ))
        ));
        assert_eq!(complex_args_header, (
            "test_4".into(), 
            None, 
            vec!(
                ("a".into(), Type::Or(vec!(
                    Type::Ref(Box::new(INT)),
                    STR
                ))),
                ("b".into(), Type::Ref(Box::new(
                    Type::Template(
                        ARR_ID,
                        vec!(Type::And(vec!(
                            BOOL,
                            INT
                        )))
                    ))
                )),
                ("c".into(), Type::MutRef(Box::new(Type::Wildcard)))
            ),
            Type::Template(
                map_id,
                vec!(
                    INT,
                    STR
                )
            )
        ));
    }

    #[test]
    fn function_definition_and_flow_control_parsing() {
        let mut ctx = standard_ctx();

        ctx.define_type("Map".into(), vec!("Key".into(), "Value".into()), vec!(), None, vec!(), None).unwrap();
        let map_id = ctx.get_type_id("Map".into()).unwrap();

        let test_1_str = "fn inc() -> Int {
            let res = 5;

            for i in arr {
                return 7;
            }

            return res;
        }";

        let test_str = "fn inc(arg: &Int) -> Int | String {
            let r: Int = arg + 1;

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

        let (_, test_1) = ctx.function_definition_parser(Span::new(test_1_str), &RefCell::default()).unwrap();
        let (_, test) = ctx.function_definition_parser(Span::new(test_str), &RefCell::default()).unwrap();
        let (_, test_3) = ctx.function_definition_parser(Span::new(test_3_str), &RefCell::default()).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::FunctionDefinition(Location::none(), 
                0,
                vec!(),
                vec!(),
                INT,
                vec!(
                    NessaExpr::VariableDefinition(Location::none(), "res".into(), Type::InferenceMarker, Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5))))),
                    NessaExpr::For(Location::none(), 
                        "i".into(),
                        Box::new(NessaExpr::NameReference(Location::none(), "arr".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(7)))))
                        )
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::NameReference(Location::none(), "res".into())))
                )
            ) 
        );

        assert_eq!(
            test,
            NessaExpr::FunctionDefinition(Location::none(), 
                0,
                vec!(),
                vec!(
                    (
                        "arg".into(), 
                        Type::Ref(Box::new(INT))
                    )
                ),
                Type::Or(vec!(
                    INT,
                    STR
                )),
                vec!(
                    NessaExpr::VariableDefinition(Location::none(), 
                        "r".into(), 
                        INT, 
                        Box::new(NessaExpr::BinaryOperation(Location::none(), 
                            0,
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1))))
                        ))
                    ),
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::BinaryOperation(Location::none(), 
                            0,
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "r".into())),
                            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1))))
                        )),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new("a".to_string()))))
                        ),
                        vec!(
                            (
                                NessaExpr::BinaryOperation(Location::none(), 
                                    0,
                                    vec!(),
                                    Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                                    Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(2))))
                                ),
                                vec!(
                                    NessaExpr::VariableAssignment(Location::none(), 
                                        "r".into(),
                                        Box::new(NessaExpr::BinaryOperation(Location::none(), 
                                            0,
                                            vec!(),
                                            Box::new(NessaExpr::NameReference(Location::none(), "r".into())),
                                            Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1))))
                                        ))
                                    )
                                )
                            )
                        ),
                        Some(vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))))
                        ))
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::NameReference(Location::none(), "r".into())))
                )
            ) 
        );
        assert_eq!(
            test_3,
            NessaExpr::FunctionDefinition(Location::none(), 
                0,
                vec!("K".into(), "V".into()),
                vec!(
                    ("key".into(), T_0),
                    ("value".into(), T_1)
                ),
                Type::Template(map_id, vec!(T_0, T_1)),
                vec!(
                    NessaExpr::VariableDefinition(Location::none(), 
                        "a".into(), 
                        Type::Or(vec!(T_0, T_1)), 
                        Box::new(NessaExpr::BinaryOperation(Location::none(), 
                            0,
                            vec!(),
                            Box::new(NessaExpr::NameReference(Location::none(), "value".into())),
                            Box::new(NessaExpr::NameReference(Location::none(), "key".into())),
                        ))
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::NameReference(Location::none(), "a".into())))
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

        let (_, prefix) = ctx.operator_definition_parser(Span::new(prefix_str)).unwrap();
        let (_, postfix) = ctx.operator_definition_parser(Span::new(postfix_str)).unwrap();
        let (_, binary) = ctx.operator_definition_parser(Span::new(binary_str)).unwrap();
        let (_, nary) = ctx.operator_definition_parser(Span::new(nary_str)).unwrap();

        assert_eq!(prefix, NessaExpr::PrefixOperatorDefinition(Location::none(), "~".into(), 200));
        assert_eq!(postfix, NessaExpr::PostfixOperatorDefinition(Location::none(), "&".into(), 300));
        assert_eq!(binary, NessaExpr::BinaryOperatorDefinition(Location::none(), "$".into(), false, 400));
        assert_eq!(nary, NessaExpr::NaryOperatorDefinition(Location::none(), "`".into(), "´".into(), 500));
    }

    #[test]
    fn operation_definition_and_flow_control_parsing() {
        let mut ctx = standard_ctx();

        ctx.define_unary_operator("?".into(), false, 150).unwrap();

        let test_1_str = "op !(arg: Bool) -> Bool {
            if arg {
                return false;
            }

            return true;
        }";

        let test_str = "op (arg: Bool)? -> Int | Bool {
            if arg {
                return 5;
            }

            for i in arr {
                return i;
            }

            return true;
        }";

        let test_3_str = "op (a: Bool) + (b: Bool) -> Int {
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

        let test_4_str = "op (a: Int)[b: Int, c: Int] -> (Int, Bool) {
            return (a + b * c, true);
        }";

        let (_, test_1) = ctx.operation_definition_parser(Span::new(test_1_str), &RefCell::default()).unwrap();
        let (_, test) = ctx.operation_definition_parser(Span::new(test_str), &RefCell::default()).unwrap();
        let (_, test_3) = ctx.operation_definition_parser(Span::new(test_3_str), &RefCell::default()).unwrap();
        let (_, test_4) = ctx.operation_definition_parser(Span::new(test_4_str), &RefCell::default()).unwrap();

        assert_eq!(
            test_1,
            NessaExpr::PrefixOperationDefinition(Location::none(), 
                1,
                vec!(),
                "arg".into(),
                BOOL,
                BOOL,
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(false))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test,
            NessaExpr::PostfixOperationDefinition(Location::none(), 
                3,
                vec!(),
                "arg".into(),
                BOOL,
                Type::Or(vec!(
                    INT,
                    BOOL
                )),
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::For(Location::none(), 
                        "i".into(),
                        Box::new(NessaExpr::NameReference(Location::none(), "arr".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::NameReference(Location::none(), "i".into())))
                        )
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test_3,
            NessaExpr::BinaryOperationDefinition(Location::none(), 
                0,
                vec!(),
                ("a".into(), BOOL),
                ("b".into(), BOOL),
                INT,
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "a".into())),
                        vec!(
                            NessaExpr::If(Location::none(), 
                                Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                                vec!(
                                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(2)))))
                                ),
                                vec!(),
                                None
                            ),
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(0)))))
                )
            ) 
        );

        assert_eq!(
            test_4,
            NessaExpr::NaryOperationDefinition(Location::none(), 
                1,
                vec!(),
                ("a".into(), INT),
                vec!(
                    ("b".into(), INT),
                    ("c".into(), INT)
                ),
                Type::And(vec!(INT, BOOL)),
                vec!(
                    NessaExpr::Return(Location::none(), Box::new(
                        NessaExpr::Tuple(Location::none(), vec!(
                            NessaExpr::BinaryOperation(Location::none(), 
                                0,
                                vec!(),
                                Box::new(NessaExpr::NameReference(Location::none(), "a".into())),
                                Box::new(NessaExpr::BinaryOperation(Location::none(), 
                                    2,
                                    vec!(),
                                    Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                                    Box::new(NessaExpr::NameReference(Location::none(), "c".into()))
                                )
                            )),
                            NessaExpr::Literal(Location::none(), Object::new(true))
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

        let test_template_str = "op<T> (arg: 'T)? -> Int | 'T {
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

        let test_template_4_str = "op<T, G> (a: 'T)[b: 'G, c: Int] -> ('T, Array<'G>) {
            return (a + b * c, true);
        }";

        let (_, test_template_1) = ctx.operation_definition_parser(Span::new(test_template_1_str), &RefCell::default()).unwrap();
        let (_, test_template) = ctx.operation_definition_parser(Span::new(test_template_str), &RefCell::default()).unwrap();
        let (_, test_template_3) = ctx.operation_definition_parser(Span::new(test_template_3_str), &RefCell::default()).unwrap();
        let (_, test_template_4) = ctx.operation_definition_parser(Span::new(test_template_4_str), &RefCell::default()).unwrap();

        assert_eq!(
            test_template_1,
            NessaExpr::PrefixOperationDefinition(Location::none(), 
                1,
                vec!("T".into()),
                "arg".into(),
                T_0,
                T_0,
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(false))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test_template,
            NessaExpr::PostfixOperationDefinition(Location::none(), 
                3,
                vec!("T".into()),
                "arg".into(),
                T_0,
                Type::Or(vec!(
                    INT,
                    T_0
                )),
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "arg".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(5)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::For(Location::none(), 
                        "i".into(),
                        Box::new(NessaExpr::NameReference(Location::none(), "arr".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::NameReference(Location::none(), "i".into())))
                        )
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(true))))
                )
            ) 
        );

        assert_eq!(
            test_template_3,
            NessaExpr::BinaryOperationDefinition(Location::none(), 
                0,
                vec!("T".into(), "G".into()),
                ("a".into(), T_0),
                ("b".into(), T_0),
                T_1,
                vec!(
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "a".into())),
                        vec!(
                            NessaExpr::If(Location::none(), 
                                Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                                vec!(
                                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(2)))))
                                ),
                                vec!(),
                                None
                            ),
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::If(Location::none(), 
                        Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                        vec!(
                            NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(1)))))
                        ),
                        vec!(),
                        None
                    ),
                    NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::new(Integer::from(0)))))
                )
            ) 
        );

        assert_eq!(
            test_template_4,
            NessaExpr::NaryOperationDefinition(Location::none(), 
                1,
                vec!("T".into(), "G".into()),
                ("a".into(), T_0),
                vec!(
                    ("b".into(), T_1),
                    ("c".into(), INT)
                ),
                Type::And(vec!(T_0, ARR_OF!(T_1))),
                vec!(
                    NessaExpr::Return(Location::none(), Box::new(
                        NessaExpr::Tuple(Location::none(), vec!(
                            NessaExpr::BinaryOperation(Location::none(), 
                                0,
                                vec!(),
                                Box::new(NessaExpr::NameReference(Location::none(), "a".into())),
                                Box::new(NessaExpr::BinaryOperation(Location::none(), 
                                    2,
                                    vec!(),
                                    Box::new(NessaExpr::NameReference(Location::none(), "b".into())),
                                    Box::new(NessaExpr::NameReference(Location::none(), "c".into()))
                                )
                            )),
                            NessaExpr::Literal(Location::none(), Object::new(true))
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
            faces: Int;
            rolls: Int;
        }";

        let sync_lists_str = "class SyncLists<K, V> {
            syntax from 'test';
            syntax from [[a-h] | d];
            syntax from Arg(['-'], Sign) Arg(1{d}, Int) ['.' Arg(1{d}, Dec)];

            from: Array<'K>;
            to: Array<'V>;
        }";

        let (_, dice_roll) = ctx.class_definition_parser(Span::new(dice_roll_str)).unwrap();
        let (_, sync_lists) = ctx.class_definition_parser(Span::new(sync_lists_str)).unwrap();

        assert_eq!(dice_roll, NessaExpr::ClassDefinition(Location::none(), 
            "DiceRoll".into(),
            vec!(),
            vec!(
                ("faces".into(), INT),
                ("rolls".into(), INT)
            ),
            None,
            vec!()
        ));

        assert_eq!(sync_lists, NessaExpr::ClassDefinition(Location::none(), 
            "SyncLists".into(),
            vec!("K".into(), "V".into()),
            vec!(
                ("from".into(), ARR_OF!(T_0)),
                ("to".into(), ARR_OF!(T_1))
            ),
            None,
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

    #[test]
    fn alias_definition_parsing() {
        let ctx = standard_ctx();

        let number_str = "type Number = Int | Float;";

        let (_, number) = ctx.alias_definition_parser(Span::new(number_str)).unwrap();

        assert_eq!(number, NessaExpr::ClassDefinition(Location::none(), 
            "Number".into(),
            vec!(),
            vec!(),
            Some(Type::Or(vec!(
                INT, FLOAT
            ))),
            vec!()
        ));
    }
}