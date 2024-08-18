use std::collections::{HashMap, HashSet};

use nom::{branch::alt, bytes::complete::tag, character::complete::satisfy, combinator::{eof, map, map_opt, peek, value}, multi::{many0, many_till}, sequence::{delimited, preceded, tuple}};
use serde::{Deserialize, Serialize};

use crate::{annotations::Annotation, context::RynaContext, parser::{empty0, identifier_parser, Location, PResult, Span}, patterns::Pattern};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RynaMacroType {
    Function, Expression, Block, Rdl
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RdlMacro {
    Text(String),
    Var(String),
    IndexedVar(String, String),
    Loop(String, String, Box<RdlMacro>),
    Seq(Vec<RdlMacro>),
    Code(Box<RdlMacro>)
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RynaMacro {
    pub location: Location,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub m_type: RynaMacroType,
    pub pattern: Pattern,
    pub generator: RdlMacro
}

pub fn text_pattern_char(input: Span<'_>) -> PResult<char> {
    alt((
        preceded(
            tag("\\"), 
            map_opt(
                satisfy(|_| true),
                |c| match c {
                    '\\' => Some('\\'),
                    'n' => Some('\n'),
                    't' => Some('\t'),
                    '{' => Some('{'),
                    '}' => Some('}'),
                    '$' => Some('$'),
                    '@' => Some('@'),
                    _ => None
                }
            )
        ),
        satisfy(|_| true),
    ))(input)
}

pub fn text_pattern_end(input: Span<'_>) -> PResult<()> {
    alt((
        value((), tuple((tag("$"), identifier_parser))),
        value((), tuple((tag("@"), identifier_parser))),
        value((), tag("{|")),
        value((), tag("|}")),
        value((), tag("}")),
        value((), eof),
    ))(input)
}

pub fn parse_text(input: Span<'_>) -> PResult<RdlMacro> {
    map_opt(
        many_till(text_pattern_char, peek(text_pattern_end)),
        |(i, _)| if !i.is_empty() { Some(RdlMacro::Text(i.iter().collect())) } else { None }
    )(input)
}

pub fn parse_var(input: Span<'_>) -> PResult<RdlMacro> {
    map(
        preceded(
            tag("$"),
            identifier_parser
        ),
        RdlMacro::Var
    )(input)
}

pub fn parse_index(input: Span<'_>) -> PResult<RdlMacro> {
    map(
        tuple((
            tag("$"),
            identifier_parser,
            tag("."),
            identifier_parser
        )),
        |(_, c, _, i)| RdlMacro::IndexedVar(c, i)
    )(input)
}

pub fn parse_loop_header(input: Span<'_>) -> PResult<(String, String)> {
    map(
        tuple((
            tag("@"),
            identifier_parser,
            tag("."),
            identifier_parser
        )),
        |(_, c, _, i)| (c, i)
    )(input)
}

pub fn parse_loop(input: Span<'_>) -> PResult<RdlMacro> {
    map(
        tuple((
            parse_loop_header,
            empty0,
            delimited(
                tag("{"),
                parse_ryna_macro,
                tag("}")                
            )
        )),
        |(h, _, b)| RdlMacro::Loop(h.0, h.1, Box::new(b))
    )(input)
}

pub fn parse_code(input: Span<'_>) -> PResult<RdlMacro> {
    map(
        delimited(
            tag("{|"),
            parse_ryna_macro,
            tag("|}")
        ),
        |i| RdlMacro::Code(Box::new(i))
    )(input)
}

pub fn parse_ryna_macro_line(input: Span<'_>) -> PResult<RdlMacro> {
    alt((
        parse_index,
        parse_var,
        parse_loop,
        parse_code,
        parse_text
    ))(input)
}

pub fn parse_ryna_macro(input: Span<'_>) -> PResult<RdlMacro> {
    map(
        many0(parse_ryna_macro_line),
        RdlMacro::Seq
    )(input)
}

impl RdlMacro {
    pub fn get_markers(&self) -> HashSet<(bool, String)> {
        return match self {
            RdlMacro::Loop(v, i, b) => {
                let mut res = b.get_markers().into_iter().chain(vec!((false, v.clone()), (true, i.clone()))).collect::<HashSet<_>>();
                let repeated = res.iter().filter(|(it, _)| *it).cloned().collect::<Vec<_>>();

                // Delete referenced iterator variables
                for (_, s) in repeated {
                    res.remove(&(false, s.clone()));
                }

                res
            },
            
            RdlMacro::Seq(b) => b.iter().flat_map(RdlMacro::get_markers).collect(),
            RdlMacro::Var(v) => vec!((false, v.clone())).into_iter().collect(),
            RdlMacro::IndexedVar(v, i) => vec!((false, v.clone()), (true, i.clone())).into_iter().collect(),

            RdlMacro::Code(c) => c.get_markers(),

            _ => HashSet::new(),
        };
    }

    pub fn expand(&self, args: &HashMap<String, Vec<String>>, ctx: &RynaContext) -> Result<String, String> {
        macro_rules! extract_var {
            ($n: expr) => {
                match args.get($n) {
                    Some(inner) => Ok(inner),
                    _ => Err(format!("Did not extract variable with name '{}'", $n))
                }
            };
        }

        macro_rules! assert_single {
            ($args: expr, $n: expr) => {
                if $args.len() != 1 {
                    return Err(format!("Extracted {} arguments with name {} instead of 1", $args.len(), $n))
                }
            };
        }

        match self {
            RdlMacro::Text(s) => Ok(s.clone()),
            
            RdlMacro::Var(v) => {
                let var = extract_var!(v)?;
                assert_single!(var, v);

                Ok(var[0].clone())
            },
            
            RdlMacro::IndexedVar(c, i) => {
                let cs = extract_var!(c)?;
                let idx = extract_var!(i)?;
                assert_single!(idx, i);

                match idx[0].parse::<usize>() {
                    Ok(idx_usize) => Ok(cs[idx_usize].clone()),
                    Err(_) => Err(format!("Unable to parse '{}' as an index", idx[0])),
                }
            },

            RdlMacro::Loop(c, i, b) => {
                let cont = extract_var!(c)?;

                let mut args_cpy = args.clone();

                let iters = (0..cont.len()).map(|iv| {
                    *args_cpy.entry(i.clone()).or_default() = vec!(iv.to_string());

                    b.expand(&args_cpy, ctx)

                }).collect::<Result<Vec<_>, _>>()?;

                Ok(iters.join(""))
            },
            
            RdlMacro::Seq(b) => {
                Ok(b.iter().map(|i| i.expand(args, ctx)).collect::<Result<Vec<_>, _>>()?.join(""))
            },

            RdlMacro::Code(p) => {
                let sub_code = p.expand(args, ctx)?;

                let ex = RynaContext::parse_and_execute_ryna_project_inner::<false>(
                    ctx.module_path.clone(), 
                    Some(sub_code), 
                    true, 
                    ctx.optimize,
                    false,
                    &[]
                ).unwrap();

                Ok(ex.captured_output)
            }
        }
    }
}

mod tests {
    #[allow(unused)] 
    use std::collections::HashMap;
    #[allow(unused)] 
    use crate::context::standard_ctx;
    #[allow(unused)] 
    use crate::macros::RdlMacro;
    #[allow(unused)] 
    use super::parse_ryna_macro;

    #[test]
    fn macro_parsing() {
        let example_1 = "let test = arr<$type>();";
        let example_2 = "let \\$var = arr<$type>();";
        let example_3 = "if $cond { $expr \\}";
        let example_4 = "@list.i { let i = $list.i; }";
        let example_5 = "let res = {|$expr|};";

        let example_1_macro = parse_ryna_macro(example_1.into()).unwrap().1;
        let example_2_macro = parse_ryna_macro(example_2.into()).unwrap().1;
        let example_3_macro = parse_ryna_macro(example_3.into()).unwrap().1;
        let example_4_macro = parse_ryna_macro(example_4.into()).unwrap().1;
        let example_5_macro = parse_ryna_macro(example_5.into()).unwrap().1;

        assert_eq!(example_1_macro, RdlMacro::Seq(vec!(
            RdlMacro::Text("let test = arr<".into()),
            RdlMacro::Var("type".into()),
            RdlMacro::Text(">();".into())
        )));
        
        assert_eq!(example_2_macro, RdlMacro::Seq(vec!(
            RdlMacro::Text("let $var = arr<".into()),
            RdlMacro::Var("type".into()),
            RdlMacro::Text(">();".into())
        )));
        
        assert_eq!(example_3_macro, RdlMacro::Seq(vec!(
            RdlMacro::Text("if ".into()),
            RdlMacro::Var("cond".into()),
            RdlMacro::Text(" { ".into()),
            RdlMacro::Var("expr".into()),
            RdlMacro::Text(" }".into()),
        )));
        
        assert_eq!(example_4_macro, RdlMacro::Seq(vec!(
            RdlMacro::Loop("list".into(), "i".into(), Box::new(RdlMacro::Seq(vec!(
                RdlMacro::Text(" let i = ".into()),
                RdlMacro::IndexedVar("list".into(), "i".into()),
                RdlMacro::Text("; ".into())
            ))))
        )));
        
        assert_eq!(example_5_macro, RdlMacro::Seq(vec!(
            RdlMacro::Text("let res = ".into()),
            RdlMacro::Code(Box::new(RdlMacro::Seq(vec!(RdlMacro::Var("expr".into()))))),
            RdlMacro::Text(";".into()),
        )));
    }

    #[test]
    fn macro_expansion() {
        let ctx = standard_ctx();
        
        // Array syntax
        let macro_ex_str = "let res = arr<$type>(); @elems.i {res.push($elems.i);} return move(res);";

        let macro_ex = parse_ryna_macro(macro_ex_str.into()).unwrap().1;

        let expanded_code_ex = macro_ex.expand(&[
            ("type".into(), vec!("Int".into())),
            ("elems".into(), vec!("1".into(), "7".into(), "10".into()))
        ].iter().cloned().collect(), &ctx).unwrap();

        assert_eq!(
            expanded_code_ex, 
            "let res = arr<Int>(); res.push(1);res.push(7);res.push(10); return move(res);"
        );

        // Hashmap syntax
        let macro_ex_str = "let res = hashmap<$ktype, $vtype>(); @keys.i {res.add($keys.i, $values.i);} return move(res);";

        let macro_ex = parse_ryna_macro(macro_ex_str.into()).unwrap().1;

        let expanded_code_ex = macro_ex.expand(&[
            ("ktype".into(), vec!("Int".into())),
            ("vtype".into(), vec!("String".into())),
            ("keys".into(), vec!("1".into(), "7".into(), "10".into())),
            ("values".into(), vec!("\"Test 1\"".into(), "\"Test 2\"".into(), "\"Test 3\"".into()))
        ].iter().cloned().collect(), &ctx).unwrap();

        assert_eq!(
            expanded_code_ex, 
            "let res = hashmap<Int, String>(); res.add(1, \"Test 1\");res.add(7, \"Test 2\");res.add(10, \"Test 3\"); return move(res);"
        );
    }
}