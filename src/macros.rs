use std::collections::{HashMap, HashSet};

use nom::{branch::alt, bytes::complete::tag, character::complete::satisfy, combinator::{eof, map, map_opt, peek, value}, multi::{many0, many_till}, sequence::{delimited, preceded, tuple}};

use crate::{context::NessaContext, parser::{empty0, identifier_parser, PResult, Span}};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NessaMacroType {
    Function, Expression, Block, Ndl
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NessaMacro {
    Text(String),
    Var(String),
    IndexedVar(String, String),
    Loop(String, String, Box<NessaMacro>),
    Seq(Vec<NessaMacro>),
    Code(Box<NessaMacro>)
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

pub fn parse_text(input: Span<'_>) -> PResult<NessaMacro> {
    map_opt(
        many_till(text_pattern_char, peek(text_pattern_end)),
        |(i, _)| if i.len() > 0 { Some(NessaMacro::Text(i.iter().collect())) } else { None }
    )(input)
}

pub fn parse_var<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    map(
        preceded(
            tag("$"),
            identifier_parser
        ),
        |i| NessaMacro::Var(i)
    )(input)
}

pub fn parse_index(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        tuple((
            tag("$"),
            identifier_parser,
            tag("."),
            identifier_parser
        )),
        |(_, c, _, i)| NessaMacro::IndexedVar(c, i)
    )(input)
}

pub fn parse_loop_header<'a>(input: Span<'a>) -> PResult<(String, String)> {
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

pub fn parse_loop(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        tuple((
            parse_loop_header,
            empty0,
            delimited(
                tag("{"),
                parse_nessa_macro,
                tag("}")                
            )
        )),
        |(h, _, b)| NessaMacro::Loop(h.0, h.1, Box::new(b))
    )(input)
}

pub fn parse_code(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        delimited(
            tag("{|"),
            parse_nessa_macro,
            tag("|}")
        ),
        |i| NessaMacro::Code(Box::new(i))
    )(input)
}

pub fn parse_nessa_macro_line(input: Span<'_>) -> PResult<NessaMacro> {
    alt((
        parse_index,
        parse_var,
        parse_loop,
        parse_code,
        parse_text
    ))(input)
}

pub fn parse_nessa_macro(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        many0(parse_nessa_macro_line),
        NessaMacro::Seq
    )(input)
}

impl NessaMacro {
    pub fn get_markers(&self) -> HashSet<(bool, String)> {
        return match self {
            NessaMacro::Loop(v, i, b) => {
                let mut res = b.get_markers().into_iter().chain(vec!((false, v.clone()), (true, i.clone()))).collect::<HashSet<_>>();
                let repeated = res.iter().filter(|(it, _)| *it).cloned().collect::<Vec<_>>();

                // Delete referenced iterator variables
                for (_, s) in repeated {
                    res.remove(&(false, s.clone()));
                }

                res
            },
            
            NessaMacro::Seq(b) => b.iter().flat_map(NessaMacro::get_markers).collect(),
            NessaMacro::Var(v) => vec!((false, v.clone())).into_iter().collect(),
            NessaMacro::IndexedVar(v, i) => vec!((false, v.clone()), (true, i.clone())).into_iter().collect(),

            NessaMacro::Code(c) => c.get_markers(),

            _ => HashSet::new(),
        };
    }

    pub fn expand(&self, args: &HashMap<String, Vec<String>>, ctx: &NessaContext) -> Result<String, String> {
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

        return match self {
            NessaMacro::Text(s) => Ok(s.clone()),
            
            NessaMacro::Var(v) => {
                let var = extract_var!(v)?;
                assert_single!(var, v);

                Ok(var[0].clone())
            },
            
            NessaMacro::IndexedVar(c, i) => {
                let cs = extract_var!(c)?;
                let idx = extract_var!(i)?;
                assert_single!(idx, i);

                match idx[0].parse::<usize>() {
                    Ok(idx_usize) => Ok(cs[idx_usize].clone()),
                    Err(_) => Err(format!("Unable to parse '{}' as an index", idx[0])),
                }
            },

            NessaMacro::Loop(c, i, b) => {
                let cont = extract_var!(c)?;

                let mut args_cpy = args.clone();

                let iters = (0..cont.len()).map(|iv| {
                    *args_cpy.entry(i.clone()).or_default() = vec!(iv.to_string());

                    b.expand(&args_cpy, ctx)

                }).collect::<Result<Vec<_>, _>>()?;

                Ok(iters.join(""))
            },
            
            NessaMacro::Seq(b) => {
                Ok(b.iter().map(|i| i.expand(args, ctx)).collect::<Result<Vec<_>, _>>()?.join(""))
            },

            NessaMacro::Code(p) => {
                let sub_code = p.expand(args, ctx)?;

                let ex = NessaContext::parse_and_execute_nessa_project_inner::<false>(
                    ctx.module_path.clone(), 
                    Some(sub_code), 
                    true, 
                    ctx.optimize,
                    &[]
                ).unwrap();

                Ok(ex.captured_output)
            }
        };
    }
}

mod tests {
    #[allow(unused)] 
    use std::collections::HashMap;
    #[allow(unused)] 
    use crate::context::standard_ctx;
    #[allow(unused)] 
    use crate::macros::NessaMacro;
    #[allow(unused)] 
    use super::parse_nessa_macro;

    #[test]
    fn macro_parsing() {
        let example_1 = "let test = arr<$type>();";
        let example_2 = "let \\$var = arr<$type>();";
        let example_3 = "if $cond { $expr \\}";
        let example_4 = "@list.i { let i = $list.i; }";
        let example_5 = "let res = {|$expr|};";

        let example_1_macro = parse_nessa_macro(example_1.into()).unwrap().1;
        let example_2_macro = parse_nessa_macro(example_2.into()).unwrap().1;
        let example_3_macro = parse_nessa_macro(example_3.into()).unwrap().1;
        let example_4_macro = parse_nessa_macro(example_4.into()).unwrap().1;
        let example_5_macro = parse_nessa_macro(example_5.into()).unwrap().1;

        assert_eq!(example_1_macro, NessaMacro::Seq(vec!(
            NessaMacro::Text("let test = arr<".into()),
            NessaMacro::Var("type".into()),
            NessaMacro::Text(">();".into())
        )));
        
        assert_eq!(example_2_macro, NessaMacro::Seq(vec!(
            NessaMacro::Text("let $var = arr<".into()),
            NessaMacro::Var("type".into()),
            NessaMacro::Text(">();".into())
        )));
        
        assert_eq!(example_3_macro, NessaMacro::Seq(vec!(
            NessaMacro::Text("if ".into()),
            NessaMacro::Var("cond".into()),
            NessaMacro::Text(" { ".into()),
            NessaMacro::Var("expr".into()),
            NessaMacro::Text(" }".into()),
        )));
        
        assert_eq!(example_4_macro, NessaMacro::Seq(vec!(
            NessaMacro::Loop("list".into(), "i".into(), Box::new(NessaMacro::Seq(vec!(
                NessaMacro::Text(" let i = ".into()),
                NessaMacro::IndexedVar("list".into(), "i".into()),
                NessaMacro::Text("; ".into())
            ))))
        )));
        
        assert_eq!(example_5_macro, NessaMacro::Seq(vec!(
            NessaMacro::Text("let res = ".into()),
            NessaMacro::Code(Box::new(NessaMacro::Seq(vec!(NessaMacro::Var("expr".into()))))),
            NessaMacro::Text(";".into()),
        )));
    }

    #[test]
    fn macro_expansion() {
        let ctx = standard_ctx();
        
        // Array syntax
        let macro_ex_str = "let res = arr<$type>(); @elems.i {res.push($elems.i);} return move(res);";

        let macro_ex = parse_nessa_macro(macro_ex_str.into()).unwrap().1;

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

        let macro_ex = parse_nessa_macro(macro_ex_str.into()).unwrap().1;

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