use std::collections::{HashMap, HashSet};

use nom::{branch::alt, bytes::complete::{escaped_transform, tag, take_until, take_while1}, character::complete::satisfy, combinator::{map, value, opt}, sequence::{delimited, tuple, preceded}};
use regex::{Captures, Regex};

use crate::{context::NessaContext, parser::{many_separated0, Span, PResult, identifier_parser, empty0, empty1}, regex_ext::replace_all_fallible};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NessaMacroType {
    Function, Expression, Block
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NessaMacro {
    Text(String),
    Var(String),
    If(String, Box<NessaMacro>, Box<NessaMacro>),
    Loop(String, String, Box<NessaMacro>),
    Seq(Vec<NessaMacro>),
    Code(String)
}

pub fn parse_text(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        delimited(
            tag("{#"),
            escaped_transform(
                satisfy(|i| !"\\{#}".contains(i)), 
                '\\', 
                alt((
                    value("\n", tag("n")),
                    value("}", tag("}")),
                    value("{", tag("{")),
                    value("\\", tag("\\"))
                ))
            ),
            tag("}")
        ),
        NessaMacro::Text
    )(input)
}

pub fn parse_var<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    map(
        delimited(
            tag("{$"),
            delimited(
                empty0,
                take_while1(|i| !"{#}".contains(i)),
                empty0
            ),
            tag("}")
        ),
        |i: Span<'a>| NessaMacro::Var(i.to_string())
    )(input)
}

pub fn parse_loop_header(input: Span<'_>) -> PResult<(String, (), Span<'_>, (), Span<'_>, String)> {
    tuple((
        identifier_parser,
        empty1,
        tag("in"),
        empty1,
        tag("$"),
        identifier_parser
    ))(input)
}

pub fn parse_if(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        tuple((
            delimited(
                tag("{&"),
                delimited(empty0, identifier_parser, empty0),
                tag("}")
            ),
            preceded(
                empty0,
                delimited(
                    tag("{"),
                    parse_nessa_macro,
                    tag("}")                
                )
            ),
            opt(
                preceded(
                    empty0,   
                    delimited(
                        tag("{"),
                        parse_nessa_macro,
                        tag("}")                
                    )
                )
            )
        )),
        |(h, i, e)| NessaMacro::If(h, Box::new(i), Box::new(e.unwrap_or_else(|| NessaMacro::Text("".into()))))
    )(input)
}

pub fn parse_loop(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        tuple((
            delimited(
                tag("{@"),
                delimited(
                    empty0,
                    parse_loop_header,
                    empty0
                ),
                tag("}")
            ),
            empty0,
            delimited(
                tag("{"),
                parse_nessa_macro,
                tag("}")                
            )
        )),
        |(h, _, b)| NessaMacro::Loop(h.0, h.5.to_string(), Box::new(b))
    )(input)
}

pub fn parse_code(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        delimited(
            tag("{|"),
            take_until("|}"),
            tag("|}")
        ),
        |i: Span<'_>| NessaMacro::Code(i.to_string())
    )(input)
}

pub fn parse_nessa_macro_line(input: Span<'_>) -> PResult<NessaMacro> {
    alt((
        parse_var,
        parse_text,
        parse_if,
        parse_loop,
        parse_code
    ))(input)
}

pub fn parse_nessa_macro_lines(input: Span<'_>) -> PResult<Vec<NessaMacro>> {
    return delimited(
        empty0,
        many_separated0(empty0, |input| parse_nessa_macro_line(input)),
        empty0
    )(input);
}

pub fn parse_nessa_macro(input: Span<'_>) -> PResult<NessaMacro> {
    map(
        parse_nessa_macro_lines,
        NessaMacro::Seq
    )(input)
}

const MACRO_VAR_REGEX: &str = r"\{\$\s*([a-zA-Z0-9_]+)\s*\}";

fn sub_vars(code: &str, args: &HashMap<String, Vec<&str>>) -> Result<String, String> {
    let env_var_regex = Regex::new(MACRO_VAR_REGEX).unwrap();

    let replacement = |caps: &Captures| {
        let cap = caps.get(1).unwrap().as_str();
        
        match args.get(cap) {
            Some(var) if var.len() == 1 => Ok(var.first().unwrap().to_string()),
            Some(var) => Err(format!("Macro variable {} contains {} values", cap, var.len())),
            None => Err(format!("Unable to find {} macro variable", cap))
        }
    };
    
    replace_all_fallible(&env_var_regex, code, replacement)
}

fn get_var_refs(code: &str) -> Vec<String> {
    let env_var_regex = Regex::new(MACRO_VAR_REGEX).unwrap();

    env_var_regex.captures_iter(code).map(|i| i.get(1).unwrap().as_str().into()).collect()
}

impl NessaMacro {
    pub fn get_markers(&self) -> HashSet<(bool, String)> {
        return match self {
            NessaMacro::If(v, i, e) => i.get_markers().into_iter().chain(e.get_markers()).chain(vec!((false, v.clone()))).collect(),
            NessaMacro::Loop(v, c, b) => {
                let mut res = b.get_markers().into_iter().chain(vec!((true, v.clone()), (false, c.clone()))).collect::<HashSet<_>>();
                let repeated = res.iter().filter(|(it, _)| *it).cloned().collect::<Vec<_>>();

                // Delete referenced iterator variables
                for (_, s) in repeated {
                    res.remove(&(false, s.clone()));
                }

                res
            },
            NessaMacro::Seq(b) => b.iter().flat_map(NessaMacro::get_markers).collect(),
            NessaMacro::Var(v) => vec!((false, v.clone())).into_iter().collect(),

            NessaMacro::Code(c) => get_var_refs(c).into_iter().map(|i| (false, i)).collect(),

            _ => HashSet::new(),
        };
    }

    pub fn expand(&self, args: &HashMap<String, Vec<&str>>, ctx: &NessaContext) -> Result<String, String> {
        return match self {
            NessaMacro::Text(s) => Ok(s.clone()),
            
            NessaMacro::Var(v) => match args.get(v) {
                Some(r) => if r.len() == 1 {
                    Ok(r[0].to_string())

                } else {
                    Err(format!("Extracted {} arguments with name {} instead of 1", r.len(), v))
                },

                None => Err(format!("Did not extract variable with name '{v}'")),
            },

            NessaMacro::If(n, i, e) => {
                match args.get(n) {
                    Some(v) if !v.is_empty() => i.expand(args, ctx),
                    _ => e.expand(args, ctx)
                }
            },

            NessaMacro::Loop(i, c, b) => {
                let cont = match args.get(c) {
                    Some(r) => r.clone(),
                    None => return Err(format!("Did not extract variable with name '{c}'")),
                };

                let mut args_cpy = args.clone();

                let iters = cont.iter().map(|iv| {
                    if let Some(v) = args_cpy.get_mut(i) {
                        *v = vec!(iv);
                    
                    } else {
                        args_cpy.entry(i.clone()).or_insert(vec!(iv));
                    }

                    b.expand(&args_cpy, ctx)

                }).collect::<Result<Vec<_>, _>>()?;

                Ok(iters.join(""))
            },
            
            NessaMacro::Seq(b) => {
                Ok(b.iter().map(|i| i.expand(args, ctx)).collect::<Result<Vec<_>, _>>()?.join(""))
            },

            NessaMacro::Code(code) => {
                let sub_code = sub_vars(code, args)?;

                let ex = NessaContext::parse_and_execute_nessa_project_inner::<false>(ctx.module_path.clone(), Some(sub_code), true, &[]).unwrap();

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

    #[test]
    fn macro_parsing() {
        let text_1 = "{# this is an example}";
        let text_2 = "{#this is another example}";
        let text_3 = "{#}";

        assert!(crate::macros::parse_text(text_1.into()).is_ok());
        assert!(crate::macros::parse_text(text_2.into()).is_ok());
        assert!(crate::macros::parse_text(text_3.into()).is_err());

        let var_1 = "{$a}";
        let var_2 = "{$ b }";
        let var_3 = "{$}";

        assert!(crate::macros::parse_var(var_1.into()).is_ok());
        assert!(crate::macros::parse_var(var_2.into()).is_ok());
        assert!(crate::macros::parse_var(var_3.into()).is_err());

        let loop_1 = "{@i in $a} {
            {# this is an example}
        }";

        let loop_2 = "{@ i in $a } {
            {# this is an example}
            {#this is another example}
        }";

        let loop_3 = "{@ i in $a } {}";

        let loop_4 = "{@ i in $a}";
        let loop_5 = "{@ i in $a} {# test}";

        assert!(crate::macros::parse_loop(loop_1.into()).is_ok());
        assert!(crate::macros::parse_loop(loop_2.into()).is_ok());
        assert!(crate::macros::parse_loop(loop_3.into()).is_ok());
        assert!(crate::macros::parse_loop(loop_4.into()).is_err());
        assert!(crate::macros::parse_loop(loop_5.into()).is_err());

        let if_1 = "{&i} {
            {# this is an example}
        }";

        let if_2 = "{&i} {
            {# this is an example}
        } {
            {# this is another example}        
        }";

        let if_3 = "{&i} {} {
            {# this is another example}        
        }";

        let if_4 = "{&i} {
            {@ i in $a } {
                {# this is an example}
                {#this is another example}
            }
        } {
            {&i} {
                {# this is a final example}
            }     
        }";

        assert!(crate::macros::parse_if(if_1.into()).is_ok());
        assert!(crate::macros::parse_if(if_2.into()).is_ok());
        assert!(crate::macros::parse_if(if_3.into()).is_ok());
        assert!(crate::macros::parse_if(if_4.into()).is_ok());

        let macro_1 = "{#[}{$a}{#]}";
        let macro_2 = "{#[}{@i in $a}{ {#test} }";
        let macro_3 = "
        {#for}
        {$i}
        {#in}
        {$test}
        ";

        assert!(crate::macros::parse_nessa_macro(macro_1.into()).is_ok());
        assert!(crate::macros::parse_nessa_macro(macro_2.into()).is_ok());
        assert!(crate::macros::parse_nessa_macro(macro_3.into()).is_ok());
    }

    #[test]
    fn macro_expansion() {
        let ctx = standard_ctx();

        let macro_1_str = "
        {#let res = arr<}{$type}{#>();\\n}
        {@i in $values} {
            {#res.push(}{$i}{#);\\n}
        }
        {#return res;}
        ";

        let macro_1 = crate::macros::parse_nessa_macro(macro_1_str.into()).unwrap().1;

        let args = [
            ("type".into(), vec!("Int")),
            ("values".into(), vec!("5", "7", "8"))
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        let res = macro_1.expand(&args, &ctx).unwrap();

        assert_eq!(res, "let res = arr<Int>();\nres.push(5);\nres.push(7);\nres.push(8);\nreturn res;");

        let args = [
            ("type".into(), vec!("Int")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_1.expand(&args, &ctx).is_err());

        let args = [
            ("values".into(), vec!("5", "7", "8"))
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_1.expand(&args, &ctx).is_err());

        let macro_2_str = "
        {#let res = arr<}{$type}{#>();\\n}
        {#let func = (}{$it}{#) }{$map}{#;\\n\\n}
        {#for _it_ in }{$container}{#\\{\\n}
            {#  res.push(map(_it_));\\n}
        {#\\}\\n\\n}
        {#return res;}
        ";

        let macro_2 = crate::macros::parse_nessa_macro(macro_2_str.into()).unwrap().1;

        let args = [
            ("type".into(), vec!("Int")),
            ("it".into(), vec!("i")),
            ("container".into(), vec!("range(0, 10)")),
            ("map".into(), vec!("i * i")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        let res = macro_2.expand(&args, &ctx).unwrap();

        assert_eq!(res, "let res = arr<Int>();\nlet func = (i) i * i;\n\nfor _it_ in range(0, 10){\n  res.push(map(_it_));\n}\n\nreturn res;");

        let args = [
            ("type".into(), vec!("Int")),
            ("it".into(), vec!("i")),
            ("container".into(), vec!("range(0, 10)")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_2.expand(&args, &ctx).is_err());

        let args = [
            ("type".into(), vec!("Int")),
            ("container".into(), vec!("range(0, 10)")),
            ("map".into(), vec!("i * i")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_2.expand(&args, &ctx).is_err());

        let args = [
            ("it".into(), vec!("i")),
            ("container".into(), vec!("range(0, 10)")),
            ("map".into(), vec!("i * i")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_2.expand(&args, &ctx).is_err());

        let args = [
            ("type".into(), vec!("Int")),
            ("it".into(), vec!("i")),
            ("map".into(), vec!("i * i")),
        ].iter().cloned().collect::<HashMap<_, Vec<_>>>();

        assert!(macro_2.expand(&args, &ctx).is_err());
    }
}