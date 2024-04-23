use colored::Colorize;
use nom::{bytes::complete::tag, combinator::{map, opt}, multi::separated_list0, sequence::{delimited, preceded, terminated, tuple}};
use rustc_hash::FxHashMap;

use crate::parser::{empty0, identifier_parser, string_parser, PResult, Span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Annotation {
    pub name: String,
    pub args: FxHashMap<String, String>
}

pub fn parse_annotation<'a>(input: Span<'a>) -> PResult<'a, Annotation> {
    map(
        preceded(
            tag("@"),
            tuple((
                identifier_parser,
                opt(delimited(
                    tuple((empty0, tag("("), empty0)),
                    separated_list0(
                        tuple((empty0, tag(","), empty0)), 
                        tuple((
                            opt(terminated(identifier_parser, tuple((empty0, tag(":"), empty0)))),
                            string_parser
                        ))
                    ),
                    tuple((empty0, opt(tuple((tag(","), empty0))), tag(")")))
                ))
            ))
        ),
        |(n, args)| {
            let mut idx = 0;

            Annotation {
                name: n,
                args: args.unwrap_or_default().iter().map(|(k, v)| {
                    if let Some(k_inner) = k {
                        (k_inner.into(), v.into())
    
                    } else {
                        let old_idx = idx;
                        idx += 1;
                        (old_idx.to_string(), v.into())
                    }
                }).collect(),
            }
        }
    )(input)
}

impl Annotation {
    pub fn check_args(&self, required: &[&str], optional: &[&str]) -> Result<(), String> {
        // Check required arguments
        for r in required {
            if !self.args.contains_key(*r) {
                if r.parse::<usize>().is_ok() {
                    return Err(format!("Annotation {} does not contain required positional argument with index {}", self.name.cyan(), r.green()));

                } else {
                    return Err(format!("Annotation {} does not contain required argument with name {}", self.name.cyan(), r.green()));
                }
            }
        }

        // Check the rest of the arguments
        for arg in self.args.keys() {
            if !required.contains(&arg.as_str()) && !optional.contains(&arg.as_str()) {
                if arg.parse::<usize>().is_ok() {
                    return Err(format!("Unknown positional argument with index {} for annotation {}", arg.green(), self.name.cyan()));

                } else {
                    return Err(format!("Unknown argument with name {} for annotation {}", arg.green(), self.name.cyan()));
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use rustc_hash::FxHashMap;

    use crate::annotations::Annotation;

    use super::parse_annotation;

    #[test]
    fn annotation_parsing() {
        let empty_str = "@example";
        let empty_noargs_str = "@example()";
        let simple_str = "@doc(\"this is some doc\")";
        let named_str = "@arg(arg_name: \"doc1\", arg_name_2: \"doc2\")";
        let mixed_str = "@test(named_arg_1: \"doc1\", \"pos_arg_1\", named_arg_2: \"doc2\", \"pos_arg_2\")";

        let empty = parse_annotation(empty_str.into()).unwrap().1;
        let empty_noargs = parse_annotation(empty_noargs_str.into()).unwrap().1;
        let simple = parse_annotation(simple_str.into()).unwrap().1;
        let named = parse_annotation(named_str.into()).unwrap().1;
        let mixed = parse_annotation(mixed_str.into()).unwrap().1;

        assert_eq!(empty, Annotation { name: "example".into(), args: FxHashMap::default() });

        assert_eq!(empty_noargs, Annotation { name: "example".into(), args: FxHashMap::default() });

        assert_eq!(simple, Annotation { name: "doc".into(), args: [
            ("0".into(), "this is some doc".into())
        ].iter().cloned().collect() });

        assert_eq!(named, Annotation { name: "arg".into(), args: [
            ("arg_name".into(), "doc1".into()),
            ("arg_name_2".into(), "doc2".into())
        ].iter().cloned().collect() });

        assert_eq!(mixed, Annotation { name: "test".into(), args: [
            ("named_arg_1".into(), "doc1".into()),
            ("named_arg_2".into(), "doc2".into()),
            ("0".into(), "pos_arg_1".into()),
            ("1".into(), "pos_arg_2".into())
        ].iter().cloned().collect() });
    }
}