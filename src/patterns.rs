use std::collections::{ HashMap, HashSet };

use nom::{
    combinator::{map, opt, value},
    bytes::complete::{take_while1, tag},
    sequence::{tuple, delimited, separated_pair},
    character::complete::{one_of, satisfy},
    branch::alt,
    multi::separated_list1
};
use serde::{Serialize, Deserialize};

use crate::{context::NessaContext, parser::{empty0, empty1, identifier_parser, string_parser, verbose_error, PCache, PResult, Span}};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Pattern{
    // Markers
    Arg(Box<Pattern>, String),

    // Tail patterns
    Str(String),
    Range(char, char),
    Symbol(char), // Digit (d), Letter (l/L), Alphabetic (a), Alphanumeric (A), Space (s), quote (q)

    // High level patterns
    Identifier,
    Type,
    Expr,

    // Combination patterns
    Or(Vec<Pattern>),
    And(Vec<Pattern>),
    Optional(Box<Pattern>),
    Repeat(Box<Pattern>, Option<usize>, Option<usize>)
}

impl Pattern{
    pub fn get_markers(&self) -> HashSet<String> {
        return match self {
            Pattern::Arg(_, n) => vec!(n.clone()).into_iter().collect(),
            Pattern::Or(p) |
            Pattern::And(p) => p.iter().flat_map(Pattern::get_markers).collect(),
            Pattern::Repeat(p, _, _) |
            Pattern::Optional(p) => p.get_markers(),
            _ => HashSet::new()
        };
    }

    pub fn extract<'a>(&self, text: Span<'a>, ctx: &NessaContext, cache: &PCache<'a>) -> PResult<'a, HashMap<String, Vec<&'a str>>> {
        fn merge<'a>(a: &mut HashMap<String, Vec<&'a str>>, b: HashMap<String, Vec<&'a str>>) {
            for (k, v) in b.into_iter() {
                a.entry(k).or_default().extend(v);
            }
        }

        return match self {
            Pattern::Symbol('d') => value(HashMap::new(), satisfy(|c| c.is_ascii_digit()))(text),
            Pattern::Symbol('l') => value(HashMap::new(), satisfy(|c| c.is_lowercase()))(text),
            Pattern::Symbol('L') => value(HashMap::new(), satisfy(|c| c.is_uppercase()))(text),
            Pattern::Symbol('a') => value(HashMap::new(), satisfy(|c| c.is_alphabetic()))(text),
            Pattern::Symbol('A') => value(HashMap::new(), satisfy(|c| c.is_alphanumeric()))(text),
            Pattern::Symbol('s') => value(HashMap::new(), empty1)(text),
            Pattern::Symbol('q') => value(HashMap::new(), satisfy(|c| c == '\''))(text),
            Pattern::Symbol(_) => unreachable!(),

            Pattern::Range(a, b) => value(HashMap::new(), satisfy(|c| c >= *a && c <= *b))(text),

            Pattern::Identifier => value(HashMap::new(), identifier_parser)(text),
            Pattern::Type => value(HashMap::new(), |input| ctx.type_parser(input))(text),
            Pattern::Expr => value(HashMap::new(), |input| ctx.nessa_expr_parser(input, cache))(text),

            Pattern::Str(s) => value(HashMap::new(), tag(s.as_str()))(text),

            Pattern::And(patterns) => {
                let mut res = HashMap::new();
                let mut input = text;

                for p in patterns {
                    if let Ok((i, o)) = p.extract(input, ctx, cache) {
                        input = i;
                        merge(&mut res, o);

                    } else {
                        return Err(verbose_error(text, "Unable to parse"))
                    }
                }
                
                Ok((input, res))
            },

            Pattern::Or(patterns) => {
                for p in patterns {
                    if let Ok((i, o)) = p.extract(text, ctx, cache) {
                        return Ok((i, o));
                    }
                }
                
                Err(verbose_error(text, "Unable to parse"))
            },
            
            Pattern::Repeat(p, from, to) => {
                let mut res = HashMap::new();
                let mut input = text;

                // Minimum
                if let Some(f) = from {
                    for _ in 0..*f {
                        if let Ok((i, o)) = p.extract(input, ctx, cache) {
                            input = i;
                            merge(&mut res, o);
    
                        } else {
                            return Err(verbose_error(text, "Unable to parse"))
                        }
                    }
                }

                // Maximum
                if let Some(t) = to {
                    for _ in 0..(t - from.unwrap_or(0)) {
                        if let Ok((i, o)) = p.extract(input, ctx, cache) {
                            input = i;
                            merge(&mut res, o);
    
                        } else {
                            break;
                        }
                    }

                } else {
                    while let Ok((i, o)) = p.extract(input, ctx, cache) {
                        input = i;
                        merge(&mut res, o);
                    }
                }

                Ok((input, res))
            }

            Pattern::Optional(p) => map(opt(|i| p.extract(i, ctx, cache)), Option::unwrap_or_default)(text),
            Pattern::Arg(p, k) => {
                let (i, mut o) = p.extract(text, ctx, cache)?;

                o.entry(k.clone()).or_default().push(&text[..(text.len() - i.len())]);

                Ok((i, o))
            }
        };
    }
}

fn parse_and(text: Span<'_>, and: bool) -> PResult<'_, Pattern> {
    return if and {
        map(
            separated_list1(empty1, |i| parse_ndl_pattern(i, false, false)), 
            |v| if v.len() > 1 { Pattern::And(v) } else { v[0].clone() }
        )(text)
        
    } else {
        Err(verbose_error(text, "Unable to parse"))
    }
}

fn parse_or(text: Span<'_>, or: bool) -> PResult<'_, Pattern> {
    return if or {
        return map(
            separated_list1(tuple((empty0, tag("|"), empty0)), |i| parse_ndl_pattern(i, false, true)), 
            |v| if v.len() > 1 { Pattern::Or(v) } else { v[0].clone() }
        )(text)
        
    } else {
        Err(verbose_error(text, "Unable to parse"))
    }
}

pub fn parse_ndl_pattern<'a>(text: Span<'a>, or: bool, and: bool) -> PResult<'a, Pattern> {
    return alt((
        |i| parse_or(i, or),
        |i| parse_and(i, and),
        map(delimited(tag("["), separated_pair(satisfy(|c| c != '\"'), tag("-"), satisfy(|c| c != '\"')), tag("]")), |(a, b)| Pattern::Range(a, b)),
        map(string_parser, |s: String| Pattern::Str(s.to_string())),
        map(delimited(
            tuple((tag("Arg("), empty0)),
            separated_pair(|i| parse_ndl_pattern(i, true, true), tuple((empty0, tag(","), empty0)), take_while1(|c| c != ')')),
            tuple((empty0, tag(")")))
        ), |(p, n)| Pattern::Arg(Box::new(p), n.to_string())),
        map(tuple((
            opt(map(take_while1(|c: char| c.is_ascii_digit()), |s: Span<'a>| s.parse::<usize>().unwrap())),
            delimited(tuple((tag("{"), empty0)), |i| parse_ndl_pattern(i, true, true), tuple((empty0, tag("}")))),
            opt(map(take_while1(|c: char| c.is_ascii_digit()), |s: Span<'a>| s.parse::<usize>().unwrap()))
        )), |(f, p, t)| Pattern::Repeat(Box::new(p), f, t)),
        map(delimited(tuple((tag("["), empty0)), |i| parse_ndl_pattern(i, true, true), tuple((empty0, tag("]")))), |p| Pattern::Optional(Box::new(p))),
        delimited(tuple((tag("("), empty0)), |i| parse_ndl_pattern(i, true, true), tuple((empty0, tag(")")))),
        map(one_of("dlLaAsq"), Pattern::Symbol),
        value(Pattern::Identifier, tag("<ident>")),
        value(Pattern::Type, tag("<type>")),
        value(Pattern::Expr, tag("<expr>"))
    ))(text);
}

impl std::str::FromStr for Pattern{
    type Err = String;

    fn from_str(string: &str) -> Result<Pattern, Self::Err>{
        return Ok(parse_ndl_pattern(Span::new(string), true, true).unwrap().1);
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::iter::FromIterator;
    
    use crate::context::standard_ctx;
    use crate::parser::{Span, PResult};
    use crate::patterns::Pattern;

    fn ok_result<T>(res: PResult<'_, T>) -> bool {
        res.is_ok() && res.unwrap().0.is_empty()
    }

    #[test]
    fn basic_patterns() {
        let ctx = standard_ctx();

        let u_pattern = Pattern::Str("test".into());

        assert!(u_pattern.extract(Span::new("utest"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.extract(Span::new("utest"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("test1"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.extract(Span::new("utest"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("test1"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("*test1"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("*"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("1"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("2"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("*"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("1"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("2"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::And(vec!(
            Pattern::Or(vec!(
                Pattern::Str(".".into()),
                Pattern::Str("#".into())
            )),
            Pattern::Str("test".into()),
            Pattern::Or(vec!(
                Pattern::Str("1".into()),
                Pattern::Str("2".into())
            ))
        ));
        
        assert!(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new(".test"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("#test"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("test1"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new("test2"), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("#test1"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("#test2"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new(".test1"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new(".test2"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("test".into())),
            Some(1),
            Some(3)
        );
        
        assert!(u_pattern.extract(Span::new("utest"), &ctx, &RefCell::default()).is_err());
        assert!(u_pattern.extract(Span::new(""), &ctx, &RefCell::default()).is_err());
        assert!(ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("testtest"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("testtesttest"), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new("testtesttesttest"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            Some(2)
        );
        
        assert!(!ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new(""), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("a"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aa"), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new("aaa"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            Some(2),
            None
        );
        
        assert!(!ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new(""), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new("a"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaaaa"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            None
        );
        
        assert!(!ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new(""), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("a"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaaa"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("aaaaaa"), &ctx, &RefCell::default())));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Optional(
                Box::new(Pattern::Str("?".into()))
            )
        ));
        
        assert!(!ok_result(u_pattern.extract(Span::new("utest"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract(Span::new("test?"), &ctx, &RefCell::default())));
    }

    #[test]
    fn basic_parsing(){
        let pattern: Pattern = "\"hello\"".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Str("hello".into()));

        let pattern: Pattern = "[a-z]".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Range('a', 'z'));

        let pattern: Pattern = "[\"Test\"]".parse().expect("Error while parsing pattern");

        assert_eq!(pattern, Pattern::Optional(Box::new(Pattern::Str("Test".into()))));

        let pattern: Pattern = "[a-z] | [0-9]".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Or(vec!(Pattern::Range('a', 'z'), Pattern::Range('0', '9'))));

        let pattern: Pattern = "[a-z] [0-9]".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::And(vec!(Pattern::Range('a', 'z'), Pattern::Range('0', '9'))));

        let pattern: Pattern = "Arg([a-z], l)".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Arg(Box::new(Pattern::Range('a', 'z')), "l".into()));
    }

    #[test]
    fn arg_markers(){
        let ctx = standard_ctx();

        let u_pattern = Pattern::And(vec!(
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
        ));

        assert_eq!(u_pattern.extract("125".into(), &ctx, &RefCell::default()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("")),
            ("Int".into(), vec!("125")),
        )));

        assert_eq!(u_pattern.extract("0.056".into(), &ctx, &RefCell::default()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("")),
            ("Int".into(), vec!("0")),
            ("Dec".into(), vec!("056")),
        )));

        assert_eq!(u_pattern.extract("-13.26".into(), &ctx, &RefCell::default()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("-")),
            ("Int".into(), vec!("13")),
            ("Dec".into(), vec!("26")),
        )));

        assert!(!ok_result(u_pattern.extract(Span::new("+100"), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new("123."), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract(Span::new("test"), &ctx, &RefCell::default())));
    }

    #[test]
    fn high_level_pattern_parsing() {
        let pattern: Pattern = "<ident>".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Identifier);

        let pattern: Pattern = "<type>".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Type);

        let pattern: Pattern = "<expr>".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Expr);
    }

    #[test]
    fn high_level_patterns() {
        let ctx = standard_ctx();

        let pattern: Pattern = "<ident>".parse().expect("Error while parsing pattern");
        
        assert!(ok_result(pattern.extract("test".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("test2".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("test_3".into(), &ctx, &RefCell::default())));
        assert!(pattern.extract("3test".into(), &ctx, &RefCell::default()).is_err());

        let pattern: Pattern = "<type>".parse().expect("Error while parsing pattern");
        
        assert!(ok_result(pattern.extract("Int".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("'Template".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("@Bool".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("(Bool, &String)".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("(Bool, Int) => String".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("Array<Int, 'T>".into(), &ctx, &RefCell::default())));
        assert!(pattern.extract("Test".into(), &ctx, &RefCell::default()).is_err());
        assert!(pattern.extract("+++".into(), &ctx, &RefCell::default()).is_err());

        let pattern: Pattern = "<expr>".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Expr);

        assert!(ok_result(pattern.extract("5".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("true".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("6 + 2".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("(6 + 2, true, false + \"Test\")".into(), &ctx, &RefCell::default())));
        assert!(ok_result(pattern.extract("a * 6 + 5 - \"Test\"".into(), &ctx, &RefCell::default())));
        assert!(pattern.extract("5 ++ true".into(), &ctx, &RefCell::default()).unwrap().0.len() != 0);
        assert!(pattern.extract("+8u76tt".into(), &ctx, &RefCell::default()).is_err());
    }

    #[test]
    fn number_pattern() {
        let ctx = standard_ctx();

        let str_pattern = "Arg([\"-\"], Sign) Arg(1{d}, Int) [\".\" Arg(1{d}, Dec)]".parse::<Pattern>().unwrap();

        let u_pattern = Pattern::And(vec!(
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
        ));

        assert_eq!(u_pattern, str_pattern);

        assert!(ok_result(u_pattern.extract("13".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("-156".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("0156".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("15.56".into(), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract("15.".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("-56.176".into(), &ctx, &RefCell::default())));
    }

    #[test]
    fn pattern_grouping() {
        let ctx = standard_ctx();

        let str_pattern = "(l d) | (d d)".parse::<Pattern>().unwrap();

        let u_pattern = Pattern::Or(vec!(
            Pattern::And(vec!(
                Pattern::Symbol('l'),
                Pattern::Symbol('d')
            )),

            Pattern::And(vec!(
                Pattern::Symbol('d'),
                Pattern::Symbol('d')
            ))
        ));

        assert_eq!(u_pattern, str_pattern);

        assert!(ok_result(u_pattern.extract("k1".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("90".into(), &ctx, &RefCell::default())));
        assert!(ok_result(u_pattern.extract("b2".into(), &ctx, &RefCell::default())));
        assert!(!ok_result(u_pattern.extract("yy".into(), &ctx, &RefCell::default())));
    }
}