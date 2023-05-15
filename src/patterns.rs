use std::collections::{ HashMap, HashSet };

use nom::{
    IResult,
    combinator::{map, opt, value},
    bytes::complete::{take_while, take_while1, tag},
    sequence::{tuple, delimited, separated_pair},
    character::complete::{multispace0, multispace1, one_of, satisfy},
    branch::alt,
    multi::separated_list1
};

use crate::parser::Span;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern{
    // Markers
    Arg(Box<Pattern>, String),

    // Tail patterns
    Str(String),
    Range(char, char),
    Symbol(char), // Digit (d), Letter (l/L), Alphabetic (a), Alphanumeric (A), Space (s), quote (q)

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

    pub fn matches<'a>(&self, text: Span<'a>) -> IResult<Span<'a>, ()> {
        return match self {
            Pattern::Symbol('d') => value((), satisfy(|c| c.is_digit(10)))(text),
            Pattern::Symbol('l') => value((), satisfy(|c| c.is_lowercase()))(text),
            Pattern::Symbol('L') => value((), satisfy(|c| c.is_uppercase()))(text),
            Pattern::Symbol('a') => value((), satisfy(|c| c.is_alphabetic()))(text),
            Pattern::Symbol('A') => value((), satisfy(|c| c.is_alphanumeric()))(text),
            Pattern::Symbol('s') => value((), satisfy(|c| c.is_whitespace()))(text),
            Pattern::Symbol('q') => value((), satisfy(|c| c == '\''))(text),
            Pattern::Symbol(_) => unreachable!(),

            Pattern::Range(a, b) => value((), satisfy(|c| c >= *a && c <= *b))(text),

            Pattern::Str(s) => value((), tag(s.as_str()))(text),

            Pattern::And(patterns) => {
                let mut input = text;

                for p in patterns {
                    if let Ok((i, _)) = p.matches(input) {
                        input = i;

                    } else {
                        return Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
                    }
                }
                
                Ok((input, ()))
            },

            Pattern::Or(patterns) => {
                for p in patterns {
                    if let Ok((i, o)) = p.matches(text) {
                        return Ok((i, o));
                    }
                }
                
                Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
            },
            
            Pattern::Repeat(p, from, to) => {
                let mut input = text;

                // Minimum
                if let Some(f) = from {
                    for _ in 0..*f {
                        if let Ok((i, _)) = p.matches(input) {
                            input = i;
    
                        } else {
                            return Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
                        }
                    }
                }

                // Maximum
                if let Some(t) = to {
                    for _ in 0..(t - from.unwrap_or(0)) {
                        if let Ok((i, _)) = p.matches(input) {
                            input = i;
    
                        } else {
                            break;
                        }
                    }

                } else {
                    loop {
                        if let Ok((i, _)) = p.matches(input) {
                            input = i;
    
                        } else {
                            break;
                        }
                    }
                }

                Ok((input, ()))
            }

            Pattern::Optional(p) => value((), opt(|i| p.matches(i)))(text),
            Pattern::Arg(p, _) => p.matches(text)
        };
    }

    pub fn extract<'a>(&self, text: Span<'a>) -> IResult<Span<'a>, HashMap<String, Vec<&'a str>>> {
        fn merge<'a>(a: &mut HashMap<String, Vec<&'a str>>, b: HashMap<String, Vec<&'a str>>) {
            for (k, v) in b.into_iter() {
                a.entry(k).or_default().extend(v);
            }
        }

        return match self {
            Pattern::Symbol('d') => value(HashMap::new(), satisfy(|c| c.is_digit(10)))(text),
            Pattern::Symbol('l') => value(HashMap::new(), satisfy(|c| c.is_lowercase()))(text),
            Pattern::Symbol('L') => value(HashMap::new(), satisfy(|c| c.is_uppercase()))(text),
            Pattern::Symbol('a') => value(HashMap::new(), satisfy(|c| c.is_alphabetic()))(text),
            Pattern::Symbol('A') => value(HashMap::new(), satisfy(|c| c.is_alphanumeric()))(text),
            Pattern::Symbol('s') => value(HashMap::new(), satisfy(|c| c.is_whitespace()))(text),
            Pattern::Symbol('q') => value(HashMap::new(), satisfy(|c| c == '\''))(text),
            Pattern::Symbol(_) => unreachable!(),

            Pattern::Range(a, b) => value(HashMap::new(), satisfy(|c| c >= *a && c <= *b))(text),

            Pattern::Str(s) => value(HashMap::new(), tag(s.as_str()))(text),

            Pattern::And(patterns) => {
                let mut res = HashMap::new();
                let mut input = text;

                for p in patterns {
                    if let Ok((i, o)) = p.extract(input) {
                        input = i;
                        merge(&mut res, o);

                    } else {
                        return Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
                    }
                }
                
                Ok((input, res))
            },

            Pattern::Or(patterns) => {
                for p in patterns {
                    if let Ok((i, o)) = p.extract(text) {
                        return Ok((i, o));
                    }
                }
                
                Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
            },
            
            Pattern::Repeat(p, from, to) => {
                let mut res = HashMap::new();
                let mut input = text;

                // Minimum
                if let Some(f) = from {
                    for _ in 0..*f {
                        if let Ok((i, o)) = p.extract(input) {
                            input = i;
                            merge(&mut res, o);
    
                        } else {
                            return Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::Alt)))
                        }
                    }
                }

                // Maximum
                if let Some(t) = to {
                    for _ in 0..(t - from.unwrap_or(0)) {
                        if let Ok((i, o)) = p.extract(input) {
                            input = i;
                            merge(&mut res, o);
    
                        } else {
                            break;
                        }
                    }

                } else {
                    loop {
                        if let Ok((i, o)) = p.extract(input) {
                            input = i;
                            merge(&mut res, o);
    
                        } else {
                            break;
                        }
                    }
                }

                Ok((input, res))
            }

            Pattern::Optional(p) => map(opt(|i| p.extract(i)), Option::unwrap_or_default)(text),
            Pattern::Arg(p, k) => {
                let (i, mut o) = p.extract(text)?;

                o.entry(k.clone()).or_default().push(&text[..(text.len() - i.len())]);

                Ok((i, o))
            }
        };
    }
}

fn parse_and<'a>(text: Span<'a>, and: bool) -> IResult<Span<'a>, Pattern> {
    return if and {
        map(
            separated_list1(multispace1, |i| parse_ndl_pattern(i, false, false)), 
            |v| if v.len() > 1 { Pattern::And(v) } else { v[0].clone() }
        )(text)
        
    } else {
        Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::SeparatedList)))
    }
}

fn parse_or<'a>(text: Span<'a>, or: bool) -> IResult<Span<'a>, Pattern> {
    return if or {
        return map(
            separated_list1(tuple((multispace0, tag("|"), multispace0)), |i| parse_ndl_pattern(i, false, true)), 
            |v| if v.len() > 1 { Pattern::Or(v) } else { v[0].clone() }
        )(text)
        
    } else {
        Err(nom::Err::Error(nom::error::Error::new(text, nom::error::ErrorKind::SeparatedList)))
    }
}

pub fn parse_ndl_pattern<'a>(text: Span<'a>, or: bool, and: bool) -> IResult<Span<'a>, Pattern> {
    return alt((
        |i| parse_or(i, or),
        |i| parse_and(i, and),
        map(delimited(tag("["), separated_pair(satisfy(|c| c != '\''), tag("-"), satisfy(|c| c != '\'')), tag("]")), |(a, b)| Pattern::Range(a, b)),
        map(delimited(tag("'"), take_while(|c| c != '\''), tag("'")), |s: Span<'a>| Pattern::Str(s.to_string())),
        map(delimited(
            tuple((tag("Arg("), multispace0)),
            separated_pair(|i| parse_ndl_pattern(i, true, true), tuple((multispace0, tag(","), multispace0)), take_while1(|c| c != ')')),
            tuple((multispace0, tag(")")))
        ), |(p, n)| Pattern::Arg(Box::new(p), n.to_string())),
        map(tuple((
            opt(map(take_while1(|c: char| c.is_digit(10)), |s: Span<'a>| s.parse::<usize>().unwrap())),
            delimited(tuple((tag("{"), multispace0)), |i| parse_ndl_pattern(i, true, true), tuple((multispace0, tag("}")))),
            opt(map(take_while1(|c: char| c.is_digit(10)), |s: Span<'a>| s.parse::<usize>().unwrap()))
        )), |(f, p, t)| Pattern::Repeat(Box::new(p), f, t)),
        map(delimited(tuple((tag("["), multispace0)), |i| parse_ndl_pattern(i, true, true), tuple((multispace0, tag("]")))), |p| Pattern::Optional(Box::new(p))),
        delimited(tuple((tag("("), multispace0)), |i| parse_ndl_pattern(i, true, true), tuple((multispace0, tag(")")))),
        map(one_of("dlLaAsq"), Pattern::Symbol)
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
    use std::collections::HashMap;
    use std::iter::FromIterator;
    
    use nom::IResult;

    use crate::parser::Span;
    use crate::patterns::Pattern;

    fn ok_result<'a, T>(res: IResult<Span<'a>, T>) -> bool {
        return res.is_ok() && res.unwrap().0.is_empty();
    }

    #[test]
    fn basic_patterns() {
        let u_pattern = Pattern::Str("test".into());

        assert!(u_pattern.matches(Span::new("utest")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("test"))));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.matches(Span::new("utest")).is_err());
        assert!(u_pattern.matches(Span::new("test")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("test1"))));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.matches(Span::new("utest")).is_err());
        assert!(u_pattern.matches(Span::new("test")).is_err());
        assert!(u_pattern.matches(Span::new("test1")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("*test1"))));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.matches(Span::new("test")).is_err());
        assert!(u_pattern.matches(Span::new("*")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("1"))));
        assert!(ok_result(u_pattern.matches(Span::new("2"))));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.matches(Span::new("test")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("*"))));
        assert!(ok_result(u_pattern.matches(Span::new("1"))));
        assert!(ok_result(u_pattern.matches(Span::new("2"))));

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
        
        assert!(u_pattern.matches(Span::new("test")).is_err());
        assert!(u_pattern.matches(Span::new(".test")).is_err());
        assert!(u_pattern.matches(Span::new("#test")).is_err());
        assert!(u_pattern.matches(Span::new("test1")).is_err());
        assert!(u_pattern.matches(Span::new("test2")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("#test1"))));
        assert!(ok_result(u_pattern.matches(Span::new("#test2"))));
        assert!(ok_result(u_pattern.matches(Span::new(".test1"))));
        assert!(ok_result(u_pattern.matches(Span::new(".test2"))));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("test".into())),
            Some(1),
            Some(3)
        );
        
        assert!(u_pattern.matches(Span::new("utest")).is_err());
        assert!(u_pattern.matches(Span::new("")).is_err());
        assert!(ok_result(u_pattern.matches(Span::new("test"))));
        assert!(ok_result(u_pattern.matches(Span::new("testtest"))));
        assert!(ok_result(u_pattern.matches(Span::new("testtesttest"))));
        assert!(!ok_result(u_pattern.matches(Span::new("testtesttesttest"))));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            Some(2)
        );
        
        assert!(!ok_result(u_pattern.matches(Span::new("test"))));
        assert!(ok_result(u_pattern.matches(Span::new(""))));
        assert!(ok_result(u_pattern.matches(Span::new("a"))));
        assert!(ok_result(u_pattern.matches(Span::new("aa"))));
        assert!(!ok_result(u_pattern.matches(Span::new("aaa"))));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            Some(2),
            None
        );
        
        assert!(!ok_result(u_pattern.matches(Span::new("test"))));
        assert!(!ok_result(u_pattern.matches(Span::new(""))));
        assert!(!ok_result(u_pattern.matches(Span::new("a"))));
        assert!(ok_result(u_pattern.matches(Span::new("aa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaaaa"))));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            None
        );
        
        assert!(!ok_result(u_pattern.matches(Span::new("test"))));
        assert!(ok_result(u_pattern.matches(Span::new(""))));
        assert!(ok_result(u_pattern.matches(Span::new("a"))));
        assert!(ok_result(u_pattern.matches(Span::new("aa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaaa"))));
        assert!(ok_result(u_pattern.matches(Span::new("aaaaaa"))));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Optional(
                Box::new(Pattern::Str("?".into()))
            )
        ));
        
        assert!(!ok_result(u_pattern.matches(Span::new("utest"))));
        assert!(ok_result(u_pattern.matches(Span::new("test"))));
        assert!(ok_result(u_pattern.matches(Span::new("test?"))));
    }

    #[test]
    fn basic_parsing(){
        let pattern: Pattern = "'hello'".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Str("hello".into()));

        let pattern: Pattern = "[a-z]".parse().expect("Error while parsing pattern");
        
        assert_eq!(pattern, Pattern::Range('a', 'z'));

        let pattern: Pattern = "['Test']".parse().expect("Error while parsing pattern");

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

        assert_eq!(u_pattern.extract("125".into()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("".into())),
            ("Int".into(), vec!("125".into())),
        )));

        assert_eq!(u_pattern.extract("0.056".into()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("".into())),
            ("Int".into(), vec!("0".into())),
            ("Dec".into(), vec!("056".into())),
        )));

        assert_eq!(u_pattern.extract("-13.26".into()).unwrap().1, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("-".into())),
            ("Int".into(), vec!("13".into())),
            ("Dec".into(), vec!("26".into())),
        )));

        assert!(!ok_result(u_pattern.extract(Span::new("+100"))));
        assert!(!ok_result(u_pattern.extract(Span::new("123."))));
        assert!(!ok_result(u_pattern.extract(Span::new("test"))));
    }

    #[test]
    fn number_pattern() {
        let str_pattern = "Arg(['-'], Sign) Arg(1{d}, Int) ['.' Arg(1{d}, Dec)]".parse::<Pattern>().unwrap();

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

        assert!(ok_result(u_pattern.matches("13".into())));
        assert!(ok_result(u_pattern.matches("-156".into())));
        assert!(ok_result(u_pattern.matches("0156".into())));
        assert!(ok_result(u_pattern.matches("15.56".into())));
        assert!(!ok_result(u_pattern.matches("15.".into())));
        assert!(ok_result(u_pattern.matches("-56.176".into())));
    }

    #[test]
    fn pattern_grouping() {
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

        assert!(ok_result(u_pattern.matches("k1".into())));
        assert!(ok_result(u_pattern.matches("90".into())));
        assert!(ok_result(u_pattern.matches("b2".into())));
        assert!(!ok_result(u_pattern.matches("yy".into())));
    }
}