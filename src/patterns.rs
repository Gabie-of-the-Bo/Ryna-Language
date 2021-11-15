use std::collections::HashMap;

use nom::{
    IResult,
    combinator::{map, opt, value},
    bytes::complete::tag,
    character::complete::satisfy,
};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

lazy_static! {
    static ref VALID_SYMBOLS: Vec<char> = "dlLaAsq".chars().collect::<Vec<_>>();
}

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
    pub fn matches<'a>(&self, text: &'a str) -> IResult<&'a str, ()> {
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

    pub fn extract<'a>(&self, text: &'a str) -> IResult<&'a str, HashMap<String, Vec<&'a str>>> {
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

fn enclosing_split(string: &[char], s: char) -> Vec<&[char]> {
    let mut depth_par: i32 = 0;
    let mut depth_bra: i32 = 0;
    let mut depth_cur: i32 = 0;
    let mut depth_str: i32 = 0;

    let mut reps = false;
    let mut idx = 0;
    let mut indices = vec!();

    for (i, c) in string.iter().enumerate() {
        if string[idx] != *c {
            reps = false;
        }

        match *c {
            '\'' => depth_str = 1 - depth_str,

            _ if depth_str == 0 => match *c {
                '(' => depth_par += 1,
                ')' => depth_par -= 1,
                '[' => depth_bra += 1,
                ']' => depth_bra -= 1,
                '{' => depth_cur += 1,
                '}' => depth_cur -= 1,
    
                cr if !reps && cr == s && depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0 => {
                    indices.push(&string[idx..i]); 
                    idx = i + 1;
                    reps = true;
                },

                _ => { }
            }

            _ => { }
        }
    }

    indices.push(&string[idx..]); 

    return indices;
}

fn is_enclosed_from_start(string: &[char]) -> bool {
    let mut depth_par: i32 = 0;
    let mut depth_bra: i32 = 0;
    let mut depth_cur: i32 = 0;
    let mut depth_str: i32 = 0;

    for (i, c) in string.iter().enumerate() {
        match *c {
            '\'' => depth_str = 1 - depth_str,

            _ if depth_str == 0 => match *c {
                '(' => depth_par += 1,
                ')' => depth_par -= 1,
                '[' => depth_bra += 1,
                ']' => depth_bra -= 1,
                '{' => depth_cur += 1,
                '}' => depth_cur -= 1,

                _ => { }
            }

            _ => { }
        }

        // Return false when there is nothing enclosing the current position
        if i < string.len() - 1 && depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0 {
            return false;
        }
    }

    return depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0;
}

fn parse_pattern(mut string: &[char]) -> Result<Pattern, String>{
    if string.len() == 0{
        return Err("Unable to parse empty string".into());
    }

    // Trim slice
    while string[0].is_whitespace() { string = &string[1..] }
    while string[string.len() - 1].is_whitespace() { string = &string[..string.len() - 1] }

    // Symbol pattern
    if string.len() == 1 && VALID_SYMBOLS.contains(&string[0]) {
        return Ok(Pattern::Symbol(string[0]));
    }

    // Range pattern
    if string.len() == 5 && string[0] == '[' && string[2] == '-' && string[4] == ']' && string[1] != string[3] {
        return Ok(Pattern::Range(string[1], string[3]));
    }

    // String pattern
    if string.len() > 2 && string[0] == '\'' && string[string.len() - 1] == '\'' && string.iter().filter(|&i| *i == '\'').count() == 2 {
        return Ok(Pattern::Str(string[1..(string.len() - 1)].iter().collect()));
    } 

    // Pattern enclosed in parentheses
    if string[0] == '(' && string[string.len() - 1] == ')' && is_enclosed_from_start(string) {
        return parse_pattern(&string[1..(string.len() - 1)]);
    }

    // Optional pattern
    if string[0] == '[' && string[string.len() - 1] == ']' && is_enclosed_from_start(string) {
        return Ok(Pattern::Optional(Box::new(parse_pattern(&string[1..(string.len() - 1)]).unwrap())));
    }

    // Repetition pattern
    let first_dig = string.iter().enumerate().filter(|(_, c)| !c.is_digit(10)).map(|(i, _)| i).next().unwrap_or(0);
    let last_dig = string.iter().enumerate().rev().filter(|(_, c)| !c.is_digit(10)).map(|(i, _)| i).next().unwrap_or(string.len() - 1);

    let r_string = &string[first_dig..=last_dig];

    if r_string[0] == '{' && r_string[r_string.len() - 1] == '}' && is_enclosed_from_start(r_string) {
        let mut start = None;
        let mut end = None;

        // Start and end iteration limits
        if first_dig > 0 {
            start = Some(string[..first_dig].iter().collect::<String>().parse::<usize>().unwrap());
        }

        if last_dig < string.len() - 1 {
            end = Some(string[(last_dig + 1)..].iter().collect::<String>().parse::<usize>().unwrap());
        }
        
        return Ok(Pattern::Repeat(Box::new(parse_pattern(&r_string[1..(r_string.len() - 1)]).unwrap()), start, end));
    }

    // Argument marker
    if string.len() > 5 && string[0] == 'A' && string[1] == 'r' && string[2] == 'g' && string[3] == '(' && string[string.len() - 1] == ')' {
        let inner = &string[3..];

        if is_enclosed_from_start(inner){
            let args = enclosing_split(&inner[1..(inner.len() - 1)], ',');

            if args.len() == 2 {
                let name = args[1].iter().collect::<String>();
                let pattern = parse_pattern(args[0]).unwrap();

                return Ok(Pattern::Arg(Box::new(pattern), name.trim().into()));
            }

            return Err(format!("Invalid number of arguments in Argument Marker (expected 2, got {})", args.len()));
        }
    }

    // Alternative pattern
    let alternatives = enclosing_split(string, '|');

    if alternatives.len() > 1 {
        return Ok(Pattern::Or(alternatives.into_iter().map(parse_pattern).map(Result::unwrap).collect::<Vec<_>>()));
    }

    // Composition pattern
    let patterns = enclosing_split(string, ' ');

    if patterns.len() > 1 {
        return Ok(Pattern::And(patterns.into_iter().map(parse_pattern).map(Result::unwrap).collect::<Vec<_>>()));
    }

    return Err(format!("Unable to parse pattern \"{}\"", string.iter().collect::<String>()));
}

impl std::str::FromStr for Pattern{
    type Err = String;

    fn from_str(string: &str) -> Result<Pattern, Self::Err>{
        return parse_pattern(&string.chars().collect::<Vec<_>>());
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

    use crate::patterns::Pattern;

    fn ok_result<'a, T>(res: IResult<&'a str, T>) -> bool {
        return res.is_ok() && res.unwrap().0.is_empty();
    }

    #[test]
    fn basic_patterns() {
        let u_pattern = Pattern::Str("test".into());

        assert!(u_pattern.matches("utest").is_err());
        assert!(ok_result(u_pattern.matches("test")));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.matches("utest").is_err());
        assert!(u_pattern.matches("test").is_err());
        assert!(ok_result(u_pattern.matches("test1")));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        assert!(u_pattern.matches("utest").is_err());
        assert!(u_pattern.matches("test").is_err());
        assert!(u_pattern.matches("test1").is_err());
        assert!(ok_result(u_pattern.matches("*test1")));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.matches("test").is_err());
        assert!(u_pattern.matches("*").is_err());
        assert!(ok_result(u_pattern.matches("1")));
        assert!(ok_result(u_pattern.matches("2")));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        assert!(u_pattern.matches("test").is_err());
        assert!(ok_result(u_pattern.matches("*")));
        assert!(ok_result(u_pattern.matches("1")));
        assert!(ok_result(u_pattern.matches("2")));

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
        
        assert!(u_pattern.matches("test").is_err());
        assert!(u_pattern.matches(".test").is_err());
        assert!(u_pattern.matches("#test").is_err());
        assert!(u_pattern.matches("test1").is_err());
        assert!(u_pattern.matches("test2").is_err());
        assert!(ok_result(u_pattern.matches("#test1")));
        assert!(ok_result(u_pattern.matches("#test2")));
        assert!(ok_result(u_pattern.matches(".test1")));
        assert!(ok_result(u_pattern.matches(".test2")));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("test".into())),
            Some(1),
            Some(3)
        );
        
        assert!(u_pattern.matches("utest").is_err());
        assert!(u_pattern.matches("").is_err());
        assert!(ok_result(u_pattern.matches("test")));
        assert!(ok_result(u_pattern.matches("testtest")));
        assert!(ok_result(u_pattern.matches("testtesttest")));
        assert!(!ok_result(u_pattern.matches("testtesttesttest")));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            Some(2)
        );
        
        assert!(!ok_result(u_pattern.matches("test")));
        assert!(ok_result(u_pattern.matches("")));
        assert!(ok_result(u_pattern.matches("a")));
        assert!(ok_result(u_pattern.matches("aa")));
        assert!(!ok_result(u_pattern.matches("aaa")));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            Some(2),
            None
        );
        
        assert!(!ok_result(u_pattern.matches("test")));
        assert!(!ok_result(u_pattern.matches("")));
        assert!(!ok_result(u_pattern.matches("a")));
        assert!(ok_result(u_pattern.matches("aa")));
        assert!(ok_result(u_pattern.matches("aaa")));
        assert!(ok_result(u_pattern.matches("aaaa")));
        assert!(ok_result(u_pattern.matches("aaaaa")));
        assert!(ok_result(u_pattern.matches("aaaaaa")));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            None
        );
        
        assert!(!ok_result(u_pattern.matches("test")));
        assert!(ok_result(u_pattern.matches("")));
        assert!(ok_result(u_pattern.matches("a")));
        assert!(ok_result(u_pattern.matches("aa")));
        assert!(ok_result(u_pattern.matches("aaa")));
        assert!(ok_result(u_pattern.matches("aaaa")));
        assert!(ok_result(u_pattern.matches("aaaaa")));
        assert!(ok_result(u_pattern.matches("aaaaaa")));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Optional(
                Box::new(Pattern::Str("?".into()))
            )
        ));
        
        assert!(!ok_result(u_pattern.matches("utest")));
        assert!(ok_result(u_pattern.matches("test")));
        assert!(ok_result(u_pattern.matches("test?")));
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

        assert!(!ok_result(u_pattern.extract("+100".into())));
        assert!(!ok_result(u_pattern.extract("123.".into())));
        assert!(!ok_result(u_pattern.extract("test".into())));
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