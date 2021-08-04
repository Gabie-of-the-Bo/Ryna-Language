#[macro_use]
extern crate lazy_static;

mod patterns;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::iter::FromIterator;
    
    use crate::patterns::Pattern;

    #[test]
    fn basic_patterns() {
        let u_pattern = Pattern::Str("test".into());
        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("utest".into()), None);
        assert_eq!(pattern.matches("test".into()), Some(4));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("utest".into()), None);
        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("test1".into()), Some(5));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("test".into()),
            Pattern::Str("1".into())
        ));

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("utest".into()), None);
        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("test1".into()), None);
        assert_eq!(pattern.matches("*test1".into()), Some(6));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("*".into()), None);
        assert_eq!(pattern.matches("1".into()), Some(1));
        assert_eq!(pattern.matches("2".into()), Some(1));

        let u_pattern = Pattern::Or(vec!(
            Pattern::Str("*".into()),
            Pattern::Str("1".into()),
            Pattern::Str("2".into())
        ));

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("*".into()), Some(1));
        assert_eq!(pattern.matches("1".into()), Some(1));
        assert_eq!(pattern.matches("2".into()), Some(1));

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

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches(".test".into()), None);
        assert_eq!(pattern.matches("#test".into()), None);
        assert_eq!(pattern.matches("test1".into()), None);
        assert_eq!(pattern.matches("test2".into()), None);
        assert_eq!(pattern.matches("#test1".into()), Some(6));
        assert_eq!(pattern.matches("#test2".into()), Some(6));
        assert_eq!(pattern.matches(".test1".into()), Some(6));
        assert_eq!(pattern.matches(".test2".into()), Some(6));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("test".into())),
            Some(1),
            Some(3)
        );

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("utest".into()), None);
        assert_eq!(pattern.matches("".into()), None);
        assert_eq!(pattern.matches("test".into()), Some(4));
        assert_eq!(pattern.matches("testtest".into()), Some(8));
        assert_eq!(pattern.matches("testtesttest".into()), Some(12));
        assert_eq!(pattern.matches("testtesttesttest".into()), None);

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            Some(2)
        );

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("".into()), Some(0));
        assert_eq!(pattern.matches("a".into()), Some(1));
        assert_eq!(pattern.matches("aa".into()), Some(2));
        assert_eq!(pattern.matches("aaa".into()), None);

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            Some(2),
            None
        );

        let pattern = u_pattern.compile();

        println!("{:?}", pattern);

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("".into()), None);
        assert_eq!(pattern.matches("a".into()), None);
        assert_eq!(pattern.matches("aa".into()), Some(2));
        assert_eq!(pattern.matches("aaa".into()), Some(3));
        assert_eq!(pattern.matches("aaaa".into()), Some(4));
        assert_eq!(pattern.matches("aaaaa".into()), Some(5));
        assert_eq!(pattern.matches("aaaaaa".into()), Some(6));

        let u_pattern = Pattern::Repeat(
            Box::new(Pattern::Str("a".into())),
            None,
            None
        );

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("test".into()), None);
        assert_eq!(pattern.matches("".into()), Some(0));
        assert_eq!(pattern.matches("a".into()), Some(1));
        assert_eq!(pattern.matches("aa".into()), Some(2));
        assert_eq!(pattern.matches("aaa".into()), Some(3));
        assert_eq!(pattern.matches("aaaa".into()), Some(4));
        assert_eq!(pattern.matches("aaaaa".into()), Some(5));
        assert_eq!(pattern.matches("aaaaaa".into()), Some(6));

        let u_pattern = Pattern::And(vec!(
            Pattern::Str("test".into()),
            Pattern::Optional(
                Box::new(Pattern::Str("?".into()))
            )
        ));

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("utest".into()), None);
        assert_eq!(pattern.matches("test".into()), Some(4));
        assert_eq!(pattern.matches("test?".into()), Some(5));
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

        let pattern = u_pattern.compile();

        assert_eq!(pattern.extract("125".into()), Some((3, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("".into())),
            ("Int".into(), vec!("125".into())),
        )))));

        assert_eq!(pattern.extract("0.056".into()), Some((5, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("".into())),
            ("Int".into(), vec!("0".into())),
            ("Dec".into(), vec!("056".into())),
        )))));

        assert_eq!(pattern.extract("-13.26".into()), Some((6, HashMap::from_iter(vec!(
            ("Sign".into(), vec!("-".into())),
            ("Int".into(), vec!("13".into())),
            ("Dec".into(), vec!("26".into())),
        )))));

        assert_eq!(pattern.extract("+100".into()), None);
        assert_eq!(pattern.extract("123.".into()), None);
        assert_eq!(pattern.extract("test".into()), None);
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

        let pattern = u_pattern.compile();

        assert_eq!(pattern.matches("13".into()), Some(2));
        assert_eq!(pattern.matches("-156".into()), Some(4));
        assert_eq!(pattern.matches("0156".into()), Some(4));
        assert_eq!(pattern.matches("15.56".into()), Some(5));
        assert_eq!(pattern.matches("15.".into()), None);
        assert_eq!(pattern.matches("-56.176".into()), Some(7));
    }
}