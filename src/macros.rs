use nom::{sequence::{delimited, tuple}, character::complete::{multispace0, multispace1}, bytes::complete::{tag, take_while1}, combinator::map, branch::alt};

use crate::parser::{many_separated0, Span, PResult, identifier_parser};

pub enum NessaMacro {
    Text(String),
    Var(String),
    Loop(String, String, Vec<NessaMacro>),
    Seq(Vec<NessaMacro>)
}

pub fn parse_text<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    return map(
        delimited(
            tag("{#"),
            take_while1(|i| !"{#}".contains(i)),
            tag("}")
        ),
        |i: Span<'a>| NessaMacro::Text(i.to_string())
    )(input);
}

pub fn parse_var<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    return map(
        delimited(
            tag("{$"),
            delimited(
                multispace0,
                take_while1(|i| !"{#}".contains(i)),
                multispace0
            ),
            tag("}")
        ),
        |i: Span<'a>| NessaMacro::Var(i.to_string())
    )(input);
}

pub fn parse_loop_header<'a>(input: Span<'a>) -> PResult<(String, Span<'a>, Span<'a>, Span<'a>, Span<'a>, String)> {
    return tuple((
        identifier_parser,
        multispace1,
        tag("in"),
        multispace1,
        tag("$"),
        identifier_parser
    ))(input);
}

pub fn parse_loop<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    return map(
        tuple((
            delimited(
                tag("{@"),
                delimited(
                    multispace0,
                    parse_loop_header,
                    multispace0
                ),
                tag("}")
            ),
            multispace0,
            delimited(
                tag("{"),
                parse_nessa_macro_lines,
                tag("}")                
            )
        )),
        |(h, _, b)| NessaMacro::Loop(h.0, h.5.to_string(), b)
    )(input);
}

pub fn parse_nessa_macro_line<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    return alt((
        parse_var,
        parse_text,
        parse_loop
    ))(input);
}

pub fn parse_nessa_macro_lines<'a>(input: Span<'a>) -> PResult<Vec<NessaMacro>> {
    return delimited(
        multispace0,
        many_separated0(multispace0, |input| parse_nessa_macro_line(input)),
        multispace0
    )(input);
}

pub fn parse_nessa_macro<'a>(input: Span<'a>) -> PResult<NessaMacro> {
    return map(
        parse_nessa_macro_lines,
        NessaMacro::Seq
    )(input);
}

mod tests {
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
}