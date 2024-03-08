Before we dive into the syntax extension mechanics that Nessa has, we have to take a look at a sublanguage called 
**Nessa Definition Language** (called *NDL* for short). This sublanguage can be embedded in some parts of your Nessa projects
in order to extend the language. Let's see how it works.

## Almost *regex*

*NDL* is, as the title suggests, almost a flavour of regexes, but more expressive. This bump in expressiveness comes from
its interlinkage with the Nessa parser, which contains many complex routines that **cannot** be expressed in terms of 
regular expressions. 

The basic idea is simple, a **pattern** in NDL is a sequence of other **subpatterns** that have to match a string of characters
in sequence. Some of these patters modify the "flow" of the matching, but it is more or less the same as a regex. For example, the
pattern `d` matches a digit and the pattern `'hello'` matches the text "hello", but you can also have **repeated** matches and **optional** ones.

## Patterns

> ***Note:*** this list will probably be expanded in the future due to new patterns

The following patterns are defined in the current version on *NDL*:

| Name             | Syntax              | Description                                       |
| ---------------- | ------------------- | ------------------------------------------------- |
| Text             | `"Text"`            | Matches the text in double quotes                 |
| Digit            | `d`                 | Matches a digit                                   |
| Lowercase letter | `l`                 | Matches a lowercase letter                        |
| Uppercase letter | `L`                 | Matches an uppercase letter                       |
| Alphabetic       | `a`                 | Matches an alphabetic character                   |
| Alphanumeric     | `A`                 | Matches an alphanumeric character                 |
| Space            | `s`                 | Matches one or more spacing sequences of characters. These include **comments** |
| Range            | `[a-b]`             | Matches a character that is between the two given characters |
| Optional         | `[pattern]`         | Matches `pattern` if possible, succeeds anyways   |
| Or               | `pattern1 | pattern2` | Matches `pattern1` if possible, else matches `pattern2` |
| And              | `pattern1 pattern2` | Matches `pattern1`, then matches `pattern2`       |
| Repeat           | `min{pattern}max`   | Matches `pattern`, between `min` and `max` times  |

Also, *NDL* has support for some *high level patterns* that make use of complex parsing routines. These are the patterns supported as of now:

| Name       | Syntax    | Description           |
| ---------- | --------- | --------------------- |
| Identifier | `<ident>` | Matches an identifier |
| Type       | `<type>`  | Matches a type        |
| Expression | `<expr>`  | Matches an expression |
| NDL        | `<ndl>`   | Matches a NDL pattern |

## Examples

Here are some examples that may help you to visualize how *NDL* works:

```
// Integer syntax
1{d}

// Simple float syntax
["-"] 1{d} ["." 1{d}]

// Variable definition
"let" s <ident> [s] ":" [s] <type> [s] "=" [s] <expr> [s] ";"
```