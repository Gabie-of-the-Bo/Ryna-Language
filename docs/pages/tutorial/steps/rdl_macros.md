The final type of macro that we are going to look at is one that allows you to create custom RDL patterns. These are called
**RDL macros** and work like this: 

## Syntax

A RDL macro can be created in Ryna using this syntax:

```
syntax rdl macro_name from RDL_Pattern {
    [...]
}
```

This macro would have to create a string that can be parsed as an RDL pattern. This syntax can then be used safely inside any
implicit class syntax or macro.

This would be an example that transforms a pattern into a series of that pattern separated by commas (again, note that we need to
escape the closing brace):

```
syntax rdl comma_separated from "#" s Arg(<rdl>, inner) s "#" {
    $inner { ", " $inner \}
}

// Usage example
class Ints {
    // This pattern es equivalent to "[" Arg(1{d}, ints) {", " Arg(1{d}, ints)} "]"
    syntax from "[" # Arg(1{d}, ints) # "]";

    ints: Array<Int>;
}

let a: Ints = [1, 2, 3];
```