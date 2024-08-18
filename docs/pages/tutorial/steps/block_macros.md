Another case we need to show is when your desired macro functionality needs to interact with the current context or when
you want to encapsulate multiple lines of code inside a single statement. For this purpose, you can use **block macros**.

## Syntax

A block macro can be created in Ryna using this syntax:

```
syntax block macro_name from NDL_Pattern {
    [...]
}
```

The main difference with expression and function macros is that these can only be used as statements, so they do not count as valid expressions.
Code such as `1 + <some_block_macro>` is not valid Ryna. You need to use expression and function macros for that.

An example of a block macro that creates an early return would be the following (note that we have to escape the closing brace of the if statement in order
for the macro body to be correct):

```
syntax block early_return from "ret#" Arg(1{l}, name) ";" {
    if $name < 5 {
        return false;
    \}
}

// This function ...
fn test(var) -> Bool {
    ret#var;

    return true;
}

// ... compiles to this
fn test(var) -> Bool {
    if var < 5 {
        return false;
    }

    return true;
}
```

Of course, you can generate as many lines of code as you want with a **block macro**, but this is just a simple example of what you can do.