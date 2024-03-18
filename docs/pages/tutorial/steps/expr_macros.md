Sometimes you want to use macros to create small code transformations that result in an expression (ie. not an **if-else**, **for**, **while** or definition). 
For this purpose, you can use the second type of macro that Nessa has: **expression macros**. Let"s see how they work.

## Syntax

An expression macro can be created in Nessa using this syntax:

```
syntax expr macro_name from NDL_Pattern {
    [...]
}
```

You can see that the syntax is almost the same, but contains the `expr` keyword. The other thing that changes is how they are compiled
internally:

```
syntax expr double_integer from "Dbl" Arg(1{d}, n) {
    $n + $n
}

// This macro ...
print(Dbl6);

// ... is compiled to this
print(6 + 6);
```

You might be worrying about operator precedence by looking at this, but the interpreter ensures that the expression inside
the macro is kept as-is, so you could use it like this:

```
// This macro ...
print(2 * Dbl6);

// ... is compiled to this ...
print(2 * (6 + 6));

// ... not to this
print(2 * 6 + 6);
```