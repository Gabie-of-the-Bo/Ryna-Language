Let's create arguably one of the most useful constructs when working with iterable structures:
list comprehensions. This can be done thanks to Nessa's syntax definition features.

## What is *list comprehension*?

List comprehensions are a syntax option in some languages that allow the programmer to
**transform** the values of a list or even filter them without having to create an explicit for loop and
a temporary variable. In this case, we will not deal with filtering the list.

## Creating the syntax

The syntax for list comprehensions then to be something like this:

```
[expr for elem in container]
```

This is problematic for us because Nessa is a strongly typed language. We will have to adapt it like this:

```
[expr for elem: ElemType in container]
```

This can be expressed in *NDL* like this:

```
"[" 
    [s] Arg(<expr>, map) [s]                                        // Map expression
    "for" [s] Arg(<ident>, it) [s] ":" [s] Arg(<type>, type) [s]    // Element
    "in" [s] Arg(<expr>, container) [s]                             // Container
"]"
```

Note that you need `Arg`s to mark the variables that we will use inside the body.

## Generating the body

Internally, a list comprehension is just a **for loop** with a transformation function. There are multiple ways to do this, but
this is one of them:

```
syntax list_comprehension from [...] {
    {#let res = arr<} {$type} {#>();\n}
    {#let func = (} {$it} {#: } {$type} {#) -> } {$type} {# } {$map}{#;\n\n}
    {#for _it_ in }{$container}{# \{\n}
        {#  res.push(func(*_it_));\n}
    {#\}\n\n}
    {#return *res;}
}
```

## Example

Making use of an `Array` initialization syntax, you can build the following code:

```
let array = [i * 2 for i: Int in <Int>[1, 2, 3, 4, 5]]; // [2, 4, 6, 8, 10]
```

This would compile to:

```
let array = (() = {
    let res = arr<Int>();
    let func = (i: Int) { i * 2 };

    for _it_ in <Int>[1, 2, 3, 4, 5] {
        res.push(func(*_it_));
    }

    return *res;
})();
```

In this example `<Int>[1, 2, 3, 4, 5]` would also be recursively compiled into a new expression based on a lambda expression, but
the exact compilation is not important.