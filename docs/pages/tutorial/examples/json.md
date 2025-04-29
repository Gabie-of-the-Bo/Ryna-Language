An interesting use case for Ryna syntax extension capabilities is defining recursive structures. This can be done using high level patterns such as
`<expr>`, but sometimes you want to restrict the scope of the recursion. Let's take a look at how we would define a simple JSON syntax.

## Defining final values

In order to define a JSON syntax, we need to define **final values** (i.e. values that do not have any kind of recursion). In this case we will use **integers**,
**floats** and **strings**:

```
// Generic JSON class
class Json {
    elem: *;
}

// Json values (marked as intermediate)
syntax int expr json_int from Arg(["-"] 1{d}, digits) {
    Json($digits)
}

syntax int expr json_float from Arg(["-"] 1{d} "." 1{d}, digits) {
    Json($digits)
}

// This is a simplified syntax, but it will serve as an example
syntax int expr json_str from Arg("\"" {l} "\"", string) {
    Json($string)
}
```

Note that we also defined a `Json` class that will wrap every value. This may not be necessary, but it can be useful to add layers of functionality and remove
special cases.

## Recursive values

JSONs have two kinds of recursive structures: **lists** and **maps**. We will leave the second one as an exercise to the reader. A list is, in essence, comma
separated JSONs wrapped in square brackets. For this, we need to define what a **JSON value** is:

```
// We use this to allow inline array definitions. This is part of prelude
syntax array_initialization from "<" [s] Arg(<type>, type) [s] ">" [s] "[" [s] [{Arg(<expr>, elems) "," [s]} [Arg(<expr>, elems)]] [s] "]" {
    let res = arr<$type>();

    @elems.i {
        res.push($elems.i);
    }
    
    return move(res);
}

// Intermediate aggregator syntax (be careful with the order)
syntax int expr json_value from Macro(json_float, value) | Macro(json_int, value) | Macro(json_str, value) | Macro(json_array, value) {
    $value
}

// Array syntax
syntax int expr json_array from "[" [s] [{Macro(json_value, elems) "," [s]} Macro(json_value, elems)] [s] "]" {
    Json(<Json>[@elems.i {$elems.i,}])
}
```

Now, this is a complex-looking syntax, so let's break it down.

The `array_initialization` syntax is just a syntax defined in the *prelude* library that allows the user to write definitions such as this one:

```
let array = <Int>[0, 1, 4, 5];

// Equivalent to
let array = do {
    let res = arr<Int>();

    res.push(0);
    res.push(1);
    res.push(4);
    res.push(5);

    return move(res);
};
```

The `json_value` syntax is just a syntax that tells the interpreter what a JSON value is, just the same way that we did in the tutorial. The order 
of the *Macro* patterns is important because **RDL does not backtrack**.

Finally, the `json_array` syntax is just a syntax that accepts comma separated `json_value` instances wrapped in square brackets. Internally, it uses the
`array_initialization` syntax in order to make it easier to read. You can see here that allowing trailing commas can be very useful and a good practise in general.

## Final structure

Now, let's go for the final assembly. In order to make sure that there are no collisions, it is a good practise to add a small prefix to the syntax, so let's build it
like so:

```
// Final syntax with prefix
syntax expr json from "JSON" s Macro(json_value, value) {
    $value
}
```

This means that we can use it like this:

```
let json = JSON [
    0,
    "test",
    [
        -4.5,
        "testagain"
    ]
]
```

You can see that the syntax can be as nested as you want it to be and, since it has a prefix, it is very difficult for it to collide with any other you define. Of course, this
is a very reduced version of what a JSON really is, but it is enough to understand the concept of recursive syntaxes in Ryna.