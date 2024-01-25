The final feature that we will be taking a look at is one is arguably one of the most unique: **macros**.
Be aware that a macro in Nessa is not the same as a macro in other languages such as C, C++ or even Rust, even though they are 
close to Rust's. The main difference between Rust's macros and Nessa's is that there is no restriction on the syntax that you can match.
Let's take a look at how they work by diving into the first kind of macros Nessa has: **function macros**.

## Syntax

A function macro can be created in Nessa using this syntax:

```
syntax macro_name from NDL_Pattern {
    [...]
}

// This is also allowed
syntax fn macro_name from NDL_Pattern {
    [...]
}
```

When you define a macro of any kind you can "invoke" it by using the syntax anywhere in your code where an expression is legal. Just be aware that 
macros are internally compiled to a different representation. In the case of function macros, they are converted to lambda expressions:

```
syntax macro_name from 'Macro' {
    {#return 5;}
}

// This macro ...
print(Macro);

// ... is compiled to this
print((() { return 5; })());
```

You may have noticed that the syntax inside a macro body is not the same as a function. This is because you can only write
text-based macros with macros that generate a string from the `Arg` patterns found in the input.

## Macro patterns

> ***Note:*** This is subject to change. Once compile-time code execution is implemented this *might* become obsolete

These are the patterns that you can use inside a macro's body:

| Pattern  | Syntax               | Description                                                          |
| -------- | -------------------- | -------------------------------------------------------------------- |
| Text     | `{#Text}`            | Pastes `Text` into the macro result                                  |
| Variable | `{$Var}`             | Pastes the value of the `Arg` named `Var` into the macro result      |
| If       | `{&Var}{Pat1}{Pat2}` | `Pat1` if `Var` has been read from the input, else `Pat2` (optional) |
| Loop     | `{@I in $V}{Pat}`    | `Pat` creating a variable `I` for each element in the variable `V`   |

## Example

Let's see an example where we try to create a macro to initialize an `Array` statically:

```
syntax array_initialization from '<' Arg(<type>, type) '>[' [{Arg(<expr>, elems) ',' {' '}} 
                                 Arg(<expr>, elems)] ']' {
    {#let res = arr<} {$type} {#>(} {#);\n}
    {@i in $elems} {
        {#res.push(} {$i} {#);\n}
    }
    {#return *res;}
}
```

This macro allows you to write code such as this:

```
let array = <Int>[1, 2, 3, 4, 5];
```

Which will then be compiled to this:

```
let array = (() {
    let res = arr<Int>();

    res.push(1);
    res.push(2);
    res.push(3);
    res.push(4);
    res.push(5);

    return *res;
})();
```

As you can imagine, this allows you to create very complex behaviours transparently. You can see more 
complex examples in the *Learn by example* section.