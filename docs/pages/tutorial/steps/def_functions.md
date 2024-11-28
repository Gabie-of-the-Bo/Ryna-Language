Let's now take a look at the syntax and semantics for defining new functions in Ryna.

## Syntax

You can define and use a function using the following syntax:

```
// Definition
fn funtion_name_1(param_1: Type_1, param_2: Type_2, ...) -> Return_type {

    [...]

    return some_expression;
}

// Usage
let variable: Return_type = funtion_name_1(something_1, something_2);
```

Of course, you can have as many `return` statements as you want (or none at all if your function does not return anything), but 
this is a simple example. Now let's modify this code to create a generic function:

```
// Definition
fn<T, G> funtion_name_2(param_1: Type_1, param_2: Type_2, ...) -> Return_type {

    [...]

    let variable_1: 'T = something;
    let variable_2: Array<'G> = something;

    [...]

    return some_expression;
}

// Explicit usage
let variable_1: Return_type = funtion_name_2<Type_3, Type_4>(something_1, something_2);

// Type parameter inference (when unambiguous)
let variable_2: Return_type = funtion_name_2(something_1, something_2);
```

Here you can see that putting `<T, G>` after the `fn` keyword in the definition **defines** the templates `T` and `G` inside the function body.
They **cannot** be used outside the function body.

> ***Note:*** the `<T, G>` syntax might change to `<'T, 'G>` in a future release for ease of highlighting and uniformization.

## Examples

Let's create some common examples to see how functions work in this language. First off, let's create two factorial functions, a recursive and an iterative one:

```
fn factorial_recursive(n: Int) -> Int {
    if n == 1 {
        return 1;
    }

    return n * factorial_recursive(n - 1);
}

fn factorial_iterative(n: Int) -> Int {
    let res = 1;

    while n > 1 {
        res = res * n;
        n = n - 1;
    }

    return res.deref();
}
```

Now let's dive into generics and make a function that counts how many elements in an array are equal to one given as a parameter:

```
fn<T> count(array: &Array<'T>, elem: &'T) -> Int {
    let res = 0;

    for i in array {
        if i == elem {
            res = res + 1;
        }
    }

    return res.deref();
}
```

As a final touch, note that functions and operations can access **global variables**:

```
let global_var = 5;

fn double_global() -> Int {
    return global_var * 2;
}
```