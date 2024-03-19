The main problem with regular functions is that they cannot be passed around, since polymorphism makes it really difficult.
In order to have first class functions you have to use **lambda expressions**. They work like this:

## Syntax

The syntax to create lambda expression is as follows:

```
[captures](arg_1: Type_1, arg_2: Type_2, ...) -> ReturnType { 
    [body]    
};

// With inferred return type
[captures](arg_1: Type_1, arg_2: Type_2, ...) { 
    [body]    
};

// Direct return
[captures](arg_1: Type_1, arg_2: Type_2, ...) expression;
```

**Captures** are optional.

## Using them

In terms of functionality, they are the same as regular functions. Here is an example:

```
let double = (i: Int) i * 2;

print(double(4)); // Prints 8
```

If you want to access data outside the context of the lambda expression, you can use **captures** (you can only capture variables, not expressions):

```
let i = -1;

let count = [i]() {
    i.inc();
    return *i;
};

let c0 = count(); // 0
let c1 = count(); // 1
let c2 = count(); // 2
let c3 = count(); // 3
```

Of course, you can also pass lambdas as function arguments as long as you use the correct type (see **function types**).