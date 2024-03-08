The main problem with regular functions is that they cannot be passed around, since polymorphism makes it really difficult.
In order to have first class functions you have to use **lambda expressions**. They work like this:

## Syntax

The syntax to create lambda expression is as follows:

```
(arg_1: Type_1, arg_2: Type_2, ...) -> ReturnType { 
    [body]    
};

// With inferred return type
(arg_1: Type_1, arg_2: Type_2, ...) { 
    [body]    
};

// Direct return
(arg_1: Type_1, arg_2: Type_2, ...) expression;
```

## Using them

In terms of functionality, they are the same as regular functions, including the fact that you cannot use variables outside its context.
Here is an example:

```
let double = (i: Int) i * 2;

print(double(4)); // Prints 8
```

Of course, you can pass lambdas as function arguments as long as you use the correct type (see **function types**).