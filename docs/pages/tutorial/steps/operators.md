Let's now see how you can define operators and operations in Nessa. The process is always to define the operator and then the operations,
so the sections will follow that order. 

## How precedence works

Precedence is an **unique integer** that tells the interpreter the order in which it has to read operators. Let's take this expression for example:

```
let n = 2 + 3 * 4;
```

Is `a` equal to `2 + (3 * 4)` or `(2 + 3) * 4`? In this case it is obvious that the first option is correct, but this might not be as obvious with
other operators. We make the interpreter read that expression properly by setting a **lower** precedence on the `*` operator compared to `+`. This 
idea can also be naturally be extended to every other type of operator supported by Nessa.

## Prefix

You can define a prefix operator using the following syntax:

```
unary prefix op "operator_repr" (precedence);
```

Operations are defined as follows:

```
// Simple
op operator_repr (arg_name: Type) -> Return_type {
    [...]
}

// With generics
op<T> operator_repr (arg_name: Type) -> Return_type {
    [...]
}

// Simple usage
let var_1 = operator_repr expression;

// Generic usage
let var_2 = operator_repr<Type> expression;
``` 

## Postfix

You can define a postfix operator using the following syntax:

```
unary postfix op "operator_repr" (precedence);
```

Operations are defined as follows:

```
// Simple
op (arg_name: Type) operator_repr -> Return_type {
    [...]
}

// With generics
op<T> (arg_name: Type) operator_repr -> Return_type {
    [...]
}

// Simple usage
let var_1 = expression operator_repr;

// Generic usage
let var_2 = expression <Type>operator_repr;
``` 

## Binary

You can define a binary operator using the following syntax:

```
binary op "operator_repr" (precedence);
```

Alternatively, you can create a right-associative operator using the `right` keyword: 

```
binary right op "operator_repr" (precedence);
```

Operations are defined as follows:

```
// Simple
op (arg_name_left: Type_left) operator_repr (arg_name_right: Type_right) -> Return_type {
    [...]
}

// With generics
op<T> (arg_name_left: Type_left) operator_repr (arg_name_right: Type_right) -> Return_type {
    [...]
}

// Simple usage
let var_1 = expression operator_repr expression;

// Generic usage
let var_2 = expression <Type>operator_repr expression;
``` 

## N-ary

You can define a nary operator using the following syntax:

```
nary op from "open_repr" to "close_repr" (precedence);
```

Operations are defined as follows:

```
// Simple
op (arg: Type) open_repr arg_1: Type_1, arg_2: Type_2, ... open_repr -> Return_type {
    [...]
}

// With generics
op<T> (arg: Type) open_repr arg_1: Type_1, arg_2: Type_2, ... open_repr -> Return_type {
    [...]
}

// Simple usage
let var_1 = expression open_repr expression_1, expression_2, ... close_repr;

// Generic usage
let var_2 = expression <Type>open_repr expression_1, expression_2, ... close_repr;
``` 