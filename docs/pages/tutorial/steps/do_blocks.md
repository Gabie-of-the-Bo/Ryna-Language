There is a final function-related issue that might happen when you code in Ryna. It can be solved with a new feature called **do blocks**, 
so let's take a look:

## The problem

Imagine you want to create a variable called `var` and you want to assign it the value `a` or `b` depending on a condition `c`. Your first
instict might be doing something like this:

```
let var;

if c {
    var = a;

} else {
    var = b;
}
```

The problem is, Ryna does not allow to declare a variable without initializing it. One way to solve this would be using a dummy value `d`: 

```
let var = d;

if c {
    var = a;

} else {
    var = b;
}
```

But this creates two problems:

1. Your program does unnecesary steps, so performance may suffer.
2. `d` might not be available in generic contexts.

You cannot do this, so you will have to use **do blocks**;

## Do blocks

A do block can be understood as a function body that will only be executed once. You could solve the previous problem like this:

```
let var = do {
    if c {
        return a;

    } else {
        return b;
    }
};
```

the syntax is self-explanatory and the semantics are the same as a function body, but without calls to any methods. Also, they allow you to access
the **context above it** (unlike lambdas or regular functions). 
These are the blocks that are used to compile **function macros**, but we will take a look at that in a later section. 