Nessa does not allow inheritance-based polymorphism as in most programming languages. Instead, generics are
entirely based on **bounded template substitution**. In order to use this, we need to define **interfaces** and
**implement** them for out types. In this section we will define the syntax and semantics of this mechanism.

## About interfaces

An **interface** can be defined as a set of function signatures that may or may not be fulfilled by a target type 
which is marked as `Self`. We say that a type `T` fulfills the interface if every function definition inside the
interface exists when we substitute `Self` by `T`.

Also, when a type fulfills an interface, you can **implement** it, which means that we mark thye type so that
the interpreter knows it and we can make use of bounded substitution on that interface with that type. Implementations
can also be **parametric**.

## Syntax

We can define an interface using the following syntax:

```
// Simple
interface Example {
    fn example_1(v: Self);
    fn example_2(...) -> Type;
    [...]
}

// With generics
interface Example<T> {
    fn example_1(v: Self, n : 'T);
    fn example_2(...) -> Type;
    [...]
}
```

Then we can implement it using the following syntax:

```
// Simple
implement Example for Type;

// With generics
implement Example<OtherType> for Type;

// Generic implementation (define for all arrays)
implement<T> Example<OtherType> for Array<'T>;
```

If `Type` does not fulfill `Example`, the interpreter will raise an error.

## Semantics

The best way to understand how this works is with an example. Imagine that you want to create a function that
takes an argument of a type that can be converted to `Int`. You could start by writing this:

```
fn test(n: Int) -> Int {
    [...]
}
```

This is problematic because you would have to convert everything on call time. To avoid this you could write something like this:

```
// An overload for each supported type
fn convert(n: Int) -> Int {
    return n.deref();
}

fn convert(n: Float) -> Int {
    return n.floor();
}

// Use generics
fn<T> test(n: 'T) -> Int {
    let converted: Int = convert(n.deref());
    [...]
}
```

The problem with this approach is that you rely on the `convert` function being defined for `'T` and that might not be the case. Even worse,
it could be defined with wrong types and cause difficult to debug errors. In order to avoid all this, you can use an interface and **bounded
substitution**:

```
// Define interface
interface Convertible {
    fn convert(n: Self) -> Int;
}

// An overload for each supported type
fn convert(n: Int) -> Int {
    return n.deref();
}

fn convert(n: Float) -> Int {
    return n.floor();
}

// Explicit implementations
implement Convertible for Int;
implement Convertible for Float;

// Use bounded generics
fn<T> test(n: 'T [Convertible]) -> Int {
    let converted: Int = convert(n.deref());
    [...]
}
```