We have seen the most important building blocks for types in the previous sections, but there are **three** 
other types that we have to take into account. These might not come out so often, but are relevant nonetheless.

## Empty type

The **empty type** `()` is, technically the type of a tuple with no elements inside it (hence, the name). In Ryna,
it is used to represent what other languages call *void* or, in other words, **no value**.

As you might expect, this comes up in functions that do not return anything. The action of *not returning* is translated 
by the compiler into returning an instance of the empty type.

## Wildcard type

Perhaps a more strange type is the **wildcard type** `*`. As you might expect it represents an instance of **any type at all**.
Ryna includes it just because it might be useful in case you are trying to build a dynamic dispatch system of any kind and you
do not like generics.

## Funtion types

These types are formed by two parts: the **arguments** and the **return type**. A function type represents a function that takes
the parameters specified in the arguments and returns the return type. For example, `Int => Float` represents a function that takes an
`Int` and returns a `Float`. Multiple arguments can be specified using product types, so all these are valid function types:

```
(Int, Float) => Bool
(Int, *, Int) => Int
() => String          // No arguments 
```

The only way as of now to create instances of these types is via **lambda expressions**, but we will talk about them in a later section.
Now, let's finally dive into generics.