**Algebraic types** are a concept that has been hovering around functional programming languages for a long time. There are multiple 
resources online where you can get a very deep understanding on what they mean and what they can be used for, so here we will only
give a very short introduction to the concept. Also, you can check the *Learn by example* section at the end of the tutorial in 
order to see some applications.

The mechanics of these types are very simple, you have **sum types** and **product types**. The first ones allow you to define a 
**tagged union** of data (i.e. data that can be from *any* of the types that conform the sum, but only one at a time). The second one
is what we would understand as a **tuple**, which holds multiple elements in order. The interesting thing about these is that, when 
you allow recursion (such as Nessa does), you can create complex data structures called **Algebraic Data Types**. These can be extremely
useful in functional contexts, but simpler types can also be very useful when expressing complex algorithms.

In Nessa, **sum types** are expressed using `|`, so a value that is either an `Int` or a `Float` would be expressed using `Int | Float`. Of course,
you can add as many **variants** (types in the sum) as you want. On the other hand, **product types** are expressed using tuple-like syntax, so
`(Int, Float)` would be a tuple containing an `Int` and a `Float`, in that very same order. You can also add as many types as you want to a tuple.

As you might expect, you can mix these with everything we have seen before. All these are valid Nessa types:

```
Int | &Float         // Either an Int or a constant reference to a Float
&(Int | Bool)        // A constant reference to either an Int or a Bool
@(String, Int)       // Mutable reference to a tuple of a String and an Int
Bool | &(Bool, Bool) // Either a Bool or a constant reference to a tuple of two Bools
```

Even if these sound very expressive, this is not everything there is to know abot the Nessa type system. There are some special types that we need to talk about
before we dive into generics.