One of the most important aspects of the Ryna type system are **templates**. These allow you
to pass types as parameters in order to create other types. This is mainly used in function and class
definitions to prevent code repetition. In this section, we will only explain the basic syntax and
semantics of templates.

## Introduction

A **template** can be defined as a placeholder for another type to go into. They allow the programmer to write typed structures
such as "lists of *T*", where *T* is a type with certain properties without the need of rewriting the class for each possible *T*.
The implementation can be a litle more involved, but it pays off as a library writer.

## Syntax

Type templates are specified in the context of **structural types**, **classes**, **functions** and **operations**. Each of these
has their own syntax and diving into the specifics would be outside the scope of the type system, but we will talk about the syntax
when templates are already defined.

Templates are always represented with an single quote character before an alphanumerical identifier that follows the same rules as
class names. Some examples of valid template names would be `'Test` or `'Parameter_1`. Names such as `'0123` or names with non-ASCII
characters are not allowed.

Now, this is the way to reference a template that has already been defined, but when you have class that **depends** on one or more templates (i.e.
a **parametric class**), you also have to specify the type parameters when referring to the class. For example, you cannot refer to the `Array` class
without specifying the type of data that goes inside the array as the first (and only in this case) type parameter. These parameters are specified 
the same way as in languages such as C++, with angle brackets. These would be some examples:

```
Array<Int>              // Array of Ints
Array<Float | String>   // Array of either Floats or Strings
Array<*>                // Array of anything
Array<'T>               // Array of 'T (only if 'T is in scope)
```

Of course, we can define a class with an arbitrary amount of type parameters, but Ryna does not include any with more than one by default. If we had an
hypotetical class called `HashMap` that had two parameters (the type of the key and the type of the value), we could refer to it like this:

```
HashMap<Int, Int>                 // HashMap from Ints to Ints
HashMap<String, Float | String>   // HashMap from Strings to either Floats or Strings
HashMap<*, *>                     // HashMap from anything to anything
HashMap<Bool, 'T>                 // HashMap from Bool to 'T (only if 'T is in scope)
```

## Semantics

Like we said before, a template is not exactly a type, but a placeholder for a type that will be substituted by a proper type later. This is done to
prevent manual code repetition and allow the creation of more general code. 

How this works is pretty simple. Every time you create a **parametric class** and refer to it anywhere in the code, the interpreter will substitute
the parameters automatically for the ones given inside the angle brackets. This can be essentially understood as creating a new class for each distinct
instance of a parametric class. This mechanism works exactly the same way for **parametric functions**, **structural types** and **operations**.

Of course, the fact that you substitute a type does not mean that the code magically works. In fact, it might fail to compile if you try to call a function 
overload that does not exist. The main way to avoid this is **bounded substitution** via **interfaces**.

## Bounded substitution

> **Note:** this works as of now, but syntax will probably change in future releases

This mechanism allows the programmer to specify constraints while defining a parametric type instance. One example of why you would want to do this is that
`HashMap` class we discussed earlier. As you might know, a *HashMap* structure requires keys to be **hashable** because of how data is stored inside for fast 
retrieval, but not all types might be hashable and you would also want to user to know that they have to implement a hashing function for a custom type
before they can put it as a *HashMap* key.

This is what **interfaces** do, they allow you to define APIs and assert whether or not a type follows it or not. If you define an interface called `Hashable`
that checks whether or not the type that implements it (called `Self`) has a function called `hash` that takes a `Self` and returns an `Int`, you can use it as a
**constraint** for template substitution.

This woult be done simply by changing the references to `'T` inside the class definition to `'T [Hashable]`, which translates to *a T which is Hashable*. You can put as
many constraints as you want separated by commas and the constraints can also be parametric, following the same syntax rules as types.