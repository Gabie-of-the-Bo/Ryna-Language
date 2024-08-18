Finally, before we take a look at the binding rules that tie this type system together, we will explain one last feature that
takes algebraic types to the next level: **structural types**. These allow you to create **aliases** for types, but these are 
allowed to be **recursive** so you can create data structures from them.

## Syntax

To talk about structural types we need to take a look at the syntax Ryna uses to define them:

```
type Name = AnotherType;
```

In this example, `Name` would be the generated structural type and `AnotherType` would be the so called *alias*, which is the
"translation" of the type `Name`. Of course, these could also be made using generics: 

```
type Name<T1, T2> = AnotherType<'T1, 'T2>;
```

## Recursive Data Types

This is fine for having shorter names for complex types, but not much else. The power in these comes from allowing recursion.
This would be an example of a recursive structure that can encode a **binary tree** of `Int`:

```
type BinaryTree = () | (Int, BinaryTree, BinaryTree);
```

This is a common introduction to these data types. In this case, a binary tree is either an empty tree (represented by a `()`) 
or an `Int` followed by two subtrees with the left and right branches. Of course, this is not the only way to construct such 
an structure, but it is a valid example.

Now that we have seen every feature of the type system we can define the binding rules. 