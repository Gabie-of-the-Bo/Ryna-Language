Now that we have seen all available types, we will see the last section on the type system: the **binding rules**. These specify whether or 
not you can put a value of a given type inside a variable of another type. Most of these are pretty straightforward, but listing them
makes things easier on complex cases.

A type `A` is bindable to type `B` if and only if one of the following cases apply:

1. `B` is `*`.
2. `A` and `B` are the same.
3. Both `A` and `B` are **constant references** and their inner types are bindable.
4. Both `A` and `B` are **mutable references** and their inner types are bindable.
5. `A` is a **structural type** and its **alias** is bindable to `B`.
6. `B` is a **structural type** and `A` is bindable to its **alias**.
7. `A` is a **template** and `B` follows its **constraints** and the substitution is not incoherent with the rest.
8. `B` is a **template** and `A` follows its **constraints** and the substitution is not incoherent with the rest.
9. `A` is a **sum type** and all its **variants** are bindable to `B`
10. `B` is a **sum type** and `A` is bindable to any of its **variants**.
11. Both `A` and `B` are **product types** with the same length and every type `A_i` is bindable to `B_i`.
12. Both `A` and `B` are the same **parametric type** with the same **class** and every type parameter `A_i` is bindable to the type parameter `B_i`.
13. Both `A` and `B` are **function types** and both their **argument types** and **return types** are bindable.

As you can see, checking whether or not two types are bindable is a recursive process, but the ideas are very intuitive given the descriptions in their
corresponding sections. Now that we have defined one of the key aspects of Nessa, we will begin explaining how to use the language.