In the Nessa type system, **references** are the way the language has to make indirect value assotiations. They can be seen
as analogous to *pointers* in languages such as C or C++, but are much safer, since you generally cannot access invalid data.

There are two kinds of references in the Nessa type system: 

* **Constant references:** they can be used to read the data that they point to, but you **cannot modify it** under any circumstances.
* **Mutable references:** The same as constant ones, but you are allowed to modify or even replace the underlying data.

Now, in order to pointt to something, you need to know *what* you are pointing to. This is done with other types. When you
want to create a constant reference that points to a value of type *T*, you use the type `&T`. In a similar way, you use `&&T` for a 
mutable reference pointing to a *T*. This syntax does not pose a problem because references to references **are banned**.

> **Note:** this ban to double references may change in future versions 

Using this rule, we could construct types such as `&Int` or `&&String`, which refer to a constant reference to an `Int` and a mutable reference
to a `String`, respectively. In the next sections we will see how to create even more complex types.