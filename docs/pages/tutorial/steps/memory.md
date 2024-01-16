Let's begin this tutorial by talking about something that might be overly thechnical and not necessary: **the memory model**.
I'll provide a short answer for the more experienced readers so they can skip this and go to the next section: Nessa uses
**reference counting** for every value without **cycle detection**. If you understood what that meant, you are safe to skip 
this section, else you *should* keep reading, even though this can be very intuitive.

## Data storage

Nessa stores data into **Data Blocks** that can be referenced by other data blocks:

``` mermaid
%% mermaid
graph LR;
    A["`**Data A**`"]
    B["`**Data B**`"]
    C["`**Data C**`"]

    B-->A;
    C-->A;
```

In this case, ```Data A``` is **referenced** by ```Data B``` and ```Data C```. We say that these last two are **references**. 
Every Data Block has a **reference counter** (*RC* from now on) that is set to the number of blocks that reference it. The fact that two blocks 
are referencing ```Data A``` makes it have a *RC* with that very value:

``` mermaid
%% mermaid
graph LR;
    A["`**Data A** 
    (*RC* 2)`"]
    B["`**Data B** 
    (*RC* 0)`"]
    C["`**Data C** 
    (*RC* 0)`"]

    B-->A;
    C-->A;
```

Now, Data Blocks are initially created with an *RC* of 0 (as expected) and it is incremented by 1 each time a new Block references it.
This is easy, but things get a little more complicated when you start deleting them.

The rules used to delete blocks are the following:

1. A block can only be **safely** deleted if no other blocks are referencing it.
2. If you try to delete a block whose *RC* is not 0, it will become an *orphan*, which is a Block whose data can only be accessed by reference. This makes sense in the context of Nessa.
3. When you **safely** delete a block, the interpreter decreases the *RC* of every other block it references by 1. **Safely** delete any referenced *orphan* Blocks whose *RC* reaches 0.

Let's illustrate this with an example. Picture what would happen if we tried to delete ```Data C```. Since its *RC* is 0, we can safely delete it and
decrease the *RC* of ```Data A``` by 1:

``` mermaid
%% mermaid
graph LR;
    A["`**Data A** 
    (*RC* 1)`"]
    B["`**Data B** 
    (*RC* 0)`"]

    B-->A;
```

Then, if we tried to delete ```Data A```, we would create an *orphan* Block, allowing the value to still be accessed from ```Data B```:

``` mermaid
%% mermaid
graph LR;
    A["`**Data A** 
    (*RC* 1)
    *orphan*`"]
    B["`**Data B** 
    (*RC* 0)`"]

    B-->A;
```

If we tried to delete ```Data B``` after that, then we could safely delete it and decrease the *RC* of ```Data A``` to 0. Then ```Data A```
would also delete by cascade.

While they might seem a little strange, these rules are used in many other languages to allow values inside a program to live for as long as they are referenced. These also
eliminate any null dereferencing crashes. But this can also cause some problems if used carelessly.

## Cyclic references

This memory model is very good when it comes to erradicating pesky null references, but it also has the problem that it might allow the 
creation of infinite loops. Let's elaborate further.

Let's imagine that we have a configuration such as this one:

``` mermaid
%% mermaid
graph LR;
    A["`**Data A** 
    (*RC* 1)`"]
    B["`**Data B** 
    (*RC* 1)`"]

    B-->A;
    A-->B;
```

If this is the case, then ```Data A``` and ```Data B``` **cannot be deleted**. If we try to delete them in any order we only reach
a configuration where both are orphan, but have an *RC* of 1. This is called a *cycle* and is not exactly easy to solve. Right now,
Nessa allows the creation of these and no warning will be given, so a memory leak can indeed be caused.

Having said this, a cycle should not be common unless you are doing some strange coding. It's your choice if you want to abuse 
the system, but be warned that this pattern should be avoided.

> **Note:** there *might* be a moment where a full garbage collector is implemented if there is a demand for it, but performance
> will most likely be affected by it

Now we can continue to a less technical section.