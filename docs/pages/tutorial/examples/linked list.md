Let's see how we can make use of Nessa's features to program a **Linked List**. This will guide you through all the design process
and all the common pitfalls that you might face.

## What is a *Linked List*?

A **Linked List** is a common data structure that is used to store several elements in sequence. It's not the same as an array,
since those store elements **sequentially in memory** and Linked Lists do not. This makes these structures useful for very big,
constantly growing data.

A Linked List consists of a series of nodes with references to the next node in the sequence. Every **node** has an element of the
list except for theh last one, that is called **sentinel** and just marks the end of the list:

``` mermaid
%% mermaid
graph LR
    1 --> 2
    2 --> 3
    3 --> ...
    ... --> End
```

## A first approach

These are probably the most common recursive data structure to implement when learning about algebraic data types. The basic idea is to first 
create an empty class for the sentinel (usually called `Nil`):

```
class Nil {}
```

After this, you can represent a Linked List (of `Int`s, for example) like this:

```
type List = Nil | (Int, List);
```

This means that a Linked List is either an empty `List` (`Nil`) or a value followed by another `List`. This recursion is enough to make
it store any amount of data:

```
let a: List = Nil();            // Empty list
let b: List = (1, Nil());       // List of one element
let c: List = (2, (1, Nil()));  // List of two elements
[...]
```

After this, you can build some utility functions:

```
// Add a new element
fn add(list: List, number: Int) -> List {
    return (*number, *list);
}

// Get every element but the first
fn rest(list: &&List) -> &&List {
    return list.as<&&(Int, List)>().get_1();
}

// Get size of the list
fn size(list: &&List) -> Int {
    if list.is<&&Nil>() { // The size of an empty list is 0
        return 0;
    }

    return 1 + list.rest().size(); // Else the size is 1 + the size of the rest
}
```

Note that these functions are not crafted to avoid unnecessary copies, so they might be made more efficient.

## Going generic

Now you might want to make the structure generic, this is actually not very difficult given what we have:

```
type List<T> = Nil | ('T, List<'T>);
```

Now this code is legal:

```
let a: List<Int> = Nil();                  // Empty list
let b: List<Bool> = (true, Nil());         // List of one element
let c: List<Float> = (2.2, (1.1, Nil()));  // List of two elements
[...]
```

Adapting the rest of the functions is left as an exercise to the reader :)