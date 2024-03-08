One of the main features of Nessa is that it allows the programmer to define many new elements of the syntax.
One of these are **literals**, which can be defined for any valid class. Let's see how this works.

## Basic classes

The firt part of this is the syntax of basic types. Not all of them have types, but the ones that do are the
basic building blocks for more compplicated literals. Here is a table with the basic types that do have a syntax:


| Type     | NDL Pattern           | Regex        |
| -------- | --------------------- | ------------ |
| `Int`    | `["-"] 1{d}`          | `-?\d+`      |
| `Float`  | `["-"] 1{d} "." 1{d}` | `-?\d+\.\d+` |
| `Bool`   | `"true" | "false"`    | `true|false` |
| `String` | No constaint          | `.*`         |

Everything that follows these syntaxes can be parsed as an instance of this type.

## Creating a literal

In order to create a new literal you first have to create a new class that the literal will be parsed to. Also, this
class's attributes must also have at least a syntax. Here is an example class thaht represents a number of *D&D* dice rolls: 

```
class Dice {
    rolls: Int;
    sides: Int;
}

let d = Dice(5, 20); // Throw a 20 sided dice 5 times 
```

Now, the readers that have played *D&D* know that this value is often represented as `5D20`. We can define this simple syntax
using Nessa:

```
class Dice {
    syntax from Arg(1{d}, rolls) "D" Arg(1{d}, sides);

    rolls: Int;
    sides: Int;
}

let d = 5D20; // Throw a 20 sided dice 5 times 
```

Here the `Arg` NDL marker tells the interpreter that the syntax of a `Dice` consist on an integer (the `rolls`) followed
by a `D` and, finally, another integer (the `sides`). After this, the class `Dice` can also be parsed inside an NDL pattern.
Now let's take a look at a different use case: an **integer array**:

```
class Ints {
    syntax from "[" Arg(1{d}, ints) {", " Arg(1{d}, ints)} "]";

    ints: Array<Int>;
}

let a: Ints = [1, 2, 3];
```

This case shows how the interpreter works when an `Arg` marker with the same **target** is matched several times. Each time it matches,
the result is parsed inside an `Array` of the type it parses.