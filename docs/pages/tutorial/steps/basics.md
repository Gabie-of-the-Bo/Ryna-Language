Nessa is an **imperative**, **strongly-typed** programming language with many common features. Here We will explain the main
ones. All these should be enough to get you started writing basic code, but more complex features will be analyzed later in their
own respective sections.

## Literals

One of the main ways you can create values is by writing down explicit constants that the compiler will understand as **basic types**.
These are called **literals** and are a very important concept in many languages, but more so in Nessa. Here are all the literals that
tthe language supports.   

### Integers

Integers can be inputted in Nessa in a similar way as other languages:

```
881452      // Basic integer (no size limit)
-32         // Negative integers are also literals
0b1000110   // Binary integers (70 in this case)
0xFF1A      // Hexadecimal integers (65306 in this case)
```

All these values are compiled to `Int` values.

### Floating Point numbers

Floating point numbers are more or less the same as in other languages:

```
1.56        // Basic float
-67.2       // Negative floats are also literals
1e10        // Scientific notation is allowed
2E-5
1.5e2
-2.2E-8
```

All these values are compiled to `Float` values.

### Logical values

The only logical literals Nessa supports are `true` and `false`. They are compiled to `Bool` values.

### Strings

Strings in Nessa are UTF-8 encoded and are inputted by using **double quotes** with the possibility of **escape sequences**.
These would be some examples:

```
"This is an example"

"This is an example with \t tabs and line \n jumps"

"Also, you 
can have
multiline 
strings"
```

All these values are compiled to `String` values.

### Characters

Characters do not exist in Nessa as a class, but they can be used as `Int` code points:

```
'w' // This is equal to 119
```

This can be used when iterating over code points.

## Variables

In contrast to functional languages, Nessa does have support for variables and they are used as much as in any other
imperative language. Constants are not implemented for now, but they might be introduced in a future release.

### Definition

The syntax for variable definitions is as follows:

```
let name: Type = expression;
```

In `let` statements, `name` can be any alphanumeric identifier that **does not begin with a number** ("_" is also allowed), 
`Type` can be any valid type of the ones seen in the **Type System** section and `expression` can be anything that is not a 
statement. These would be some valid examples:

```
let example_1: Int = 3 + 6;     // Simple definition
let example_2 = 3 + 6;          // Here the Int type is inferred
let example_3 = "Test";         // A String type is inferred
```

### Assignment

When you have already defined a variable, you can replace its value by using the `=` operator: 

```
let example: Int = 3 + 6;

example = 5;      // Replace the value by 5
example = true;   // ERROR: Bool is not bindable to Int
```

### Getting values

To get the value of a variable, just use its name in the code. Note that you will get a **Reference** to the value instead of the value
to prevent the user from making unnecessary copies:

```
let big = "Imagine this is a very big string ...";

let big_2: String = big;            // ERROR: @String is not bindable to String
let big_3: @String = big;           // Keeps a reference
let big_4: String = big.deref();    // Copies the original data
let big_5: &String = big.demut();   // Keeps a constant reference
```

### Reference assignment

Sometimes you might need to change the underlying values of a mutable reference. An example of this would be trying to change an element inside an array
or mutating a class instance. For these cases you can use the `:=` operator:

```
/*
    Imagine a class Example that has an Int 
    attribute called attribute_1
*/

let example: Example = some_class_instance;

example.attribute_1() := 5;     // This is only allowed using this operator
example.attribute_1() := "Test" // ERROR: String is not bindable to Int
```

## Functions

You can also use functions in Nessa, as you might expect. Functions are called using the parentheses syntax used in most imperative languages, 
including angle brackets in the case of generics.

## Operations

Just as most programming languages, Nessa has **operators** and **operations**. These encode operations with one, two o even more expressions called **operands**. Nessa supports four kind of operators:

1. **Prefix operators:** the operator comes before a single expression (*example:* `-5`).
2. **Postfix operators:** the operator comes after a single expression (*example:* `5!`).
3. **Binary (infix) operators:** the operator comes betwwen two expressions (*example:* `1 + 2`).
4. **N-ary operators:** the operator has an opening and a closing delimiter. Operands are before and inside the delimiters (*example:* `mat[4, 5]`).

All these operators and operations can be defined in a similar way as functions. The syntax for generics varies depending on the type of operator
and will be explained in their corresponding section.

## Flow control

These are the flow-control operations that are allowed in Nessa.

### If-else

The language supports if-else control flow with the following syntax:

```
// Else ifs and elses are optional
if condition {
    [...]
} else if condition {
    [...]
} else {
    [...]
}
```

conditions must evaluate to `Bool`.

### While

While loops have the following syntax:

```
while condition {
    [...]
}
```

conditions must also evaluate to `Bool`. You can `break` and `continue` in while loops.

### For

For loops are the most complicated of the three. To begin with, you need an `Iterable` object, which is defined as
one that implements the `Iterable` interface (more information about interfaces in their section). After this, you can write
the following:

```
// container must implement Iterable
for i in container {
    [...]
}
```

if you want to make a custom class iterable you have to implement that Interface manually. You can `break` and `continue` in for loops.