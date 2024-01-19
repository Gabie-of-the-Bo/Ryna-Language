One of the main features of any *Object Oriented Language* is the definition of **classes**. Nessa supports them, so let's take
a look at the syntax and semantics.

## Syntax

You can define new classes using the following syntax:

```
// Simple
class ClassName {
    attr_1: Type_1;
    attr_2: Type_2;
    [...]
}

// With generics
class ClassName<T> {
    attr_1: Type_1;
    attr_2: Type_2;
    [...]
}
```

If you define a new class anywhere in your code, you can use its name as a type:

```
let var: ClassName = expression;
```

## Usage

When you create a new class, some utility functions are defined automatically in order for you to be able to use the class effectively.

### Constructor

A class's definition implies the creation of a constructor function that takes as many parameters as attributes and creates an instance of that class.
The parameters are ordered from top to bottom. Let's see an example:

```
class ExampleClass {
    attrib_1: Int;
    attrib_2: &Int;
    attrib_3: @Int;
}

let a = 1;
let b = 2;

let ex = ExampleClass(1, a.demut(), b);
```

### Accessors

Each attribute creates a function with the same name that takes an instance of the created class and returns the value of the attribute. Attributes are 
returned as references to allow modifications. Following the previous example, you could access `ExampleClass`'s attributes like this:

```
ex.attrib_1(); // Returns @Int
ex.attrib_2(); // Returns &Int
ex.attrib_3(); // Returns @Int
```