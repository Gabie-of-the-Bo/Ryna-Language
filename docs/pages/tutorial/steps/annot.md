Many programming languages allow the user to add extra information to the structures they define by using **annotations**. 
You can also use them in Nessa. Let's see how.

## Syntax

An annotation in Nessa consists on a **name**, some **positional arguments** and some **named arguments**. Depending on the 
particular annotation, some arguments might be needed or optional. Also, you can use multiple annotations on a single definition. 
Here is the syntax:

```
// These two are equivalent

@annotation_name
fn function_name() {
    // ...
}

@annotation_name()
fn function_name() {
    // ...
}

// This one has positional arguments

@annotation_name("argument 0", "argument 1")
fn function_name() {
    // ...
}

// This one has named arguments

@annotation_name(
    name_0: "argument 0", 
    name_1: "argument 1"
)
fn function_name() {
    // ...
}

// This one has both

@annotation_name(
    "Positional arg 0",
    name_0: "argument 0", 
    name_1: "argument 1",
    "Positional arg 1" // You can insert named arguments between positional ones 
)
fn function_name() {
    // ...
}
```

## Usage

An annotation in Nessa can be put just before any **class**, **syntax**, **function**, **operation** or **interface** definition. Also, you can
annotate any **function** or **operation** inside an interface definition. In later sections we will take a look at every 
available annotations defined in the interpreter.