When building a Ryna library that uses FFI, you will sometimes want to expose native structures to the Ryna side. For this, you can use a wrapped `Pointer` that
contains an adapted API, but there is a catch: the memory to which the pointer points to **will not be freed** upon destroying the object on the Ryna side. This is
due to many reasons, but you can fix this by defining a custom **destructor**. Let's take a look at how you can do this.

## What is a destructor?

A destructor is a special function that takes care of freeing the memory of an object. They are needed in programming languages with manual memory management such as 
C++, but they are rarely used (or even available) in languages with automatic memory management. These functions have some special properties and need to adhere to
certain standards depending on the language. Ryna is no special case for this.

In this language, destructors are defined by means of the `Destroyable` interface, which contains a `destroy(obj: &Self)` function. When you implement this interface
for a type, you are telling the interpreter that it has to destroy an object of this type **upon closing a context** using the function you provided. You should call 
the `LibraryFunction` that frees the memory inside `destroy`. Here is a small toy example:

```
class Test {
    name: String;
}

implement Destroyable for Test;

fn destroy(obj: &Test) {
    print("Destroying " + obj.name + "\n");
}

if some_condition {
    let t1 = Test("obj1");

    // Some more code
    // ...

    // t1 will be destroyed here
}

// here t1 no longer exists
```

## Destructor generation

Some types might need to be destroyed even though they do not implement `Destroyable`. This is the case of classes that contain elements that do implement it (`Array`s, 
for example). These cases are automatically detected by the compiler and their destructors are generated, so you do not need to implement these manually:

```
// Using the code from the previous snippet

if some_other_condition {
    let v = arr<Test>();
    v.push(Test("obj1"));
    v.push(Test("obj2"));
    v.push(Test("obj3"));

    // v is destroyed here, along with each internal element
}

// v no longer exists here
```

Also, sometimes you will have classes that need a custom destructor but that also have other attributes that have custom destructors. For these cases, you need to let the
compiler generate the destructor by wrapping every pointer. For example, this is forbidden by the compiler:

```
// Using the code from the first snippet

class Test2 {
    attr_1: Test;
    attr_2: Pointer; // This attribute needs a custom destructor
}

fn destroy(obj: &Test2) {
    // ...
}

// ERROR: a class that needs a generated destructor cannot implement Destroyable
implement Destroyable for Test2;
```

Instead, you have to do the following:

```
// Again, using the code from the first snippet

class NativeStruct {
    attr: Pointer
}

implement Destroyable for NativeStruct;

fn destroy(obj: &NativeStruct) {
    // Free the native memory here
}

class Test2 {
    attr_1: Test;
    attr_2: NativeStruct; // Now the destructor is generated
}
```

## Destructor locations

Data in general is not guaranteed to be freed in Ryna at the end of contexts. Ryna only frees the variables used inside a function's body after every execution and
when ending the execution. When you are using destructors, this is no longer true. Instead, the values are freed in these cases:

* When exiting a function via `return`.
* When exiting a context (`do`, `if`, `else`, `while` and `for`).
* When executing a lone expression that will not be stored.

So you have to take into account that memory management is a little bit more strict when dealing with values that implement the `Destroyable` interface.