Ryna is an interpreted language that runs on a custom *VM*. This means that, while it makes it easier to mantain consistent behaviours in different environments,
the *VM* may lack the ability to do certain low level tasks. Also, sometimes you want to use specialized structures for tasks that need to be as efficient as possible.
An example of this is the [NumPy](https://numpy.org/) Python library, which is coded in C for maximum performance.

As you might expect, you can do the same in Ryna by means of dynamic libraries. Let's take a look at how it works.

## Libraries and Functions

There are two classes included in the interpreter by default called `Library` and `LibraryFunction`. These allow you to call dynamic libraries using the
following functions:

```
// Load the library
let library = load_library("Path-to-your-dynamic-library");

// Load the functions inside the library
let fun1 = lib.demut().get_function("function-name-1");
let fun2 = lib.demut().get_function("function-name-2");
let fun3 = lib.demut().get_function("function-name-3");

// Call the functions
let res1 = fun1.demut().call(arg1, arg2, ...);
let res2 = fun2.demut().call(arg1, arg2, ...);
let res3 = fun3.demut().call(arg1, arg2, ...);
```

Now, this mechanism relies on the fantastic [libloading](https://crates.io/crates/libloading) Rust crate, so the string you use as an input to the `load_library`
function can automatically infer the extension of the library (*.dll* in Windows, for example).

Also, sometimes the library you are trying to load will be located inside the project's directory. You can use the `$MODULE_PATH` macro to insert a `String` containing
the path to the folder in which the *ryna_config.yml* is located. 

## Argument compatibility

As you might expect, passing Ryna objects to other programming languages is not an straightforward task. Only a subset of the original ryna types are supported and you cannot
pass custom classes as arguments. The compiler will let you do it, but it will crash in runtime. These are the types that are supported:

* `Bool`: translated as a 1 or a 0.
* `Int`: translated as a 64 bit signed integer.
* `Float`: translated as a double (64 bit floating point number).
* `String`: translated as a `Pointer`. 
* `Pointer`: passed as-is. The width depends of the architecture.

Take into account that `Pointer`s are **not typed**, so you should wrap them in custom classes in order to know what they are. Also, there is no way to dereference a value
of this type, they are only in Ryna to allow the user to create data structures that rely on FFI.

## Build scripts

If you happen to be creating a Ryna library that you want to distribute and it relies on FFI, you can add a custom build script to your library so the `ryna install` and the
`ryna build` commands execute it by default. For this, use the *build* key in the *ryna_config.yml* file.