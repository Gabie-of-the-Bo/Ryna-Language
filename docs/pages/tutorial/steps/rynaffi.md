The last piece of the puzzle when dealing with interactions with other programming languages is **the other side** (i.e. the "native" language). Ryna is programmed
in Rust, so it makes sense for the only library (at the moment) for FFI interactions to be written in that language. We will now see how we can program a very simple 
library that uses the [ryna-ffi](https://crates.io/crates/ryna-ffi) crate to call Rust functions from Ryna.

## Creating the project

We first have to create a Rust project that uses the **ryna-ffi** crate and that is configured as dynamic library. This is an example *Cargo.toml* for that project:

```
[package]
name = "ryna_ffi_example"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
ryna-ffi = "0.1.0"
```

## Creating functions

Now you can go to your *lib.rs* and create the functions that you will expose to Ryna. These functions need to have a very specific signature, so a convenience macro
is included:

```
use rynaffi::{ryna_ffi_function, FFIArgs, FFIReturn};

ryna_ffi_function!(sum_two_floats(args, out) {
    // The args variable is of type &[FFIValue]
    let a = args[0].as_f64();
    let b = args[1].as_f64();

    /*
        You have to assign a FFIReturn object to 
        the out variable in order to return values to Ryna
    */
    unsafe { *out = (a + b).into(); }
});
```

This defines a function called *sum_two_floats* that, as the name implies, takes two `Float`s and returns their sum. The parameters of the function are passed as a
slice because this makes it easier to define an universal signature. Any other way to define an FFI function **may fail unpredictably**, so the usage of the previous pattern
and convenience methods is highly encouraged. 

After this, you can run the `cargo build --release` command to generate a dynamic library inside the *target* directory

## Back to the Ryna side

Now we can use the `load_library` and `get_function` functions from Ryna:

```
// Load the library
// We are assuming that you used the --release flag
let lib = load_library("path-to-the-project/target/release/ryna_ffi_example");
let fun = lib.demut().get_function("sum_two_floats");

// This returns a value of type *, so you should make a safe wrapper
let res = fun.demut().call(10.5, 1.5);

// Prints 12 (more or less, depends on floating point precision)
print(res.deref().as<Float>());
```

And that is all!

## Extending to more languages

The Ryna interpreter is compatible with any language that supports the C ABI the same way as Rust does. It is technically possible to generate a similar C FFI library for
Ryna, I just have not done it. Also, any language that can interact with Rust can also interact with Ryna by means of the already existent **ryna-ffi** library.

If you are interested in making a FFI library for any language and you are having problems please, submit an issue in the official repository and I will be happy to help :)
