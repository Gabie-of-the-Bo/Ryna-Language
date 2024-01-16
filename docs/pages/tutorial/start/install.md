## Installing the interpreter

Nessa is installed through `cargo`, *Rust*'s package manager. You can see the crate [here](https://crates.io/crates/nessa-language) 
and you can install it by using the following command:

```
cargo install nessa-language
```

After this, you can use the interpreter by using the `nessa` command, as you will see later in the tutorial. `cargo` should
make sure that the interpreter is properly installed.

## Installing from source

If you want to take a look at the lastest (unstable) features, you can clone the repository's develop branch and
use the following command to run the interpreter:

```
cargo run <args>
```

If you want to install the current version, run the compilation command: 

```
cargo build --release
```

Then, copy the corresponding binaries in the */target* folder to some location inside your *PATH* env variable.