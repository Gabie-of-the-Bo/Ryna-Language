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

## Configuring the interpreter [**Important**]

It is extremely recommended that you run this command just after you install it:

```
nessa setup
```

This will ask you for a default modules location **where every library will be installed** by default and create a global
configuration file. Also, it will download the *prelude* libraries for you, which are some common functionalities that 
you might need to use eventually, such as sets, math functions or special iterators.  