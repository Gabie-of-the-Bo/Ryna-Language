We will take a small detour before we continue explaining features and talk about *special* functions.
When we say special we mean that the function does not just *compute* something, but affects the interpreter in some way.
These functions are not many, but are needed in order to create some complex behaviours or in order to minimize the memory
usage of a program. Here is a table with these functions:

| Name    | Description                                                                                                                                                  |
| ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `print` | Takes a single argument and prints it to *stdout*                                                                                                            |
| `panic` | Takes a single `String` and prints it to *stderr*, then exits the program                                                                                    |
| `swap`  | Takes a two arguments of the same type and swaps their contents to a memory level                                                                            |
| `move`  | Takes a single mutable reference and returns a dereferenced value. This renders the value it pointed to **invalid** and the interpreter will error on access |
| `as`    | Returns the argument coerced to the type of the type argument **if it is the same type**, else the interpreter will throw an error                           |
| `is`    | Checks if the argument is of the type given in the type argument. You should use this before using the `as` function                                         |