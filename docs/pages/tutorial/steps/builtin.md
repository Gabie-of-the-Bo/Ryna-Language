The main building blocks of the Ryna type system are what we will call **basic types**. These represent kinds of data that can be stored.
As you might expect, Ryna provides an implementation for some basic types, which are summarized in the following table:

| Name     | Description                                                 |
| -------- | ----------------------------------------------------------- |
| `Int`    | Unbounded size integer                                      |
| `Float`  | 64 bits floating point number                               |
| `Bool`   | Value that can be either *true* or *false*                  |
| `String` | UTF-8 encoded text                                          |
| `File`   | Handle that points to a file and allows reading and writing |

These types are not much by themselves, but in the following sections we will see how we can combine them in order to make more complex types. 