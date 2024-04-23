Nessa also allows you to automatically document your modules by using annotations. Let's take a look at how.

## Documenting parts of your code

In order to document some defintion in your code, you have to use the `@doc` annotation. The first positional
argument will represent the general description of the entity you are documenting and the second one will
refer to the return value if there is one (interfaces, for example, do not have one). Also, you can use
named arguments to document the semantics of **attributes** and **arguments**, using the same name. Let's take a look at
an example extracted from *prelude*:

```
@doc(
    "Fills every available position in an array with a given value. The filling starts in the current `len`.",
    array: "the array to fill.",
    value: "the value to fill the available positions with.",
    "An array where every position starting from the previous `len` is `value`"
)
fn<T> fill(array: @Array<'T>, value: 'T) -> @Array<'T> {
    while array.capacity() > array.len() {
        array.push(*value);
    }

    return array;
}
```

The used as arguments contain **markdown** and will be rendered as such when the documentation is generated.

## Generating documentations

In order to generate the documentation you have to use the use the following command:

```
nessa docs <PATH>
```

The documentation will be generated inside a *docs* folder at the root of your project.