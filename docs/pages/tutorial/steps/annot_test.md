Nessa allows you to annotate functions in order to test your modules. Let's take a look at how.

## Creating tests

In order to create a test you have to annotate a function with the `@test` annotation, which takes **no arguments**. 
This function must **take no arguments** and must return a **logical value**. The test will be considered successful if and
only if the function returns `true`. Let's take a look at an example extracted from *prelude*:

```
@test
fn fill_test() -> Bool {
    let res = arr_with_capacity<Int>(3);
    res.fill(100);

    for i in res {
        if i != 100 {
            return false;
        }
    }

    return true;
}
```

This test should always pass, since it's filling an array with a value and then checking the contents, but
this can be useful when changing your code or when programming with *TDD*. 

## Testing

In order to execute the tests inside your module you have to use the use the following command:

```
nessa run <PATH> --test
```

You can also add optimization and recompilation flags, as seen in the *Getting Started* section.