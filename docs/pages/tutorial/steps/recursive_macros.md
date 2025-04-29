You may have noticed that many common syntaxes are **recursive**. This means that they have arbitrarily nested structures or that
they have multiple, clearly separated parts that yoiu may want to also separate in the definition. One such example is JSON, which
can be built using many sub-JSONs. Ryna allows the user to define them, so let's take a look at how exactly.

## The _Macro_ RDL pattern

When defining a new syntax in Ryna, you have to give it a name. This name is what you specify when you import it from another module,
but it is also an identifier that you can reference inside RDL patterns. Take this syntaxes for example:

```
// We define two different kinds of "numbers"
syntax expr integer from Arg(["-"] 1{d}, digits) {
    $digits
}

syntax expr float from Arg(["-"] 1{d} "." 1{d}, digits) {
    $digits
}

// We define a "number" as either a "float" or an "integer"
syntax expr number from Macro(float, value) | Macro(integer, value) {
    $value
}
```

You can see that we can define two or more syntaxes and reference them inside of others. This can clear up separation of concerns, just like functions
do in regular code. Of course, any syntax can reference itself in order to build a recursive syntax (check out the JSON example in the *Learn by example* section).

## Intermediate syntaxes

As you can see, the syntaxes defined in the last part are *problematic*, since they collide with the syntaxes for `Float` and `Int` (also, *number* 
collides by definition with the other two). This does not mean that defining these does not make any sense, because you might want to reference 
them in other syntax, so you can mark them as **intermediate**:

```
// We define two different kinds of "numbers"
syntax int expr integer from Arg(["-"] 1{d}, digits) {
    $digits
}

syntax int expr float from Arg(["-"] 1{d} "." 1{d}, digits) {
    $digits
}

// We define a "number" as either a "float" or an "integer"
syntax int expr number from Macro(float, value) | Macro(integer, value) {
    $value
}
```

This means that you can reference them inside other syntaxes, but they **will not** be used to parse your code. This is very useful when defining recursive structures.

## Putting it all together

Lets imagine that we have the previous three intermediate syntaxes. We can define a syntax that does not collide with any standard one using this final syntax:

```
syntax expr final_number from "NUM" s Macro(number, value) {
    $value
}
```

You may think that this syntax is also problematic, but **it is not**. Let's take a look at an usage example:

```
let n = NUM 5;
```

This will be parsed as a `final_number` and as an `integer` inside the `number` syntax, so the final `$value` that will be pasted is the following:

```
let n = 5;
```

Now, this can be interpreted either as an `integer`, but since it is marked as an intermediate syntax, the recursion stops and the number is parsed as is. You can see more complex examples in the *Learn by example* section.