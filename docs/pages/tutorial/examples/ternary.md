A common construct in programming languages is the ternary operator. Contrary to what the name implies,
it cannot be implemented easily in Nessa with operators, since they are **eager** and the ternary operator is **lazy**. Let's
take a look at how to implement it.

## Syntax

The common syntax for the ternary operator is the following:

```
condition ? if_true : if_false
```

This syntax poses a problem, and it is that it makes the parser run out of memory. This is something that you might have to fight
sometimes. This can easily be fixed by using delimiters:

```
{ condition ? if_true : if_false }
```

We can represent this syntax using the following *NDL* pattern:

```
"{" 
    {s} Arg(<expr>, condition) {s} "?" 
    {s} Arg(<expr>, if_true) {s} ":" 
    {s} Arg(<expr>, if_false) {s} 
"}"
```

## Creating the macro

You can create the macro for the ternary operator using the following code (note that we have to escape the if's closing brace):

```
syntax ternary_operator from [...] {
    if $condition {
        return $if_true;
    \}
    
    return $if_false;
}
```

After this, you can write code such as this one:

```
let var = { 4 > 6 ? 1 + 2 : 1 + 4 };
```

Here, the `1 + 2` will never be executed, since the condition is `false`. The compiled code would be the following:

```
let var = do {
    if 4 > 6 {
        return 1 + 2;
    }

    return 1 + 4;
};
```