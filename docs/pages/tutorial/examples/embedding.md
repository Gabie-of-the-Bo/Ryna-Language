Nessa's powerful macro system allows you to execute arbitrary code inside a macro in order to create new functionalities. In this page you
will learn how to embed a simple esoteric language into Nessa, but be aware that this concept can be turned into something more interesting
very easily.

## About the language

The language that we are going to embed is a very simple one called [*Brainfuck*](https://esolangs.org/wiki/Brainfuck). The exact implementation
details are not that important in this tutorial, but we have to know some things about it:

1. Its memory model is an infinite tape (*T*) of integers. We will approximate it with a large `Array<Int>`.
2. It has a memory pointer (*P*) that moves on the tape and changes the values there. This will be an `Int`.
3. Each of its instructions take **a single character**: `+`, `-`, `<`, `>`, `[`, `]`, `.` and `,`. A program consists of
   many of these in sequence.
4. Any character not present on the list before must be ignored.

Finally, the instructions do the following when executed:

| Instruction  | Effect               |
| -------- | -------------------- |
| >     | Increase *P* by one |
| <     | Decrease *P* by one |
| +     | Increase *T[P]* by one |
| -     | Decrease *T[P]* by one |
| [     | Jump to the matching `]` if *T[P]* is 0 |
| ]     | Jump to the matching `[` if *T[P]* is not 0 |
| .     | Output *T[P]* |
| ,     | Input value to *T[P]* |

## Matching the language

Since the language is so simple, the NDL pattern that matches a program is the following:

```
1{'+' | '-' | '<' | '>' | '[' | ']' | '.' | ','}
```

In this case, we will let the `,` operation as an exercise to the reader, but output will be programmed.

In order to be able to delimit the language effectively, we will use braces and a new keyword `BF`. The
complete syntax for a Brainfuck blockm will be the following:

```
BF {
    /* code goes here */
}
```

For this, we have to use the following NDL pattern:

```
'BF' {s} '{' Arg(1{s | '+' | '-' | '.' | '<' | '>' | '[' | ']'}, code) '}'
```

## Creating the macro

In order to write smaller code, we will use some helper functions to create the memory array:

```
fn<T> arr_with_capacity(size: Int) -> Array<'T> {
    let res = arr<'T>();
    res.reserve(move(size));
    
    return move(res);
}

fn<T> fill(array: @Array<'T>, value: 'T) -> @Array<'T> {
    while array.capacity() > array.len() {
        array.push(*value);
    }

    return array;
}
```

With this, we can initialize the memory array like this:

```
let mem = arr_with_capacity<Int>(1000);
mem.fill(0);
```

Given this, we can begin our macro code pattern like this:

```
syntax block embed_bf from 'BF' {s} '{' Arg(1{s | '+' | '-' | '.' | '<' | '>' | '[' | ']'}, code) '}' {
    {|        
        // Memory array
        emit("let mem = arr_with_capacity<Int>(1000);");
        emit("mem.fill(0);");
    |}
}
```

Later, we must create the memory pointer in a similar way:

```
{|        
    // ...

    // Pointer
    // This value is arbitrary, but more or less in the middle to allow some movement
    emit("let pt = 500;");

    // ...
|}
```

With this, we can begin generating the code that corresponds to the embedded Brainfuck code. For this, we have to
create a variable inside the macro code pattern by using variable substitution and iterate over each character:

```
{|        
    // ...

    // Code
    let code = "{$code}".ref().utf8_array();

    // Execution
    for i in code {
        // Handle each case
    }

    // ...
|}
```

Now comes the hardest part: translating the instructions to valid Nessa code given our simple model. For this, we will translate the
`>` and `<` operations as changing `pt`, `+` and `-` as changing the value in `mem[*pt]` and `.` as writing to an array **outside the macro**.
Interestingly, `[` and `]` can be handled by being translated to `while mem[*pt] != 0 {` and `}`, respectively.

The final code that handles the instructions is the following:

```
{|        
    // ...

    // Execution
    for i in code {
        if i == '+' { 
            emit("mem[*pt] := mem[*pt] + 1;"); // +
        
        } else if i == '-' {
            emit("mem[*pt] := mem[*pt] - 1;"); // -
        
        } else if i == '.' {
            emit("out.push(*mem[*pt]);");      // .
        
        } else if i == '<' {
            emit("pt = pt - 1;");              // <

        } else if i == '>' {
            emit("pt = pt + 1;");              // >
        
        } else if i == '[' {
            emit("while mem[*pt] != 0 {");     // [
        
        } else if i == ']' {                    // ]
            emit("}");
        }
    }

    // ...
|}
```

After all this, we would have a functional macro that lets us embed Brainfuck code into Nessa! This would be the complete code 
and an example:

```
// Helper functions
fn<T> arr_with_capacity(size: Int) -> Array<'T> {
    let res = arr<'T>();
    res.reserve(move(size));
    
    return move(res);
}

fn<T> fill(array: @Array<'T>, value: 'T) -> @Array<'T> {
    while array.capacity() > array.len() {
        array.push(*value);
    }

    return array;
}

// Input is not allowed in this example
syntax block embed_bf from 'BF' {s} '{' Arg(1{s | '+' | '-' | '.' | '<' | '>' | '[' | ']'}, code) '}' {
    {|        
        // Memory array
        emit("let mem = arr_with_capacity<Int>(1000);");
        emit("mem.fill(0);");
        
        // Pointer
        emit("let pt = 500;");

        // Code
        let code = "{$code}".ref().utf8_array();

        // Execution
        for i in code {
            if i == '+' { 
                emit("mem[*pt] := mem[*pt] + 1;"); // +
            
            } else if i == '-' {
                emit("mem[*pt] := mem[*pt] - 1;"); // -
            
            } else if i == '.' {
                emit("out.push(*mem[*pt]);");      // .
            
            } else if i == '<' {
                emit("pt = pt - 1;");              // <

            } else if i == '>' {
                emit("pt = pt + 1;");              // >
            
            } else if i == '[' {
                emit("while mem[*pt] != 0 {");     // [
            
            } else if i == ']' {                    // ]
                emit("}");
            }
        }
    |}
}

let out = arr<Int>();

// Taken from https://en.wikipedia.org/wiki/Brainfuck
BF {
    ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
}

print(utf8_to_str(out.demut())); // Prints "Hello World!\n"
```

As we said before, this example does not include input, but it could be trivially added by using an array defined outside
the macro. Also, some parts could have been done by using other macro generator patterns, but it is a good exercise to 
illustrate the power of compile time code execution.