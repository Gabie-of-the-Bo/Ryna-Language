[![Rust](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml/badge.svg)](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml) [![codecov](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language/branch/develop/graph/badge.svg?token=WJA85OICZG)](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language)
[![Static Badge](https://img.shields.io/badge/Webpage-online-green)](https://gabie-of-the-bo.github.io/Nessa-Language/)
[![Crates.io Version](https://img.shields.io/crates/v/nessa-language)](https://crates.io/crates/nessa-language)

<p align="center">
  <a href="https://gabie-of-the-bo.github.io/Nessa-Language/">
    <img src="docs/img/logo.png" width="60%">
  </a>
</p>
  
# What is _Nessa_?

**_Nessa_** is an imperative concept programming language with formal enough semantics to allow mathematical proofs. Many of its ideas come from the [_ULAN Language_](https://idus.us.es/handle/11441/84976) <sup>(Spanish only)</sup>, of which I'm also the author. The points of this language are to challenge the idea of classical software mantainability and to take extensible programming to an extreme in order to minimize syntatical and semantical noise.

Take a look at the [official page](https://gabie-of-the-bo.github.io/Nessa-Language/) to learn about the language and its features!

# Is _Nessa_ ready for production use?

**Not yet**. The language is at an *experimental* stage, so it is indeed usable, but expect some things to fail.

# Features

> **_Disclaimer:_** these might be changed and/or expanded in future releases

These are some of the things you can do with Nessa:

* **Arbitrary precision integer arithmetic**.
* **Full parametric algebraic types**:

  ```
  a: Int | String;        // Either a number or a string
  b: (Int, String);       // A number followed by a string
  c: Array<Int | String>; // An array of elements that are either numbers of strings
  ```

* **Powerful function overloading semantics**: you will be able to define functions using this rich type system and make use of call polymorphism semantics:

  ```
  fn this_is_a_test(a: Int) -> Bool {
    return true;
  }

  fn this_is_a_test(a: String) -> Bool {
    return false;
  }

  // Both valid
  this_is_a_test(5);      // Returns true
  this_is_a_test("Test"); // Returns false
  ```

* **Generic template-based programming**: parametric types are also supported by means of templating in a similar way as _C++_ does:

  ```
  fn<T> is_number(a: 'T) -> Bool {
    return a.is<Int>();
  }

  // Template arguments are automatically inferred from the parameters if possible
  5.is_number();        // This is true
  "Test".is_number();   // This is false
  5.is_number<Int>()    // You can also explicitly instantiate the template
  ```

* **Custom literals**: you will be able to create new literals using an internal language called _NDL_ (_Nessa Definition Language_):

  ```
  class Dice {
    // Syntax definition
    syntax from Arg(1{d}, rolls) 'D' Arg(1{d}, faces)

    faces: Int
    rolls: Int
  }

  //Usage
  dice = 4D20; // Four dice of twenty sides
  ```

* **Compile-time syntax extensions**: you will be able to extend the syntax of the language using _NDL_ by means of high level patterns:

  ```
  syntax array_initialization from '<' Arg(<type>, type) '>[' [{Arg(<expr>, elems) ',' {' '}} Arg(<expr>, elems)] ']' {
      {#let res = arr<} {$type} {#>(} {#);\n}
      {@i in $elems} {
          {#res.push(} {$i} {#);\n}
      }
      {#return *res;}
  }

  let array = <Int>[1, 2, 3, 4];
  ```

* **Operator and operation definitions**: the language allows the definition of new operators and operations using an easy syntax:

  ```
  // Operator definition
  unary postfix op "++" (500);
  binary op "<=>" (1000);
  nary op from "`" to "´" (1500);
  
  // Operation definition for each operator
  // These can be overloaded and templated just the same as functions
  op (a: &Int) ++ {
    return a + 1;
  }
  
  op (a: &Int) <=> (b: &Int) {
    if a < b {
      return -1;
    }
    
    if a > b {
      return 1;
    }
    
    return 0;
  }
  
  op (a: &Int) `(b: &Int, c: &Int)´ {
    return a + b * c; // This one is pretty much made up
  }
  ```
