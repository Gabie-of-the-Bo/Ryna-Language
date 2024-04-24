[![Rust](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml/badge.svg)](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml) [![codecov](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language/branch/develop/graph/badge.svg?token=WJA85OICZG)](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language)
[![Webpage](https://img.shields.io/badge/Webpage-online-green)](https://gabie-of-the-bo.github.io/Nessa-Language/)
[![VSCode Extension](https://img.shields.io/badge/VSCode_extension-online-green)](https://marketplace.visualstudio.com/items?itemName=NessaLang.nessa-language-support)
[![Crates.io Version](https://img.shields.io/crates/v/nessa-language)](https://crates.io/crates/nessa-language)

<p align="center">
  <a href="https://gabie-of-the-bo.github.io/Nessa-Language/">
    <img src="docs/img/logo.png" width="60%">
  </a>
</p>
  
# What is _Nessa_?

**_Nessa_** is an imperative concept programming language with a stong type system. Many of its ideas come from the [_ULAN Language_](https://idus.us.es/handle/11441/84976) <sup>(Spanish only)</sup>, of which I'm also the author. The points of this language are to challenge the idea of classical software mantainability and to take extensible programming to an extreme in order to minimize syntatical and semantical noise.

Take a look at the [official page](https://gabie-of-the-bo.github.io/Nessa-Language/) to learn about the language and its features!

# Can I use _Nessa_?

**Of course!** The language is at an *experimental* stage, so it is indeed usable, but expect some things to fail. You can take a look [here](https://gabie-of-the-bo.github.io/Nessa-Language/pages/tutorial/start/install/) for instructions on how to install the interpreter.

# Features

These are some of the things you can do with Nessa:

* **Arbitrary precision integer arithmetic**.
* **Full parametric algebraic types**:

  ```
  Int | String;        // Either a number or a string
  (Int, String);       // A number followed by a string
  Array<Int | String>; // An array of elements that are either numbers of strings
  ```

* **Recursive types**:

  ```
  class Nil {}
  type Tree<T> = Nil | ('T, Tree<'T>, Tree<'T>);

  let t: Tree<Int> = (
      3, 
      (
          1, 
          Nil(), 
          (
              3, 
              (1, Nil(), Nil()), 
              (2, Nil(), Nil())
          )
      ), 
      (2, Nil(), Nil())
  );
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
  5.is_number<Int>();   // You can also explicitly instantiate the template
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
  syntax array_initialization from "<" Arg(<type>, type) ">[" [{Arg(<expr>, elems) "," [s]} Arg(<expr>, elems)] "]" {
      let res = arr<$type>();

      @elems.i {
          res.push($elems.i);
      }
      
      return move(res);
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
  op (a: &Int) ++ -> Int {
      return a + 1;
  }
  
  op (a: &Int) <=> (b: &Int) -> Int {
      if a < b {
          return -1;
      }
      
      if a > b {
          return 1;
      }
    
      return 0;
  }
  
  op (a: &Int) `(b: &Int, c: &Int)´ -> Int {
      return a + b * c;
  }
  ```

* **Built-in testing framework**: you can use the `@test` annotation to build unit tests without external libraries:

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

* **Built-in documentation generator**: you can use the `@doc` annotation document your project and the `nessa docs` command to generate human-readable markdown files with the documentation of your project:

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

# Projects written in _Nessa_

> If you want to showcase a project here, you can submit a pull request or open an issue :)

## Chessa engine

[Chessa](https://github.com/Gabie-of-the-Bo/Chessa/) (pun intended) is a simple chess engine written in _Nessa_ in order to show how the language can be used in medium sized projects. Also, it is used internally as a benchmark when measuring optimizations.

## Genessa

[Genessa](https://github.com/Gabie-of-the-Bo/Genessa/) (again, pun intended) is a genetic algorithms library written in _Nessa_. It allows creating custom crossover and mutation functions and includes examples such as the _N-Queens_ problem.

# Contribute

You can contribute to the project by opening issues when you find a bug in the interpreter or when you happen to have a suggestion on how to improve either the language or the documentation. All contributions, big or small, are welcome :)