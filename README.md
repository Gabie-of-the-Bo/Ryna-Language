[![Rust](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml/badge.svg)](https://github.com/Gabie-of-the-Bo/Nessa-Language/actions/workflows/rust.yml) [![codecov](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language/branch/develop/graph/badge.svg?token=WJA85OICZG)](https://codecov.io/gh/Gabie-of-the-Bo/Nessa-Language)

<p align="center">
  <img src="doc/img/logo.png" width="60%">
</p>
  
# What is _Nessa_?

**_Nessa_** is an imperative concept-oriented programming language with mathematical proofs. Many of its ideas come from the [_ULAN Language_](https://ulan-language.herokuapp.com) (of which I'm also the author), such as the concept programming features and the complex parameter serialization capabilities.

# Is _Nessa_ ready for production use?

**Not yet**. There is not even an stable release of the complete language, so don't expect it to work just yet. It is expected that this language will have a minimal **experimental** release soon, and features will be added as issues are created or as they seem to make sense.

# Features

> _**Disclaimer:** This entire list is a sketch for now, wo don't take anything too seriously until there is a working release. Note that this is a tremendous amount of work and cannot be done in too little time. Features may be added or discarded as the project goes on, so be patient :)_

This is a _WIP_ list of features that Nessa will have when it is released:
* Arbitrary precision integer arithmetic.
* Full parametric algebraic types:

  ```typescript
  a: Number | String;        // Either a number or a string
  b: (Number, String);       // A number followed by a string
  c: Array<Number | String>; // An array of elements that are either numbers of strings
  ```

* **Property types**: you will have the ability to specify mathematical properties about snippets of code in order to prove some other properties by induction:

  ```typescript
  n: Number [Integer, Even] = some_function();
  m: Number [GreaterEqual(0)] = abs(n);
  ```

  This should also be applicable to functions, but with prefix notation:

  ```typescript
  [Halts]
  proc(a: Number) => String {
      // Some complex procedure
  }

  /* 
    proc's type is [Halts] (Number) => String

    This translates to "a function which halts (terminates) 
    that gives a String from a Number"
  */
  ```

* **Property assertions**: these properties will sometimes be hard or impossible to prove, so there has to be the possibility to assert them. Let's see an example:
  
  ```typescript
  a: Number = some_difficult_function();
  b: Number [Equal(0)] = a.mantissa();
  ```

  This will not work because ```a``` can be a non-integer. If we assume that it is very difficult or impossible to prove that ```some_difficult_function()``` returns an integer, we can try to assert it to postpone its proof:

  ```typescript
  a: Number [assert! Integer] = some_difficult_function();
  b: Number [Equal(0)] = a.mantissa();
  ```

* **Code markings**: you will be able to mark certain parts of your code with an identifier in order to separate the proof from the code. If the accompanying proof is declared true by the system, then the property will be accepted by the interpreter (syntax subject to changes): 

  ```typescript
  // example.nessa
  
  [assert! Halts]
  func_1() { /* Some procedure */ }

  [assert! Halts]
  func_2() { /* Some procedure */ }

  [Halts (1)]
  proc() {
      func_1()
      func_2()
  }

  a: Number = some_difficult_function();
  b: Number [Equal(0)] = a.mantissa();
  ```

  ```typescript
  // axioms.proof

  // An array's elements have a property if all its elements imply it
  /* Name: ArrayInnerProps */
  a: Array<* [*'1', *]>
    given i: Number [Integer, Bounded(0, a.size() - 1)]
    for all i    
      if a.get(i) [*'2']
      if '2' -> '1'

  // A function halts if all its subprocedures also halt
  /* Name: SubproceduresHalt */
  f: [Halts, *] * => * if f.subprocedures: Array<* [Halts, *]>
  ```

  ```typescript
  // example.proof

  import axioms

  // Target: proc: [Halts, *] * => *
  proof 1:
    let s = proc.subprocedures;
    let i: Number [Integer, Bounded(0, s.size() - 1) '1'];

    s.size() is 2 by code; // We can know this by looking at the code
    '1' is Bounded(0, 1) by sub, arith; // We substitute and calculate

    s: Array<* [Halts, *]> by check ArrayInnerProps:
        given i is i by sub // Left is the theorem's i, right is ours
        s.get(i) [Halts] by cases i: // We can know the cases because of Bounded
            0 => s.get(0): [Halts, *] by code, sub, assertion
            1 => s.get(1): [Halts, *] by code, sub, assertion

        qed by all
        
    // If ommited, a "qed by all" will be added
    proc: [Halts, *] * => * by check SubproceduresHalt

    qed by this // "this" is the previous line of code. It has to match the target
  ```

  These proofs will only be necessary when the property cannot be trivially deduced (such as the ```abs``` one). More on this will be added when the implementation begins. 

* **Custom syntaxes**: you will be able to create new tokens using an internal language called _NDL_ (_Nessa Definition Language_):

  ```typescript
  class Dice{
    // Short definition
    syntax from Arg(1{d}, rolls) 'D' Arg(1{d}, faces)

    faces: Number
    rolls: Number
  }

  // Alternative, more involved, definition
  syntax for Dice from Arg(1{d}, rolls) 'D' Arg(1{d}, faces) {
      self.faces = faces;
      self.rolls = rolls;
  }

  //Usage
  dice = 4D20 // Four dice of twenty sides
  ```

* **Operator and operation definitions**: you will be able to define new operators and operations using an easy syntax:

  ```typescript
  unary operator "++";
  binary operator "<=>";
  nary operator "`" to "´";
  
  (a: &Number) ++ {
    return a + 1;
  }
  
  (a: &Number) <=> (b: &Number) {
    if a < b {
      return -1;
    }
    
    if a > b {
      return 1;
    }
    
    return 0;
  }
  
  (a: &Number) `(b: &Number, c: &Number)´ {
    return a + b * c; // This one is pretty much made up
  }
  ```
