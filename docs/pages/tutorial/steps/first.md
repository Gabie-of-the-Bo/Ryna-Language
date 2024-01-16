Now that you have the Nessa interpreter installed, you can start creating 
programs in this language, but you should know some basic facts about it first.
In this step by step guide we will explain almost everything there is to know about Nessa,
but be aware that the language is in a very young stage, so things may change.

## Main features and focus

The main focus of Nessa is to allow the user to write very expressive code
by means of powerful **syntax extension capabilities**. These range from **custom literals** to
a very expressive **macro system** that makes extending the syntax much easier than in other languages. 
This makes the language vary syntactically depending on which libraries you import into your program
and which features you select from them.

Be aware that this could be potentially a bad thing if it is not treated carefully. For example, this could
mean absolute chaos for a dynamically typed language such as Python. Nessa is designed so there are enough
safety measures to ensure the user can use these features without losing their minds when debugging extensions.

To begin with, Nessa is a primarily **imperative** language, but it also supports
many features that would be expected in a **functional** language, such as *first-class
functions*, *lambda expressions* and *Algebraic Data Types*. These features will be explained
later, but it's good to know they are supported beforehand. Readers with experience using
other functional languages might already realize that these make the language more reliable
in general.

There are other things that you should take into account, such as the lack of exceptions **by design** and 
the limited support for common algorithms, but these might be either changed in the future or
added as an optional library.

## What can you expect from Nessa?

A **mostly working** language, but no more for now.

This is a one-man passion project and cannot be sustained as a production grade language, so
you can expect bugs, the lack of important features and some design flaws. You can open an issue
in the repository if you find any of these. In the unlikely case that Nessa catches up, the state 
of things may change and support might be expanded.

## Is there any way to track the development?

Yes and no. The development cycle is mostly chaotic and depends on how I set priorities on the features
and how much free time I have. You can encourage me to work on something by opening an issue. 