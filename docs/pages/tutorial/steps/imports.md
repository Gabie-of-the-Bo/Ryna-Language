An important aspect of every programming language is importing code created before either by you or by
someone else. We call these **modules** in Ryna and you can import code selectively from them. Let's take a look
at how.

## Syntax

The syntax we need to use to import modules is as follows:

```
//Import function/s from a module
import fn function_name from module_name;
import fn {function_name1, function_name2, ...} from module_name;
import fn * from module_name;

//Import class/es from a module
import class class_name from module_name;
import class {class_name1, class_name2, ...} from module_name;
import class * from module_name;

//Import interface from a module
import interface interface_name from module_name;
import interface {interface_name1, interface_name2, ...} from module_name;
import interface * from module_name;

//Import syntax (macro) from a module
import syntax syntax_name from module_name;
import syntax {syntax_name1, syntax_name2, ...} from module_name;
import syntax * from module_name;

//Import operators from a module
import prefix op "op" from module_name;
import prefix op {"op1", "op2", ...} from module_name;
import prefix op * from module_name;

import postfix op "op" from module_name;
import postfix op {"op1", "op2", ...} from module_name;
import postfix op * from module_name;

import binary op "op" from module_name;
import binary op {"op1", "op2", ...} from module_name;
import binary op * from module_name;

import nary op "op" from module_name; // Concatenate open and close symbols for these
import nary op {"op1", "op2", ...} from module_name;
import nary op * from module_name;

// Import everything from a module
import * from module_name;
```

As you can expect, you have to configure the `module_name` module in your **ryna_config.yml** before importing it.

## Local imports

Local imports are a special case that apply when you want to import code from a file that is not **main.ryna** and is located 
inside the project folder (with any number of anidated folders). For this, just use the syntax `/folder/of/the/file`, where the file
is `<project_folder>/folder/of/file.ryna`. It is important the the path **starts with** `/`.