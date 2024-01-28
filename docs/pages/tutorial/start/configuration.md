When you create and execute a new Nessa project, multiple files are created and it's good to know the meaning and
structure of each one. Let's take a look at them.

## Project files

In a Nessa project there can be multiple files created automatically:

1. **nessa_config.yml**: contains everything the interpreter needs to execute a project. This includes the module's name,
   verion and paths to look for modules.
2. **nessa_cache/main.nessac**: contains a cached file that allows the execution of a module without recompiling.
3. **nessa_cache/prof.json**: contains profiling information about the program.

Let's take a look at each of them.

## General configuration

The **nessa_config.yml** file contains the following values:

* **module_name**: the module's name.
* **version**: the current moddule's version.
* **hash**: an **automatically calculated** value that summarizes the contents of a module in order to check if recompilation is necessary.
* **module_paths**: a list of strings that contains every path where the interpreter should look for modules when executing this project. They can contain
  environment variables by using the format `${variable_name}`.
* **modules**: a map where the keys are the names of the modules that can be used inside the module and the values are objects with the following keys and values:
    * **version**: SemVer string that represents the version of the imported module.
    * **path**: path pointing to the folder where the imported **nessa_config.yml** is located. 

You can edit this however you want, but it is not recommended to modify the **hash** property. Also, the **modules** property *should* be handled using the `nessa add`
command.

## Nessa cache

There is not much to say about **main.nessac** other than when it is present, the program will not be recompiled as long as the hash is the same. Also, you can force
recompilation by using the `--recompile` flag.

## Profiling information

> ***Note:*** this file is still at a very experimental stage, so things may change and you might see some things that are not documented. Only the parts meant for
> non-developers will be explained here.

> ***Note 2:*** this file does not record generic functions very well for now.

The **prof.json** file is a json that contains many values, but the only ones that you should care about are the following:

* **total_time**: total execution time in nanoseconds.
* **fn_count**: map where the keys are the signatures of the functions in your code and the values are the number of times they have been called.
* **fn_time**: map where the keys are the signatures of the functions in your code and the values are the combined execution time of each one.