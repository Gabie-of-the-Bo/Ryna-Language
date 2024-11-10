When you create and execute a new Ryna project, multiple files are created and it's good to know the meaning and
structure of each one. Let's take a look at them.

## Project files

In a Ryna project there can be multiple files created automatically:

1. **main.ryna**: entrypoint of the interpeter. This is the file that `ryna run` will execute.
2. **ryna_config.yml**: contains everything the interpreter needs to execute a project. This includes the module's name,
  version and paths to look for modules. This file should not be shared publicly.
2. **ryna_deps.yml**: contains the name, dependencies and versions of the project. This file can be shared publicly and
  a full **ryna_config.yml** file can be reconstructed from it if the required libraries are available.
3. **ryna_cache/main.rynac**: contains a cached file that allows the execution of a module without recompiling.
4. **ryna_cache/prof.json**: contains profiling information about the program.

Let's take a look at each of them.

## General configuration

The **ryna_config.yml** file contains the following values:

* **module_name**: the module's name.
* **version**: the current module's version.
* **hash**: an **automatically calculated** value that summarizes the contents of a module in order to check if recompilation is necessary.
* **build**: a build script that will be executed when installing the library.
* **module_paths**: a list of strings that contains every path where the interpreter should look for modules when executing this project. They can contain
  environment variables by using the format `${variable_name}`.
* **modules**: a map where the keys are the names of the modules that can be used inside the module and the values are objects with the following keys and values:
    * **version**: SemVer string that represents the version of the imported module.
    * **path**: path pointing to the folder where the imported **ryna_config.yml** is located. 

You can edit this however you want, but it is not recommended to modify the **hash** property. Also, the **modules** property *should* be handled using the `ryna add`
command.

## Ryna cache

There is not much to say about **main.rynac** other than when it is present, the program will not be recompiled as long as the hash is the same. Also, you can force
recompilation by using the `--recompile` flag.

## Profiling information

> ***Note:*** this file is still at a very experimental stage, so things may change and you might see some things that are not documented. Only the parts meant for
> non-developers will be explained here.

The **prof.json** file is a JSON that contains many values, but the only ones that you should care about are the following:

* **total_time**: total execution time in nanoseconds.
* **loc_time**: map where the keys are the modules and the values are the times in nanoseconds spent executing that line in the module.