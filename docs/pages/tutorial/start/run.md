Once you have created a project, you will want to run it. For this, you can use the following command.

## Running

The command to run a Ryna module is as follows:

```
ryna run <input_path>
```

Here `input_path` is the folder of the project that you want to run and defaults to the current folder. You may pass the following
arguments to the command:

| Long name   | Short name | Description                                                                                                   |
| ----------- | ---------- | ------------------------------------------------------------------------------------------------------------- |
| `recompile` | `r`        | if passed, do not cache compilation                                                                           |
| `profile`   | `p`        | if passed, the interpreter will instrument the code before running it in order to generate a *prof.json* file |
| `optimize`  | `o`        | if passed, the interpreter will try to optimize the code in order to run it faster (*recommended*)            |
| `test`      | `o`        | if passed, the interpreter will run the functions marked as tests instead of the main program                 |

## Documenting

You can automatically generate the documentation for your project by using the following command:

```
ryna docs <input_path>
```

Here `input_path` is the folder of the project that you want to document.
