Once you have created a project, you will want to run it. For this, you can use the following commands.

## Running

The command to run a Nessa module is as follows:

```
nessa run <input_path>
```

Here `input_path` is the folder of the project that you want to run and defaults to the current folder.You may pass the following
arguments to the command:

| Long name   | Short name | Description                         |
| ----------- | ---------- | ----------------------------------- |
| `recompile` | `r`        | if passed, do not cache compilation |

## Profiling

The command to profile a Nessa module is as follows:

```
nessa profile <input_path>
```

Here `input_path` is the folder of the project that you want to run and defaults to the current folder. You may pass the following
arguments to the command:

| Long name   | Short name | Description                         |
| ----------- | ---------- | ----------------------------------- |
| `recompile` | `r`        | if passed, do not cache compilation |

This command creates a JSON file called *prof.json* inside the *nessa_cache* folder. You can inspect it to profile the execution
of your code.