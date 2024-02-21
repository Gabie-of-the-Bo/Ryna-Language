Once you have installed the Nessa interpreter, can begin creating projects. Let's see how this works.

## Create a project

The command to create a Nessa module is as follows:

```
nessa new <args>
```

Executing the command without arguments is enough to crate a project, since it will open a wizard that will
allow you to input a name and configure everything. In any case, here are the arguments you may input:

| Long name      | Short name | Description                                           |
| -------------- | ---------- | ----------------------------------------------------- |
| `name`         | `n`        | Name of the project (skips name in wizard)            |
| `version`      | `v`        | Version of the project (skips version in wizard)      |
| `modules`      | `m`        | Modules path of the project (skips modules in wizard) |
| `no-gitignore` | `g`        | Skip default .gitignore creation                      |

## Adding dependencies

The command to add a dependency to a Nessa project is as follows:

```
nessa add
```

This will open up a wizard that will autocomplete the names and versions of the available libraries using the module paths.
In any case, you can use the following args:

| Long name | Short name | Description                                           |
| --------- | ---------- | ----------------------------------------------------- |
| `name`    | `n`        | Name of the project (skips name in wizard)            |
| `version` | `v`        | Version of the project (skips version in wizard)      |

## Exporting dependencies file

You can export an anonymous project file that you can share in public repositories with the following command:

```
nessa save-deps
```

This will create a **nessa_deps.yml** file in your project's directory using the data from **nessa_config.yml**.

## Importing dependencies file

You can import a **nessa_deps.yml** file with the following command:

```
nessa load-deps
```

This will create a **nessa_config.yml** file in your project's directory using the data from **nessa_deps.yml**. In order to do this
the interpreter will use your default libraries path that you set with the `nessa setup` command and an optional extra libraries path.
You can use the following args:

| Long name | Short name | Description                       |
| --------- | ---------- | --------------------------------- |
| `modules` | `m`        | Extra modules path of the project |