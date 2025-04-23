The Ryna interpreter allows you to install libraries from git repositories. Here are the commands that you can
use in order to do that.

## Installing libraries

### Basic installation

The Ryna package manager considers that any git repository with a **ryna_deps.yml** file at its root is a library. There are three
ways of installing Ryna libraries:

1. **_(Recommended)_** Use `ryna install <LIBRARY_NAME>`. For this to work, the library has to be registered in the [central library index](https://github.com/Gabie-of-the-Bo/Ryna-lib-index). You can open an issue at the repository if you want your library to be registered.
2. Use `ryna install <LIBRARY_NAME> -r <REPO_URL>` if your library has a git repository but is not registered yet.
3. Manually copy your library to your designated libraries folder and make sure it is properly configured.

Following the recommended way, here are the arguments that you can pass to the command:

| Long name       | Short name | Description                                 |
| --------------- | ---------- | ------------------------------------------- |
| `repository`    | `r`        | Repository URL to fetch from                |
| `version`       | `v`        | Version to install                          |
| `execute-build` | `b`        | Execute build commands without asking first |

This will clone the repository inside the configured modules folder. We assume that you have already executed `ryna setup` 
before and completed the wizard. If the library contains a build script, you will be asked if you want to run it automatically 
(unless the `-b` flag has been set). Please, be aware that you should only execute build scripts from **trusted sources**, 
since they may contain arbitrary commands.

### Understanding library versions

The Ryna package manager supports having multiple versions of a library in the same repository. **Branches** are used for this with the following rules:

1. If the repository only has **one branch**, that will be the **v0.1.0**.
2. If it has **multiple branches**, the package manager will take the branch names that are valid SemVer strings (i.e. v1.0.0 or v0.11.6) and will offer them as possibilities.

## Uninstalling libraries

In order to uninstall a library pack you have to use the following command:

```
ryna uninstall <LIBRARY_NAME>
```

This removes the library from the configured modules folder. It has to be the same name that you used to install it.

## Searching libraries

If you want to look for a certain library in the central library index, you can use the `ryna search <TERM>` command.
This will list the registered libraries that contain the given term in their names. You can also pass the following arguments to the command:

| Long name  | Short name | Description            |
| ---------- | ---------- | ---------------------- |
| `versions` | `v`        | Also list the versions |