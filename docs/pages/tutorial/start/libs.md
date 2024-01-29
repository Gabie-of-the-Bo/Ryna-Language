The Nessa interpreter allows you to install libraries from git repositories. Here are the commands that you can
use in order to do that.

## Installing *library packs*

A *library pack* is one or more libraries uploaded to a git repository (*main* branch). This is more correct that calling them a single library
because a repository might have more than one in different folders for convenience (such as *prelude*). You can install a library pack using the following
command:

```
nessa install <REPO_URL> <PACK_NAME>
```

This will clone the repository inside the configured modules folder. We assume that you have already executed `nessa setup` 
before and completed the wizard.

## Uninstalling *library packs*

In order to uninstall a library pack you have to use the following command:

```
nessa uninstall <PACK_NAME>
```

This removes the library pack from the configured modules folder.