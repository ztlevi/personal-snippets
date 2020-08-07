If you want a git repository to ignore permission changes (chmod),

type the following command into the Terminal while inside the git repository:

```
git config core.filemode false
```

It is usually possible to do this for all git repositories at once, instead of going one-by-one.

This is done by using the following command inside the Terminal (no need to be inside a git repository for this
command):

```
git config --global core.filemode false
```
