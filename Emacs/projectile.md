[Projects :: Projectile](https://docs.projectile.mx/projectile/projects.html#ignoring-files)

## Ignoring files

The contents of `.projectile` are ignored when using the `alien` project indexing method.

If you’d like to instruct Projectile to ignore certain files in a project, when indexing it you can
do so in the `.projectile` file by adding each path to ignore, where the paths all are relative to
the root directory and start with a slash. Everything ignored should be preceded with a `-` sign.
Alternatively, not having any prefix at all also means to ignore the directory or file pattern that
follows. Here’s an example for a typical Rails application:

```
-/log
-/tmp
-/vendor
-/public/uploads
```

This would ignore the folders only at the root of the project. Projectile also supports relative
pathname ignores:

```
-tmp
-*.rb
-*.yml
-models
```

You can also ignore everything except certain subdirectories. This is useful when selecting the
directories to keep is easier than selecting the directories to ignore, although you can do both. To
select directories to keep, that means everything else will be ignored.

Example:

```
+/src/foo
+/tests/foo
```

Keep in mind that you can only include subdirectories, not file patterns.

If both directories to keep and ignore are specified, the directories to keep first apply,
restricting what files are considered. The paths and patterns to ignore are then applied to that
set.

Finally, you can override ignored files. This is especially useful when some files ignored by your
VCS should be considered as part of your project by projectile:

```
!/src/foo
!*.yml
```

When a path is overridden, its contents are still subject to ignore patterns. To override those
files as well, specify their full path with a bang prefix.

If you would like to include comment lines in your .projectile file, you can customize the variable
`projectile-dirconfig-comment-prefix`. Assigning it a non-nil character value, e.g. `#`, will cause
lines in the .projectile file starting with that character to be treated as comments instead of
patterns.

### [](https://docs.projectile.mx/projectile/projects.html#file-local-project-root-definitions)File-local project root definitions

If you want to override the projectile project root for a specific file, you can set the file-local
variable `projectile-project-root`. This can be useful if you have files within one project that are
related to a different project (for instance, Org files in one git repo that correspond to other
projects).
