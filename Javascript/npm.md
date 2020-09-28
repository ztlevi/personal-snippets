# NPM

## package version `~, ^, and *?`

~, ^, and \*? Oh I see what they do now... Let's start by looking at the '~'. The '~' character
fixes both the major and minor version numbers, while matching any patch number. e.g. if the version
dependency was defined as ~2.3.1, npm would match all versions greater than or equal to 2.3.1 and
less than 2.4.0 for that dependency.

Next up is the '^', which locks down the major version number, but leaves the minor and patch
versions to be more flexible. When installing a dependency that is listed with the version of
^2.2.3, npm will match any version that is greater than or equal to 2.2.3 and also less than 3.0.0.

Finally there is the '_', which while available, isn't used as often. The primary reason for its
less frequent use is the fact that it acts as a stand in for either the major, minor, or patch
number and catches any version number for the place that it represents. For example a dependency
with a version of '_' would equate to any version that was greater than or equal to 0.0.0, while
1.\* would allow versions greater than or equal to 1.0.0 and less than 2.0.0.

## Link

https://medium.com/dailyjs/how-to-use-npm-link-7375b6219557

Sometimes you need to work on application code and a dependency at the same time. You might be the
author of a dependency and don’t have good test coverage yet. The application can serve as an
end-to-end test for the dependency. Maybe you need to debug an issue in your application and the
problem seems to be in the dependency sources.

<figure>
<img src="https://cdn-images-1.medium.com/max/1600/0*wvDueqq35PXNE1qA" alt="" style="width:100%;display:block;margin-left:auto;margin-right:auto;"/>
<figcaption style="text-align:center"></figcaption>
</figure>

You could make changes in `node_modules` and manually copy the changes to the `git` repository of
the dependency once you are done. But there is a much cleaner approach: `npm link`.

#### Usage

Package linking is a two-step process:

1.  Create a global symlink for a dependency with `npm link`. A **symlink**, short for symbolic
    link, is a shortcut that points to another directory or file on your system.
2.  Tell the application to use the global symlink with `npm link some-dep`.

```sh
cd ~/projects/some-dep
npm link  # Step 1.
cd ~/projects/my-app
npm link some-dep  # Step 2.
```

<figure>
<img src="https://cdn-images-1.medium.com/max/1600/0*x8jMbWUMifff9Eao" alt="" style="width:100%;display:block;margin-left:auto;margin-right:auto;"/>
<figcaption style="text-align:center"></figcaption>
</figure>

You can edit, transpile, run tests, or commit as usual in `some-dep`. All while `my-app` runs with
the changes you made to `some-dep`. The symbolic links are local and will not be committed to `git`.
When you are ready to share your code, publish a new version of `some-dep` or push to a branch that
you specify in `my-app`’s `package.json`:

```sh
cd ~/projects/my-app
npm install — save some-dep@fhinkel/some-dep#experimental-branch
```

#### Debugging

If you use[ VSCode](https://code.visualstudio.com/) and want to set breakpoints in `some-dep`, you
need to enable symlinks in the debugger for `my-app`. Do so by setting

```markup--code
“runtimeArgs”: [  “--preserve-symlinks”]
```

in `launch.json`.

<figure>
<img src="https://cdn-images-1.medium.com/max/1600/0*H1TB22svP8POFP8p" alt="" style="width:100%;display:block;margin-left:auto;margin-right:auto;"/>
<figcaption style="text-align:center"></figcaption>
</figure>

#### Back to Normal

How do you switch back to _normal_ dependencies? When you don’t want to use the local version of
`some-dep` anymore, delete the symlink. But careful, `npm unlink` is an alias for `npm uninstall`,
it does not mirror the behavior of `npm link`.

```sh
cd ~/projects/my-app
npm uninstall --no-save some-dep && npm install
```

You can clean up the global link, though its presence won’t interfere with `my-app`.

```sh
cd ~/projects/some-dep
npm uninstall  # Delete global symlink
```

#### Conclusion

I used `npm link` while working on dependencies of the
[client libraries](https://github.com/googleapis/google-cloud-node) for Google Cloud Platform. All
our libraries use the module `@google-cloud/common`. In some cases I needed to immediately see the
changes in the larger libraries instead of in isolation in `common`.

**Mastering the two-step process of** `**npm link**` **is a useful addition to the toolset of any
Node.js developer.** The process consists of running `npm link` in the dependency, and
`npm link some-dep` in the application.

_Huge thanks to Peter Marshall and Alexander Fenster for helping review this post._
