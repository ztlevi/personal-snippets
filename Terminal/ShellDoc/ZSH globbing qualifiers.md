# Extended Globbing

As you might have noticed at this point, when it comes to Globbing, zsh goes above and beyond the
call of duty and then some more. What we'll discuss next is the more advanced aspects of Globbing,
commonly referred to as extended Globbing. Put simply, we'll learn a new set of characters and
expressions that expand on what we have been using to provide even more functionality to the shell's
operations. However, before we ride that horse, pry open that `.zshrc` file of yours and add the
following option:

```sh
setopt EXTENDED_GLOB
```

Or call it from your terminal if you plan on adding it later on. As we'll see in no time, extended
Globbing is there to give a special meaning to characters like `#`, which if you recall, is
typically used for comments. Now let's get our hands dirty.

## Special patterns

Zsh's vast repertoire also includes a series of shortcuts or special patterns that aim to make
mundane tasks a bit more tolerable. We will get familiarized with them in this section.

## Recursive searching

Arguably, the most popular pattern out there is recursive searching. Accessible through the `**/`
combination, this pattern tells zsh to perform a recursive search, starting from the current
directory and working its way inwards along the directory tree.

For example, here's how we look for all the markdown files (files which typically have the `.md`
extension) on the current working directory:

```
% echo **/*.md
```

README.md brew/README.md git/README.md scripts/README.md zsh/README.md Then there's also the \*\*\*/
flavor, which tells the shell to follow symbolic links. Be careful though, as it can lead to errors
such as "file name too long", which is the operating system's way of telling you that either the
rabbit hole is too deep, or you have a circular reference somewhere.

> Keep in mind that specialized tools like find or The Silver Searcher
> (https://github.com/ggreer/the_silver_searcher) will run circles around the shell's directory
> recursion mechanism. Thus, you should avoid relying on it for "serious" operations.

As for the caveats of using the recursive pattern expression, you might eventually be greeted with
an "argument list too long" warning from the system. This usually means the shell is taking up too
much memory space when expanding the `**/` pattern into the directory structure, which in turn could
happen if you have a really complex tree to work on. A workaround, if you insist on using the
recursive expansion, is to pass each argument with the help of `xargs` as follows:

```sh
% find **/*.md | xargs echo
```

I know, this example is a bit dumb as the same could be accomplished just with a simple find
`**/*.md` for a multiple-row result. The idea here is that you get to know how to `pipe` the results
of the find into echo by splitting them with `xargs`, so bear with me. Lastly, there's somewhat of a
hack you can use in case you want to exclude the current directory from the pattern:

```sh
% echo */**/*.md
```

That way, only filenames that include `base_dir/any_dir` will match the pattern.

## Alternate patterns

Having to choose between two options and then being given a third one clearly inferior, can make a
person rethink his decision... or so the story goes. Luckily, the shell is not a complex creature
like us, and we can provide it with a choice of patterns to select should one fail. We do that by
using the parentheses with a pipe construct, like the following example:

```sh
% echo [[:upper:]]*.(md|txt)
README.md README.txt
```

We continue on our search for the README files, using a named range to specify the filename we want
with an uppercase letter before defining either an md or a txt extension. Simple, right? Well, not
quite. Just be careful so as not to start the command line with parentheses, as this might make them
run in a subshell instead. Zsh is smart enough to discriminate between intended usages, so you'll
probably be safe most of the time. Try not to push your luck though. Before we move on, it bears
mentioning you can't use a pattern that contains a / character within the group alternatives we just
learned. You have been warned!

## Numeric ranges

You can make the shell match any series of digits it encounters with the `<->` special pattern. What
makes this construct great though, is that it can match any series of digits without a length
restriction (this is because the shell processes each digit independently and not as a whole
integer). Take, for example, the following directory:

```sh
% ls
log.txt log_002.txt log_010.txt log_031.txt
log_001.txt log_009.txt log_030.txt
```

We want to work with those files that match the log_xxx.txt pattern, where xxx is a digit. Let's put
what we just learned to good use:

```sh
% echo log_<->.txt
log_001.txt log_002.txt log_009.txt log_010.txt log_030.txt log_031.txt
```

What if we want those logfiles from 10 upwards? Zsh has you covered:

```sh
% echo log_<10->.txt
log_010.txt log_030.txt log_031.txt
```

As you can see, the <-> pattern can define a range with lower and upper bounds. Let's try again,
this time for files between 10 and 20:

```sh
% echo log_<10-20>.txt
log_010.txt
```

Another cool feat of this expression is that it doesn't take into account leading zeroes, allowing
you to sort things such as 00010 and 00013. Speaking of which, there's the `NUMERIC_GLOB_SORT`
option, which you can also set in order to output a sorted numeric match of any pattern matches (and
that's any as in, not just the numeric range pattern).

```sh
% setopt numericglobsort
% echo log_*
log_001.txt log_002.txt log_009.txt log_010.txt log_030.txt log_031.txt
```

## Revisiting the caret operator

As we saw earlier, we use the caret `(^)` operator to negate patterns (remember: "anything but what
matches this"). Here's another way to use the caret:

```sh
% ls
README.md README.txt bindings.c bindings.h bindings.o main.c main.o
% echo b^*.o
bindings.c bindings.h
```

So basically, we're telling the shell to expand that pattern so as to match the filenames that start
with b but do not have an .o extension. We can then safely say that the pattern^other_pattern
expressions work by matching the first pattern and avoiding matches on the other_pattern side of the
expression. A word of caution now that we are using special characters with different meanings
though is, remember to wrap names or expressions that you want taken literally with single quotes,
like in the following example:

```sh
% echo '^c'
```

Otherwise, you might be asking for trouble. The tilde operator Similar to the caret operator's
second usage, the tilde (~) operator can be used to define a pattern that consists of a part that
should match and a second part that shouldn't:

```sh
% ls
README.md README.txt bindings.c bindings.h bindings.o main.c main.o
% echo b*~*.o
bindings.c bindings.h
```

Basically, this is just a combination of two patterns: `b*` and `*.o`, linked with the "do not match
what follows `"` operator: `~`. Again, we can read that as "match everything that starts with a
lowercase b and does not match anything that ends with `.o"`. If you recall, we used `b^*`.o with
the caret, so the tilde version seems a bit more straightforward if I might say so. But don't take
my word for it. Let's use the tilde to exclude, for example, any files within a temporary directory:

```sh
% ls tmp
delete_me.sh out.txt
% echo **/*.sh~tmp/*
src/script.sh
```

What happens is that the shell runs the first pattern `(**/*.sh)` and recursively checks for all
files with the sh extension. The preliminary result is a list of possible filenames that is then
matched against the second pattern `(tmp/*)`. The filenames that match the latter are removed from
the list, and we are left with the filenames we were searching for. Just for academic purposes, it
might be a good time to mention that `**/` is equivalent to the `(*/)#` pattern. As it stands, the
special operator `#` will match a single repeating character (in parentheses), or a recurrent
expression (in brackets).

## Glob qualifiers

Besides operators, zsh boasts qualifiers, which are essentially a sort of filters you apply to your
pattern in order to restrict things like matching only files or folders, type of permissions for
those filenames, or even the owner of such entries. So in the following example, we'll list all the
directories that match the `*tmp` pattern. Notice the `(/)` construct, that's what intuitively sets
files and folders apart:

```sh
% echo *tmp(/)
tmp
```

What about matching only vanilla files then? Fair enough, (.) is your designed qualifier for
files-only restrictions.

```sh
% ls -F
README.txt script.zsh zsh/ src/
```

Suddenly, a wild filename appears:

```sh
% echo *zsh(.)
script.zsh
```

We have a zsh directory and a script file with a `.zsh` extension. Typically, we would roll with an
echo `*zsh` construct to list both of them, or a more restrictive echo `*.zsh` construct if we were
just looking for files with an extension; however, the `(.)` qualifier is arguably better suited for
complex tree searches or when dealing with lots of similar filenames and directories. What follows
is a "cheatsheet" for the most common qualifiers:

- `(N)`: Remove argument if no matches are found, silently ignore errors. Acts as a per-command
  NO_GLOB option.
- `(@)`: Symlink qualifier. Used for only selecting symbolic links.
- `(-@)`: A special variation of the previous one. Use this to find any broken symlinks.
- `(/)`: Directories only.
- `(.)`: Files only. Whatever is not either a link, directory, or any of the previous will be
  selected by this.
- `(*)`: Executable files. Directories need not apply. Think of this as `(.)` for those files with
  +x permissions.
- `(r)`: File is readable by the current shell user.
- `(w)`: File is writable by the current shell user.
- `(X)`: File is executable by the current shell user.
- `(U)`: File is owned by the current shell user.
- `(R)`: File is readable by anyone.
- `(W)`: File is writable by anyone.
- `(X)``: File is executable by anyone.
- `(u:root:)`: File is owned by the user root. You can replace the : character with any another pair
  of symbols such as curly braces: `(u{root})`. Just refrain from using pipes `(|)`.
- `(on)`: Sort filenames by name. The echo `*(on)` construct will be analogous to ls.
- `(On)`: Reverse-sort filenames by name.
- `(oL)`: Sort filenames by file size.
- `(OL)`: Reverse-sort filenames by file size.
- `(om)`: Sort filenames by modification date.
- `(Om)`: Reverse-sort filenames by modification date.

As always, feel free to mix and match to spice up things. Like poking with `(*r^w)` for regular
files that are readable but not writable by your user, or `(@,/)` for either symlinks or directories

> Eager to find out more about qualifiers and what have you? Fret not dear reader, and embrace the
> mystical powers of... never mind, we'll just resort to context completion. Type the following, and
> remember to press Tab right after the opening parentheses `% echo *zsh<Tab>` This will yield
> context completion for the glob qualifiers listed here (and many more!). What follows are the more
> complex batch of qualifiers, such as timestamps and file size, which require a bit more explaining
> before delving right into their usage.

### Timestamp qualifiers

Unix systems typically record three timestamps on their filesystems: modification, access, and
change times. With that in mind, you can use the following construct for Globbing filenames:

```sh
% echo *(mh-1)
```

This will provide you with the files modified in the last hour. You can easily check this result via
an ls -l qualifier. The m there is the modification time, which is the most common type of timestamp
you'll be interested in. Nevertheless, you could also check for either access ((ah-1)) or creation
((ch-1)) qualifiers within the last hour. Regarding that "last hour" bit, it's represented by the
h-1 qualifier, where h stands for hour (yes, yes, I know) and could be replaced by either minutes
(m), weeks (w), or Months (an uppercase "M"). Note that the default unit for this qualifier is days,
so (m1) will mean a day ago or, more precisely, up to 24 hours before the current system time.
Similarly, the plus operator can be translated as "more than", allowing you to describe such
patterns as (mw+3), which is a concise way of saying "more than three weeks from today". Finally,
you can also specify a range by combining the two operators:

```sh
% echo *(m-5mh+2)
```

This will provide the files modified between five and two hours.

### File size qualifiers

The last qualifier you'll get to know today is the file size. As you might have guessed already, we
can query filenames on the basis of their size on the disk:

- `(Lm+size)`: The file size is larger than size megabytes. For example: `(Lm+5)`—larger than five
  megabytes.
- `(Lm-size)`: The file is smaller than size megabytes. For example: `(Lm-2)`— smaller than two
  megabytes.
- `(Lk+size)`: The file size is larger than size kilobytes. For example: `(Lk+5000)`—larger than
  5000 kilobytes.
- `(Lk-size)`: The file is smaller than size kilobytes. For example: `(Lm2000)`—smaller than 2000
  kilobytes.
