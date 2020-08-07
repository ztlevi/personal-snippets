# Bash

```sh
readarray -t arr < <(cat ./substring.md)
printf '%s\n' "${arr[@]}"
```

# Zsh

```sh
arr=("${(@f)$(cat ./substring.md)}")
declare arr
```

## ZSH String splitting

```sh
st="aaa.bbb.ccc"
a=(${(s:.:)st})
declare -p a
echo ${#a}

>> typeset -a a
>> a=( aaa bbb ccc )
>> 3
```

# <()

This is called process substitution.

The `<(list)` syntax is supported by both,
[`bash`](https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html#Process-Substitution) and
[`zsh`](http://zsh.sourceforge.net/Doc/Release/Expansion.html#Process-Substitution). It provides a way to pass the
output of a command (`list`) to another command when using a pipe (`|`) is not possible. For example when a command just
does not support input from `STDIN` or you need the output of multiple commands:

```
diff <(ls dirA) <(ls dirB)
```

---

`<(list)` connects the output of `list` with a file in `/dev/fd`, if supported by the system, otherwise a named pipe
(FIFO) is used (which also depends on support by the system; neither manual says what happens if both mechanisms are not
supported, presumably it aborts with an error). The name of the file is then passed as argument on the command line.

---

`zsh` additionally supports `=(list)` as possible replacement for `<(list)`. With `=(list)` a temporary file is used
instead of file in `/dev/fd` or a FIFO. It can be used as a replacement for `<(list)` if the program needs to lseek in
the output.

According to the [ZSH manual](http://zsh.sourceforge.net/Doc/Release/Expansion.html#Process-Substitution) there might
also be other issues with how `<(list)` works:
