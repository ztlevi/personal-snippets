# FIND

You can combine criteria with `-o`. Note that `-o` has lower precedence than juxtaposition, so you may need parentheses.

```
find . -name '*.jpg' -o -name '*.png'
find ./ -type f \( -iname \*.jpg -o -iname \*.png \)
find . -mtime -7 \( '*.jpg' -o -name '*.png' \)  # all .jpg or .png images modified in the past week
```

- On Linux, you can use `-regex` to combine extensions in a terser way. The default regexp syntax is Emacs
  ([basic regexps](http://en.wikipedia.org/wiki/Regular_expression#POSIX_Basic_Regular_Expressions) plus a few
  extensions such as `\|` for alternation); there's an option to switch to
  [extended regexps](http://en.wikipedia.org/wiki/Regular_expression#POSIX_Extended_Regular_Expressions).

  ```
  find -regex '.*\.\(jpg\|png\)'
  find -regextype posix-extended -regex '.*\.(jpg|png)'
  ```

- On FreeBSD, NetBSD and OSX, you can use `-regex` combined with `-E` for extended regexps.

  ```
  find -E . -regex '.*\.(jpg|png)'
  ```

# FD

## Parallel command execution

If the `-x`/`--exec` option is specified alongside a command template, a job pool will be created for executing commands
in parallel for each discovered path as the input. The syntax for generating commands is similar to that of GNU
Parallel:

- `{}`: A placeholder token that will be replaced with the path of the search result (`documents/images/party.jpg`).
- `{.}`: Like `{}`, but without the file extension (`documents/images/party`).
- `{/}`: A placeholder that will be replaced by the basename of the search result (`party.jpg`).
- `{//}`: Uses the parent of the discovered path (`documents/images`).
- `{/.}`: Uses the basename, with the extension removed (`party`).

```sh
# Convert all jpg files to png files:
fd -e jpg -x convert {} {.}.png

# Unpack all zip files (if no placeholder is given, the path is appended):
fd -e zip -x unzip

# Convert all flac files into opus files:
fd -e flac -x ffmpeg -i {} -c:a libopus {.}.opus

# Count the number of lines in Rust files (the command template can be terminated with ';'):
fd -x wc -l \; -e rs
```
