# WC

Word count. Its value is primarily derived from the -l flag, which will give you the line count.

# Will return number of lines in CSV

```
wc -l gigantic_comma.csv
```

This tool comes in handy to confirm the output of various commands. So, if we were to convert the
delimiters within a file and then run `wc -l` we would expect the total lines to be the same. If
not, then we know something went wrong.

- Useful options:

  - `wc -c` print the byte counts
  - `wc -m` print the character counts
  - `wc -L` print length of longest line
  - `wc -w` print word counts
