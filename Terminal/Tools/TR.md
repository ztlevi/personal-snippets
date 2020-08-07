# TR

Tr is analogous to translate. This powerful utility is a workhorse for basic file cleaning. An ideal use case is for
swapping out the delimiters within a file.

# Converting a tab delimited file into commas

```
cat tab_delimited.txt | tr "\t" "," comma_delimited.csv
```

Another feature of tr is all the built in [:class:] variables at your disposal. These include:

```
[:alnum:] all letters and digits
[:alpha:] all letters
[:blank:] all horizontal whitespace
[:cntrl:] all control characters
[:digit:] all digits
[:graph:] all printable characters, not including space
[:lower:] all lower case letters
[:print:] all printable characters, including space
[:punct:] all punctuation characters
[:space:] all horizontal or vertical whitespace
[:upper:] all upper case letters
[:xdigit:] all hexadecimal digits
```

You can chain a variety of these together to compose powerful programs. The following is a basic word count program you
could use to check your READMEs for overuse.

```
cat README.md | tr "[:punct:][:space:]" "\n" | tr "[:upper:]" "[:lower:]" | grep . | sort | uniq -c | sort -nr
```

Another example using basic regex:

# Converting all upper case letters to lower case

```
cat filename.csv | tr '[A-Z]' '[a-z]'
```

- Useful options:

  - tr -d delete characters
  - tr -s squeeze characters
  - \b backspace
  - \f form feed
  - \v vertical tab
  - \NNN character with octal value NNN
