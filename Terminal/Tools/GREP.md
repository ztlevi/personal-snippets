# GREP

Global search for a regular expression and print, or grep; likely, the most well known command, and with good reason.
Grep has a lot of power, especially for finding your way around large codebases. Within the realm of data science, it
acts as a refining mechanism for other commands. Although its standard usage is valuable as well.

```
# Recursively search and list all files in directory containing 'word'

grep -lr 'word' .

# List number of files containing word

grep -lr 'word' . | wc -l
```

Count total number of lines containing word / pattern.

```
grep -c 'some_value' filename.csv

# Same thing, but in all files in current directory by file name

grep -c 'some_value' *
Grep for multiple values using the or operator - \|.

grep "first_value\|second_value" filename.csv
```

- Useful options

  - `alias grep="grep --color=auto"` make grep colorful
  - `grep -E` use extended regexps
  - `grep -w` only match whole words
  - `grep -l` print name of files with match
  - `grep -v` inverted matching
