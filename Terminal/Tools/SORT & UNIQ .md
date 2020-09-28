# SORT & UNIQ

The preceding commands are obvious: they do what they say they do. These two provide the most punch
in tandem (i.e. unique word counts). This is due to uniq, which only operates on duplicate adjacent
lines. Thus, the reason to sort before piping the output through. One interesting note is that
`sort -u` will achieve the same results as the typical `sort file.txt | uniq pattern`.

Sort does have a sneakily useful ability for data scientists: the ability to sort an entire CSV
based on a particular column.

```
# Sorting a CSV file by the second column alphabetically

sort -t"," -k2,2 filename.csv

# Numerically

sort -t"," -k2n,2 filename.csv

# Reverse order

sort -t"," -k2nr,2 filename.csv
```

The -t option here is to specify the comma as our delimiter. More often than not spaces or tabs are
assumed. Furthermore, the -k flag is for specifying our key. The syntax for this is `-km,n`, with
`m` being the starting field and n being the last.

- Useful options:

  - `sort -f` ignore case
  - `sort -r` reverse sort order
  - `sort -R` scramble order
  - `uniq -c` count number of occurrences
  - `uniq -d` only print duplicate lines
