# PASTE

Paste is a niche command with an interesting function. If you have two files that you need merged,
and they are already sorted, paste has you covered.

```
# names.txt
adam
john
zach

# jobs.txt
lawyer
youtuber
developer
```

```
# Join the two into a CSV

paste -d ',' names.txt jobs.txt > person_data.txt
```

```
# Output
adam,lawyer
john,youtuber
zach,developer
```

For a more SQL\_-esque variant, see below.

# JOIN

Join is a simplistic, quasi-tangential, SQL. The largest differences being that join will return all
columns and matches can only be on one field. By default, join will try and use the first column as
the match key. For a different result, the following syntax is necessary:

```
# Join the first file (-1) by the second column
# and the second file (-2) by the first

join -t"," -1 2 -2 1 first_file.txt second_file.txt
```

The standard join is an inner join. However, an outer join is also viable through the -a flag.
Another noteworthy quirk is the -e flag, which can be used to substitute a value if a missing field
is found.

```
# Outer join, replace blanks with NULL in columns 1 and 2
# -o which fields to substitute - 0 is key, 1.1 is first column, etc...

join -t"," -1 2 -a 1 -a2 -e ' NULL' -o '0,1.1,2.2' first_file.txt second_file.txt
```

Not the most user-friendly command, but desperate times, desperate measures.

- Useful options:

  - `join -a` print unpairable lines
  - `join -e` replace missing input fields
  - `join -j` equivalent to -1 FIELD -2 FIELD
