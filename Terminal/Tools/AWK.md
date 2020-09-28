# AWK

The best for last. Awk is much more than a simple command: it is a full-blown language. Of
everything covered in this article, awk is by far the coolest. If you find yourself impressed there
are loads of great resources - see here, here and here.

Common use cases for awk include:

Text processing Formatted text reports Performing arithmetic operations Performing string operations
Awk can parallel grep in its most nascent form.

```
awk '/word/' filename.csv
```

Or with a little more magic the combination of grep and cut. Here, awk prints the third and fourth
column, tab separated, for all lines with our word. -F, merely changes our delimiter to a comma.

```
awk -F, '/word/ { print $3 "\t" $4 }' filename.csv
```

Awk comes with a lot of nifty variables built-in. For instance, NF - number of fields - and NR -
number of records. To get the fifty-third record in a file:

```
awk -F, 'NR == 53' filename.csv
```

An added wrinkle is the ability to filter based off of one or more values. The first example, below,
will print the line number and columns for records where the first column equals string.

```
awk -F, ' $1 == "string" { print NR, $0 } ' filename.csv

# Filter based off of numerical value in second column

awk -F, ' $2 == 1000 { print NR, $0 } ' filename.csv
```

Multiple numerical expressions:

```
# Print line number and columns where column three greater
# than 2005 and column five less than one thousand

awk -F, ' $3 >= 2005 && $5 <= 1000 { print NR, $0 } ' filename.csv
```

Sum the third column:

```
awk -F, '{ x+=$3 } END { print x }' filename.csv
```

The sum of the third column, for values where the first column equals “something”.

```
awk -F, '$1 == "something" { x+=$3 } END { print x }' filename.csv
```

Get the dimensions of a file:

```
awk -F, 'END { print NF, NR }' filename.csv

# Prettier version

awk -F, 'BEGIN { print "COLUMNS", "ROWS" }; END { print NF, NR }' filename.csv
```

Print lines appearing twice:

```
awk -F, '++seen[$0] == 2' filename.csv
```

Remove duplicate lines:

```
# Consecutive lines
awk 'a !~ $0; {a=$0}']

# Nonconsecutive lines
awk '! a[$0]++' filename.csv

# More efficient
awk '!($0 in a) {a[$0];print}
```

Substitute multiple values using built-in function gsub().

```
awk '{gsub(/scarlet|ruby|puce/, "red"); print}'
```

This awk command will combine multiple CSV files, ignoring the header and then append it at the end.

```
awk 'FNR==1 && NR!=1{next;}{print}' *.csv > final_file.csv
```

Need to downsize a massive file? Welp, awk can handle that with help from sed. Specifically, this
command breaks one big file into multiple smaller ones based on a line count. This one-liner will
also add an extension.

```
sed '1d;$d' filename.csv | awk 'NR%NUMBER_OF_LINES==1{x="filename-"++i".csv";}{print > x}'

# Example: splitting big_data.csv into data_(n).csv every 100,000 lines
```

sed '1d;\$d' big*data.csv | awk 'NR%100000==1{x="data*"++i".csv";}{print > x}'
