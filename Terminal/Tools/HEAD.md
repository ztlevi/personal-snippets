# HEAD

If you are a frequent Pandas user then head will be familiar. Often when dealing with new data the
first thing we want to do is get a sense of what exists. This leads to firing up Pandas, reading in
the data and then calling df.head() - strenuous, to say the least. Head, without any flags, will
print out the first 10 lines of a file. The true power of head lies in testing out cleaning
operations. For instance, if we wanted to change the delimiter of a file from commas to pipes. One
quick test would be: head mydata.csv | sed 's/,/|/g'.

```
# Prints out first 10 lines

head filename.csv

# Print first 3 lines

head -n 3 filename.csv
```

- Useful options:

  - head -n print a specific number of lines
  - head -c print a specific number of bytes
