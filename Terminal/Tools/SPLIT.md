# SPLIT

File sizes can range dramatically. And depending on the job, it could be beneficial to split up the file - thus split.
The basic syntax for split is:

```
# We will split our CSV into new_filename every 500 lines

split -l 500 filename.csv new_filename_

# filename.csv
# ls output
# new_filename_aaa
# new_filename_aab
# new_filename_aac
```

Two quirks are the naming convention and lack of file extensions. The suffix convention can be numeric via the -d flag.
To add file extensions, you’ll need to run the following find command. It will change the names of ALL files within the
current directory by appending .csv, so be careful.

```
find . -type f -exec mv '{}' '{}'.csv \;

# ls output
# filename.csv.csv
# new_filename_aaa.csv
# new_filename_aab.csv
# new_filename_aac.csv
```

- Useful options:

  - `split -b` split by certain byte size
  - `split -a` generate suffixes of length N
  - `split -x` split using hex suffixes
