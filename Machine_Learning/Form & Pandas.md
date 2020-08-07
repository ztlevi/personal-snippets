# Pandas

```python
import pandas as pd

print(train_data.shape)

column_names = [
    "CRIM",
    "ZN",
    "INDUS",
    "CHAS",
    "NOX",
    "RM",
    "AGE",
    "DIS",
    "RAD",
    "TAX",
    "PTRATIO",
    "B",
    "LSTAT",
]

df = pd.DataFrame(train_data, columns=column_names)
print(df.head(5))
print(df.tail(5))
```

## Read Csv

```python
csv_data = pandas.read_csv("example.csv")
```

## Concat csv

```python
list_ = []

for file_ in allFiles:
    df = pd.read_csv(file_,index_col=None, header=0)
    list_.append(df)

frame = pd.concat(list_, axis = 0, ignore_index = True)
```

## Shuffle csv rows

The more idiomatic way to do this with pandas is to use the `.sample` method of your dataframe, i.e.

```
df.sample(frac=1)
```

The `frac` keyword argument specifies the fraction of rows to return in the random sample, so `frac=1` means return all
rows (in random order).

> Note: If you wish to shuffle your dataframe in-place and reset the index, you could do e.g.

```
df = df.sample(frac=1).reset_index(drop=True)
```

Here, specifying `drop=True` prevents `.reset_index` from creating a column containing the old index entries.
