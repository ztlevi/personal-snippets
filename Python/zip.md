# Zip

zip() should only be used with unequal length inputs when you don’t care about trailing, unmatched
values from the longer iterables. If those values are important, use itertools.zip_longest()
instead.

zip 将 axis=0 和 axis=1 对换，其余不变

```python
a = [1, 2, 3]
b = [4, 5, 6]
c = [7, 8, 9]
print(list(zip(a, b, c)))
# [(1, 4, 7), (2, 5, 8), (3, 6, 9)]
print(list(zip(*[a, b, c])))
# [(1, 4, 7), (2, 5, 8), (3, 6, 9)]
```

### transpose matrix

```python
# board is a 2D matrix
# transpose matrix
list(zip(*board))

# rotate clockwise 90 degree
board=list(map(list,zip(*reversed(board))))

# rotate counter-clockwise 90 degree
board=list(reversed(list(map(list,zip(*board)))))
```
