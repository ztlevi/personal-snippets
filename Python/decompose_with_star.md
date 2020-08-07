# Destruct

# `*` and `**`

```python
a = [1, 2, 3]
print(*a)
c = {'a':2, 'b':4}
d = {'c':3}
print({**c, **d})
```

## Inside a function header:

- `*` collects all the positional arguments in a tuple.

- `**` collects all the keyword arguments in a dictionary.

```python
>>> def functionA(*a, **kw):
       print(a)
       print(kw)

>>> functionA(1, 2, 3, 4, 5, 6, a=2, b=3, c=5)
(1, 2, 3, 4, 5, 6)
{'a': 2, 'c': 5, 'b': 3}
```

## In a function call:

- `*` unpacks a list or tuple into position arguments.

- `**` unpacks a dictionary into keyword arguments.

```python
>>> lis=[1, 2, 3, 4]
>>> dic={'a': 10, 'b':20}
>>> functionA(*lis, **dic)  #it is similar to functionA(1, 2, 3, 4, a=10, b=20)
(1, 2, 3, 4)
{'a': 10, 'b': 20}
```
