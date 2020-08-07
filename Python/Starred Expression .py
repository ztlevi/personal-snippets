From docs.python.org/dev/reference/expressions.html#calls

Starred Expression:

If the syntax *expression appears in the function call, expression must evaluate to an iterable. Elements from these iterables are treated as if they were additional positional arguments. For the call f(x1, x2, *y, x3, x4), if y evaluates to a sequence y1, ..., yM, this is equivalent to a call with M+4 positional arguments x1, x2, y1, ..., yM, x3, x4.
---

>>> def f(a, b):
...     print(a, b)
...
>>> f(b=1, *(2,))
2 1
>>> f(a=1, *(2,))
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: f() got multiple values for keyword argument 'a'
>>> f(1, *(2,))
1 2
