## `functools.partial(func, *args, **keywords)`

Return a new [partial object](https://docs.python.org/3/library/functools.html#partial-objects) which when called will
behave like _func_ called with the positional arguments _args_ and keyword arguments _keywords_. If more arguments are
supplied to the call, they are appended to _args_. If additional keyword arguments are supplied, they extend and
override _keywords_. Roughly equivalent to:

```python
def partial(func,*args, **keywords):
    def newfunc(*fargs, **fkeywords):
        newkeywords = keywords.copy()
        newkeywords.update(fkeywords)
        return func(*args, *fargs, **newkeywords)
    newfunc.func = func
    newfunc.args = args
    newfunc.keywords = keywords
    return newfunc
```

The [`partial()`](https://docs.python.org/3/library/functools.html#functools.partial "functools.partial") is used for
partial function application which “freezes” some portion of a function’s arguments and/or keywords resulting in a new
object with a simplified signature. For example,
[`partial()`](https://docs.python.org/3/library/functools.html#functools.partial "functools.partial") can be used to
create a callable that behaves like the [`int()`](https://docs.python.org/3/library/functions.html#int "int") function
where the _base_ argument defaults to two:

```python
>>> fromfunctools import partial
>>> basetwo = partial(int, base=2)
>>> basetwo.__doc__ = 'Convert base 2 string to an int.'
>>> basetwo('10010')
18
```

```python
from functools import partial

def multiply(x,y):
        return x * y

# create a new function that multiplies by 2
dbl = partial(multiply,2)
print(dbl(4))
```

## `class functools.partialmethod(func, *args, **keywords)`

Return a new
[`partialmethod`](https://docs.python.org/3/library/functools.html#functools.partialmethod "functools.partialmethod")
descriptor which behaves like
[`partial`](https://docs.python.org/3/library/functools.html#functools.partial "functools.partial") except that it is
designed to be used as a method definition rather than being directly callable.

_func_ must be a [descriptor](https://docs.python.org/3/glossary.html#term-descriptor) or a callable (objects which are
both, like normal functions, are handled as descriptors).

When _func_ is a descriptor (such as a normal Python function,
[`classmethod()`](https://docs.python.org/3/library/functions.html#classmethod "classmethod"),
[`staticmethod()`](https://docs.python.org/3/library/functions.html#staticmethod "staticmethod"), `abstractmethod()` or
another instance of
[`partialmethod`](https://docs.python.org/3/library/functools.html#functools.partialmethod "functools.partialmethod")),
calls to `__get__` are delegated to the underlying descriptor, and an appropriate
[partial object](https://docs.python.org/3/library/functools.html#partial-objects) returned as the result.

When _func_ is a non-descriptor callable, an appropriate bound method is created dynamically. This behaves like a normal
Python function when used as a method: the _self_ argument will be inserted as the first positional argument, even
before the _args_ and _keywords_ supplied to the
[`partialmethod`](https://docs.python.org/3/library/functools.html#functools.partialmethod "functools.partialmethod")
constructor.

Example:

> > >

```python
>>> classCell(object):
... def __init__(self):
...         self._alive = False
...     @property
...     def alive(self):
...         return self._alive
...     def set_state(self, state):
...         self._alive = bool(state)
...     set_alive = partialmethod(set_state, True)
...     set_dead = partialmethod(set_state, False)
...
>>> c = Cell()
>>> c.alive
False
>>> c.set_alive()
>>> c.alive
True
```

New in version 3.4.
