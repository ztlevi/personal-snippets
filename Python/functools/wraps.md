## `functools.update_wrapper(wrapper, wrapped, assigned=WRAPPER_ASSIGNMENTS, updated=WRAPPER_UPDATES)`

Update a wrapper function to look like the wrapped function. The optional arguments are tuples to specify which
attributes of the original function are assigned directly to the matching attributes on the wrapper function and which
attributes of the wrapper function are updated with the corresponding attributes from the original function. The default
values for these arguments are the module level constants WRAPPER_ASSIGNMENTS (which assigns to the wrapper function’s
`__module__`, `__name__`, `__qualname__`, `__annotations__` and `__doc__`, the documentation string) and
`WRAPPER_UPDATES` (which updates the wrapper function’s `__dict__`, i.e. the instance dictionary).

To allow access to the original function for introspection and other purposes (e.g. bypassing a caching decorator such
as lru_cache()), this function automatically adds a `__wrapped__` attribute to the wrapper that refers to the function
being wrapped.

The main intended use for this function is in decorator functions which wrap the decorated function and return the
wrapper. If the wrapper function is not updated, the metadata of the returned function will reflect the wrapper
definition rather than the original function definition, which is typically less than helpful.

`update_wrapper()` may be used with callables other than functions. Any attributes named in assigned or updated that are
missing from the object being wrapped are ignored (i.e. this function will not attempt to set them on the wrapper
function). AttributeError is still raised if the wrapper function itself is missing any attributes named in updated.

New in version 3.2: Automatic addition of the `__wrapped__` attribute.

New in version 3.2: Copying of the `__annotations__` attribute by default.

Changed in version 3.2: Missing attributes no longer trigger an AttributeError.

Changed in version 3.4: The `__wrapped__` attribute now always refers to the wrapped function, even if that function
defined a `__wrapped__` attribute. (see bpo-17482)

## `@functools.wraps(wrapped, assigned=WRAPPER_ASSIGNMENTS, updated=WRAPPER_UPDATES)`

This is a convenience function for invoking `update_wrapper()` as a function decorator when defining a wrapper function.
It is equivalent to partial(update_wrapper, wrapped=wrapped, assigned=assigned, updated=updated). For example:

```python
>>>
>>> from functools import wraps
>>> def my_decorator(f):
...     @wraps(f)
...     def wrapper(*args, **kwds):
...         print('Calling decorated function')
...         return f(*args, **kwds)
...     return wrapper
...
>>> @my_decorator
... def example():
...     """Docstring"""
...     print('Called example function')
...
>>> example()
Calling decorated function
Called example function
>>> example.__name__
'example'
>>> example.__doc__
'Docstring'
```

Without the use of this decorator factory, the name of the example function would have been 'wrapper', and the docstring
of the original `example()` would have been lost.
