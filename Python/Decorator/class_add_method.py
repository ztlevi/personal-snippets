#!/usr/bin/env python3

from functools import \
    wraps  # This convenience func preserves name and docstring


class A:
    pass


def add_method(cls):
    def decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            return func(*args, **kwargs)

        setattr(cls, func.__name__, wrapper)
        # Note we are not binding func, but wrapper which accepts self but does exactly the same as func
        return func  # returning func means func can still be used normally

    return decorator


# No trickery. Class A has no methods nor variables.
a = A()
try:
    a.foo()
except AttributeError as ae:
    print(f'Exception caught: {ae}')  # 'A' object has no attribute 'foo'

try:
    a.bar('The quick brown fox jumped over the lazy dog.')
except AttributeError as ae:
    print(f'Exception caught: {ae}')  # 'A' object has no attribute 'bar'
