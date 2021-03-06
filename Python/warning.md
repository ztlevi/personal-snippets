# Warning https://docs.python.org/3/library/warnings.html

## Temporarily Suppressing Warnings

If you are using code that you know will raise a warning, such as a deprecated function, but do not
want to see the warning (even when warnings have been explicitly configured via the command line),
then it is possible to suppress the warning using the catch_warnings context manager:

```python
import warnings

def fxn():
    warnings.warn("deprecated", DeprecationWarning)

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    fxn()
```

While within the context manager all warnings will simply be ignored. This allows you to use
known-deprecated code without having to see the warning while not suppressing the warning for other
code that might not be aware of its use of deprecated code. Note: this can only be guaranteed in a
single-threaded application. If two or more threads use the catch_warnings context manager at the
same time, the behavior is undefined.
