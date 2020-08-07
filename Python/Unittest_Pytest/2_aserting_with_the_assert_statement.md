## Asserting with the `assert` statement[¶](https://docs.pytest.org/en/latest/assert.html#asserting-with-the-assert-statement "Permalink to this headline")

`pytest` allows you to use the standard python `assert` for verifying expectations and values in Python tests. For
example, you can write the following:

```python
# content of test_assert1.py
def f():
    return 3

def test_function():
    assert f() == 4
```

to assert that your function returns a certain value. If this assertion fails you will see the return value of the
function call:

```sh
$ pytest test_assert1.py
=========================== test session starts ============================
platform linux -- Python 3.x.y, pytest-5.x.y, py-1.x.y, pluggy-0.x.y
cachedir: $PYTHON_PREFIX/.pytest_cache
rootdir: $REGENDOC_TMPDIR
collected 1 item

test_assert1.py F                                                    [100%]

================================= FAILURES =================================
______________________________ test_function _______________________________

    def test_function():
>       assert f() == 4
E       assert 3 == 4
E        +  where 3 = f()

test_assert1.py:6: AssertionError
============================ 1 failed in 0.12s =============================
```

`pytest` has support for showing the values of the most common subexpressions including calls, attributes, comparisons,
and binary and unary operators. (See
[Demo of Python failure reports with pytest](https://docs.pytest.org/en/latest/example/reportingdemo.html#tbreportdemo)).
This allows you to use the idiomatic python constructs without boilerplate code while not losing introspection
information.

However, if you specify a message with the assertion like this:

```python
assert a % 2 == 0, "value was odd, should be even"
```

then no assertion introspection takes places at all and the message will be simply shown in the traceback.

See [Assertion introspection details](https://docs.pytest.org/en/latest/assert.html#assert-details) for more information
on assertion introspection.

## Assertions about expected exceptions[¶](https://docs.pytest.org/en/latest/assert.html#assertions-about-expected-exceptions "Permalink to this headline")

In order to write assertions about raised exceptions, you can use `pytest.raises` as a context manager like this:

```python
import pytest

def test_zero_division():
    with pytest.raises(ZeroDivisionError):
        1 / 0
```

and if you need to have access to the actual exception info you may use:

```python
def test_recursion_depth():
    with pytest.raises(RuntimeError) as excinfo:

        def f():
            f()

        f()
    assert "maximum recursion" in str(excinfo.value)
```

`excinfo` is a `ExceptionInfo` instance, which is a wrapper around the actual exception raised. The main attributes of
interest are `.type`, `.value` and `.traceback`.

You can pass a `match` keyword parameter to the context-manager to test that a regular expression matches on the string
representation of an exception (similar to the `TestCase.assertRaisesRegexp` method from `unittest`):

```python
importpytest

def myfunc():
    raise ValueError("Exception 123 raised")

def test_match():
    with pytest.raises(ValueError, match=r".* 123 .*"):
        myfunc()
```

The regexp parameter of the `match` method is matched with the `re.search` function, so in the above example
`match='123'` would have worked as well.

There’s an alternate form of the `pytest.raises` function where you pass a function that will be executed with the given
`*args` and `**kwargs` and assert that the given exception is raised:

```python
pytest.raises(ExpectedException, func, *args, **kwargs)
```

The reporter will provide you with helpful output in case of failures such as _no exception_ or _wrong exception_.

Note that it is also possible to specify a “raises” argument to `pytest.mark.xfail`, which checks that the test is
failing in a more specific way than just having any exception raised:

```python
@pytest.mark.xfail(raises=IndexError)
def test_f():
    f()
```

Using `pytest.raises` is likely to be better for cases where you are testing exceptions your own code is deliberately
raising, whereas using `@pytest.mark.xfail` with a check function is probably better for something like documenting
unfixed bugs (where the test describes what “should” happen) or bugs in dependencies.

## Assertions about expected warnings[¶](https://docs.pytest.org/en/latest/assert.html#assertions-about-expected-warnings "Permalink to this headline")

You can check that code raises a particular warning using
[pytest.warns](https://docs.pytest.org/en/latest/warnings.html#warns).

## Making use of context-sensitive comparisons[¶](https://docs.pytest.org/en/latest/assert.html#making-use-of-context-sensitive-comparisons "Permalink to this headline")

`pytest` has rich support for providing context-sensitive information when it encounters comparisons. For example:

```python
# content of test_assert2.py

def test_set_comparison():
    set1 = set("1308")
    set2 = set("8035")
    assert set1 == set2
```

if you run this module:

```sh
$ pytest test_assert2.py
=========================== test session starts ============================
platform linux -- Python 3.x.y, pytest-5.x.y, py-1.x.y, pluggy-0.x.y
cachedir: $PYTHON_PREFIX/.pytest_cache
rootdir: $REGENDOC_TMPDIR
collected 1 item

test_assert2.py F                                                    [100%]

================================= FAILURES =================================
___________________________ test_set_comparison ____________________________

    def test_set_comparison():
        set1 = set("1308")
        set2 = set("8035")
>       assert set1 == set2
E       AssertionError: assert {'0', '1', '3', '8'} == {'0', '3', '5', '8'}
E         Extra items in the left set:
E         '1'
E         Extra items in the right set:
E         '5'
E         Use -v to get the full diff

test_assert2.py:6: AssertionError
============================ 1 failed in 0.12s =============================
```

Special comparisons are done for a number of cases:

- comparing long strings: a context diff is shown
- comparing long sequences: first failing indices
- comparing dicts: different entries

## Defining your own explanation for failed assertions[¶](https://docs.pytest.org/en/latest/assert.html#defining-your-own-explanation-for-failed-assertions "Permalink to this headline")

It is possible to add your own detailed explanations by implementing the `pytest_assertrepr_compare` hook.

As an example consider adding the following hook in a
[conftest.py](https://docs.pytest.org/en/latest/fixture.html#conftest-py) file which provides an alternative explanation
for `Foo` objects:

```python
# content of conftest.py
from test_foocompare import Foo

def pytest_assertrepr_compare(op, left, right):
    if isinstance(left, Foo) and isinstance(right, Foo) and op == "==":
        return [
            "Comparing Foo instances:",
            "   vals: {} != {}".format(left.val, right.val),
        ]
```

now, given this test module:

```python
# content of test_foocompare.py
class Foo:
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        return self.val == other.val

def test_compare():
    f1 = Foo(1)
    f2 = Foo(2)
    assert f1 == f2
```

you can run the test module and get the custom output defined in the conftest file:

```sh
$ pytest -q test_foocompare.py
F                                                                    [100%]
================================= FAILURES =================================
_______________________________ test_compare _______________________________

    def test_compare():
        f1 = Foo(1)
        f2 = Foo(2)
>       assert f1 == f2
E       assert Comparing Foo instances:
E            vals: 1 != 2

test_foocompare.py:12: AssertionError
1 failed in 0.02s
```

## Assertion introspection details[¶](https://docs.pytest.org/en/latest/assert.html#assertion-introspection-details "Permalink to this headline")

Reporting details about a failing assertion is achieved by rewriting assert statements before they are run. Rewritten
assert statements put introspection information into the assertion failure message. `pytest` only rewrites test modules
directly discovered by its test collection process, so **asserts in supporting modules which are not themselves test
modules will not be rewritten**.

You can manually enable assertion rewriting for an imported module by calling
[register_assert_rewrite](https://docs.pytest.org/en/latest/writing_plugins.html#assertion-rewriting) before you import
it (a good place to do that is in your root `conftest.py`).

For further information, Benjamin Peterson wrote up
[Behind the scenes of pytest’s new assertion rewriting](http://pybites.blogspot.com/2011/07/behind-scenes-of-pytests-new-assertion.html).

### Assertion rewriting caches files on disk[¶](https://docs.pytest.org/en/latest/assert.html#assertion-rewriting-caches-files-on-disk "Permalink to this headline")

`pytest` will write back the rewritten modules to disk for caching. You can disable this behavior (for example to avoid
leaving stale `.pyc` files around in projects that move files around a lot) by adding this to the top of your
`conftest.py` file:

```python
import sys

sys.dont_write_bytecode = True
```

Note that you still get the benefits of assertion introspection, the only change is that the `.pyc` files won’t be
cached on disk.

Additionally, rewriting will silently skip caching if it cannot write new `.pyc` files, i.e. in a read-only filesystem
or a zipfile.

### Disabling assert rewriting[¶](https://docs.pytest.org/en/latest/assert.html#disabling-assert-rewriting "Permalink to this headline")

`pytest` rewrites test modules on import by using an import hook to write new `pyc` files. Most of the time this works
transparently. However, if you are working with the import machinery yourself, the import hook may interfere.

If this is the case you have two options:

- Disable rewriting for a specific module by adding the string `PYTEST_DONT_REWRITE` to its docstring.

- Disable rewriting for all modules by using `--assert=plain`.

  > Add assert rewriting as an alternate introspection technique.
  >
  > Introduce the `--assert` option. Deprecate `--no-assert` and `--nomagic`.
  >
  > Removes the `--no-assert` and `--nomagic` options. Removes the `--assert=reinterp` option.
