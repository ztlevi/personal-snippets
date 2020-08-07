## Install `pytest`[¶](https://docs.pytest.org/en/latest/getting-started.html#install-pytest "Permalink to this headline")

1.  Run the following command in your command line:

```sh
pip install -U pytest
```

1.  Check that you installed the correct version:

```sh
$ pytest --version
This is pytest version 5.x.y, imported from $PYTHON_PREFIX/lib/python3.6/site-packages/pytest.py
```

## Create your first test[¶](https://docs.pytest.org/en/latest/getting-started.html#create-your-first-test "Permalink to this headline")

Create a simple test function with just four lines of code:

```python
# content of test_sample.py
def func(x):
    return x + 1

def test_answer():
    assert func(3) == 5
```

That’s it. You can now execute the test function:

```sh
$ pytest
=========================== test session starts ============================
platform linux -- Python 3.x.y, pytest-5.x.y, py-1.x.y, pluggy-0.x.y
cachedir: $PYTHON_PREFIX/.pytest_cache
rootdir: $REGENDOC_TMPDIR
collected 1 item

test_sample.py F                                                     [100%]

================================= FAILURES =================================
_______________________________ test_answer ________________________________

    def test_answer():
>       assert func(3) == 5
E       assert 4 == 5
E        +  where 4 = func(3)

test_sample.py:6: AssertionError
============================ 1 failed in 0.12s =============================
```

This test returns a failure report because `func(3)` does not return `5`.

Note

You can use the `assert` statement to verify test expectations. pytest’s
[Advanced assertion introspection](http://docs.python.org/reference/simple_stmts.html#the-assert-statement) will
intelligently report intermediate values of the assert expression so you can avoid the many names
[of JUnit legacy methods](http://docs.python.org/library/unittest.html#test-cases).

## Run multiple tests[¶](https://docs.pytest.org/en/latest/getting-started.html#run-multiple-tests "Permalink to this headline")

`pytest` will run all files of the form test\__.py or _\_test.py in the current directory and its subdirectories. More
generally, it follows
[standard test discovery rules](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery).

## Assert that a certain exception is raised[¶](https://docs.pytest.org/en/latest/getting-started.html#assert-that-a-certain-exception-is-raised "Permalink to this headline")

Use the [raises](https://docs.pytest.org/en/latest/assert.html#assertraises) helper to assert that some code raises an
exception:

```python
# content of test_sysexit.py
import pytest

def f():
    raise SystemExit(1)

def test_mytest():
    with pytest.raises(SystemExit):
        f()
```

Execute the test function with “quiet” reporting mode:

```sh
$ pytest -q test_sysexit.py
.                                                                    [100%]
1 passed in 0.01s
```

## Group multiple tests in a class[¶](https://docs.pytest.org/en/latest/getting-started.html#group-multiple-tests-in-a-class "Permalink to this headline")

Once you develop multiple tests, you may want to group them into a class. pytest makes it easy to create a class
containing more than one test:

```python
# content of test_class.py
classTestClass:
    def test_one(self):
        x = "this"
        assert "h" in x

    def test_two(self):
        x = "hello"
        assert hasattr(x, "check")
```

`pytest` discovers all tests following its
[Conventions for Python test discovery](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery), so it
finds both `test_` prefixed functions. There is no need to subclass anything. We can simply run the module by passing
its filename:

```sh
$ pytest -q test_class.py
.F                                                                   [100%]
================================= FAILURES =================================
____________________________ TestClass.test_two ____________________________

self = <test_class.TestClass object at 0xdeadbeef>

    def test_two(self):
        x = "hello"
>       assert hasattr(x, "check")
E       AssertionError: assert False
E        +  where False = hasattr('hello', 'check')

test_class.py:8: AssertionError
1 failed, 1 passed in 0.02s
```

The first test passed and the second failed. You can easily see the intermediate values in the assertion to help you
understand the reason for the failure.

## Request a unique temporary directory for functional tests[¶](https://docs.pytest.org/en/latest/getting-started.html#request-a-unique-temporary-directory-for-functional-tests "Permalink to this headline")

`pytest` provides [Builtin fixtures/function arguments](https://docs.pytest.org/en/latest/builtin.html) to request
arbitrary resources, like a unique temporary directory:

```sh
\# content of test_tmpdir.py
def test_needsfiles(tmpdir):
    print(tmpdir)
    assert 0
```

List the name `tmpdir` in the test function signature and `pytest` will lookup and call a fixture factory to create the
resource before performing the test function call. Before the test runs, `pytest` creates a unique-per-test-invocation
temporary directory:

```sh
$ pytest -q test_tmpdir.py
F                                                                    [100%]
================================= FAILURES =================================
_____________________________ test_needsfiles ______________________________

tmpdir = local('PYTEST_TMPDIR/test_needsfiles0')

    def test_needsfiles(tmpdir):
        print(tmpdir)
>       assert 0
E       assert 0

test_tmpdir.py:3: AssertionError
--------------------------- Captured stdout call ---------------------------
PYTEST_TMPDIR/test_needsfiles0
1 failed in 0.02s
```

More info on tmpdir handling is available at
[Temporary directories and files](https://docs.pytest.org/en/latest/tmpdir.html#tmpdir-handling).

Find out what kind of builtin [pytest fixtures](https://docs.pytest.org/en/latest/fixture.html#fixtures) exist with the
command:

```sh
pytest --fixtures   \# shows builtin and custom fixtures
```

Note that this command omits fixtures with leading `_` unless the `-v` option is added.
