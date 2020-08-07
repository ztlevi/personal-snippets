# pytest fixtures: explicit, modular, scalable[¶](https://docs.pytest.org/en/latest/fixture.html#pytest-fixtures-explicit-modular-scalable "Permalink to this headline")

The [purpose of test fixtures](http://en.wikipedia.org/wiki/Test_fixture#Software) is to provide a fixed baseline upon
which tests can reliably and repeatedly execute. pytest fixtures offer dramatic improvements over the classic xUnit
style of setup/teardown functions:

- fixtures have explicit names and are activated by declaring their use from test functions, modules, classes or whole
  projects.
- fixtures are implemented in a modular manner, as each fixture name triggers a _fixture function_ which can itself use
  other fixtures.
- fixture management scales from simple unit to complex functional testing, allowing to parametrize fixtures and tests
  according to configuration and component options, or to re-use fixtures across function, class, module or whole test
  session scopes.

In addition, pytest continues to support
[classic xunit-style setup](https://docs.pytest.org/en/latest/xunit_setup.html#xunitsetup). You can mix both styles,
moving incrementally from classic to new style, as you prefer. You can also start out from existing
[unittest.TestCase style](https://docs.pytest.org/en/latest/unittest.html#unittest-testcase) or
[nose based](https://docs.pytest.org/en/latest/nose.html#nosestyle) projects.

## Fixtures as Function arguments[¶](https://docs.pytest.org/en/latest/fixture.html#fixtures-as-function-arguments "Permalink to this headline")

Test functions can receive fixture objects by naming them as an input argument. For each argument name, a fixture
function with that name provides the fixture object. Fixture functions are registered by marking them with
`@pytest.fixture`. Let’s look at a simple self-contained test module containing a fixture and a test function using it:

```python
# content of ./test_smtpsimple.py
import pytest
@pytest.fixture
def smtp_connection():
    import smtplib

    return smtplib.SMTP("smtp.gmail.com", 587, timeout=5)

def test_ehlo(smtp_connection):
    response, msg = smtp_connection.ehlo()
    assert response == 250
    assert 0  # for demo purposes
```

Here, the `test_ehlo` needs the `smtp_connection` fixture value. pytest will discover and call the `@pytest.fixture`
marked `smtp_connection` fixture function. Running the test looks like this:

```python
$ pytest test_smtpsimple.py
=========================== test session starts ============================
platform linux -- Python 3.x.y, pytest-5.x.y, py-1.x.y, pluggy-0.x.y
cachedir: $PYTHON_PREFIX/.pytest_cache
rootdir: $REGENDOC_TMPDIR
collected 1 item

test_smtpsimple.py F                                                 [100%]

================================= FAILURES =================================
________________________________ test_ehlo _________________________________

smtp_connection = <smtplib.SMTP object at 0xdeadbeef>

    def test_ehlo(smtp_connection):
        response, msg = smtp_connection.ehlo()
        assert response == 250
>       assert 0  # for demo purposes
E       assert 0

test_smtpsimple.py:14: AssertionError
============================ 1 failed in 0.12s =============================
```

In the failure traceback we see that the test function was called with a `smtp_connection` argument, the
`smtplib.SMTP()` instance created by the fixture function. The test function fails on our deliberate `assert 0`. Here is
the exact protocol used by `pytest` to call the test function this way:

1.  pytest [finds](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery) the `test_ehlo` because of the
    `test_` prefix. The test function needs a function argument named `smtp_connection`. A matching fixture function is
    discovered by looking for a fixture-marked function named `smtp_connection`.
2.  `smtp_connection()` is called to create an instance.
3.  `test_ehlo(<smtp_connection instance>)` is called and fails in the last line of the test function.

Note that if you misspell a function argument or want to use one that isn’t available, you’ll see an error with a list
of available function arguments.

> Note
>
> You can always issue:
>
> ```
> pytest --fixtures test_simplefactory.py
> ```
>
> to see available fixtures (fixtures with leading `_` are only shown if you add the `-v` option).

## Fixtures: a prime example of dependency injection[¶](https://docs.pytest.org/en/latest/fixture.html#fixtures-a-prime-example-of-dependency-injection "Permalink to this headline")

Fixtures allow test functions to easily receive and work against specific pre-initialized application objects without
having to care about import/setup/cleanup details. It’s a prime example of
[dependency injection](http://en.wikipedia.org/wiki/Dependency_injection) where fixture functions take the role of the
_injector_ and test functions are the _consumers_ of fixture objects.

## `conftest.py`: sharing fixture functions[¶](https://docs.pytest.org/en/latest/fixture.html#conftest-py-sharing-fixture-functions "Permalink to this headline")

If during implementing your tests you realize that you want to use a fixture function from multiple test files you can
move it to a `conftest.py` file. You don’t need to import the fixture you want to use in a test, it automatically gets
discovered by pytest. The discovery of fixture functions starts at test classes, then test modules, then `conftest.py`
files and finally builtin and third party plugins.

You can also use the `conftest.py` file to implement
[local per-directory plugins](https://docs.pytest.org/en/latest/writing_plugins.html#conftest-py-plugins).

## Sharing test data[¶](https://docs.pytest.org/en/latest/fixture.html#sharing-test-data "Permalink to this headline")

If you want to make test data from files available to your tests, a good way to do this is by loading these data in a
fixture for use by your tests. This makes use of the automatic caching mechanisms of pytest.

Another good approach is by adding the data files in the `tests` folder. There are also community plugins available to
help managing this aspect of testing, e.g. [pytest-datadir](https://pypi.org/project/pytest-datadir/) and
[pytest-datafiles](https://pypi.org/project/pytest-datafiles/).

## Scope: sharing a fixture instance across tests in a class, module or session[¶](https://docs.pytest.org/en/latest/fixture.html#scope-sharing-a-fixture-instance-across-tests-in-a-class-module-or-session "Permalink to this headline")

Fixtures requiring network access depend on connectivity and are usually time-expensive to create. Extending the
previous example, we can add a `scope="module"` parameter to the `@pytest.fixture` invocation to cause the decorated
`smtp_connection` fixture function to only be invoked once per test _module_ (the default is to invoke once per test
_function_). Multiple test functions in a test module will thus each receive the same `smtp_connection` fixture
instance, thus saving time. Possible values for `scope` are: `function`, `class`, `module`, `package` or `session`.

The next example puts the fixture function into a separate `conftest.py` file so that tests from multiple test modules
in the directory can access the fixture function:

```python
# content of conftest.py
import pytest
import smtplib

@pytest.fixture(scope="module")
def smtp_connection():
    return smtplib.SMTP("smtp.gmail.com", 587, timeout=5)
```

The name of the fixture again is `smtp_connection` and you can access its result by listing the name `smtp_connection`
as an input parameter in any test or fixture function (in or below the directory where `conftest.py` is located):

```python
# content of test_module.pydef test_ehlo(smtp_connection):
    response, msg = smtp_connection.ehlo()
    assert response == 250
    assert b"smtp.gmail.com" in msg
    assert 0  # for demo purposes

def test_noop(smtp_connection):
    response, msg = smtp_connection.noop()
    assert response == 250
    assert 0  # for demo purposes
```

We deliberately insert failing `assert 0` statements in order to inspect what is going on and can now run the tests:

```python
$ pytest test_module.py
=========================== test session starts ============================
platform linux -- Python 3.x.y, pytest-5.x.y, py-1.x.y, pluggy-0.x.y
cachedir: $PYTHON_PREFIX/.pytest_cache
rootdir: $REGENDOC_TMPDIR
collected 2 items

test_module.py FF                                                    [100%]

================================= FAILURES =================================
________________________________ test_ehlo _________________________________

smtp_connection = <smtplib.SMTP object at 0xdeadbeef>

    def test_ehlo(smtp_connection):
        response, msg = smtp_connection.ehlo()
        assert response == 250
        assert b"smtp.gmail.com" in msg
>       assert 0  # for demo purposes
E       assert 0

test_module.py:7: AssertionError
________________________________ test_noop _________________________________

smtp_connection = <smtplib.SMTP object at 0xdeadbeef>

    def test_noop(smtp_connection):
        response, msg = smtp_connection.noop()
        assert response == 250
>       assert 0  # for demo purposes
E       assert 0

test_module.py:13: AssertionError
============================ 2 failed in 0.12s =============================
```

You see the two `assert 0` failing and more importantly you can also see that the same (module-scoped) `smtp_connection`
object was passed into the two test functions because pytest shows the incoming argument values in the traceback. As a
result, the two test functions using `smtp_connection` run as quick as a single one because they reuse the same
instance.

If you decide that you rather want to have a session-scoped `smtp_connection` instance, you can simply declare it:

```python
@pytest.fixture(scope="session")
def smtp_connection():
    # the returned fixture value will be shared for
    # all tests needing it
    ...
```

Finally, the `class` scope will invoke the fixture once per test _class_.

> Note:
>
> Pytest will only cache one instance of a fixture at a time. This means that when using a parametrized fixture, pytest
> may invoke a fixture more than once in the given scope.

## Order: Higher-scoped fixtures are instantiated first[¶](https://docs.pytest.org/en/latest/fixture.html#order-higher-scoped-fixtures-are-instantiated-first "Permalink to this headline")

Within a function request for features, fixture of higher-scopes (such as `session`) are instantiated first than
lower-scoped fixtures (such as `function` or `class`). The relative order of fixtures of same scope follows the declared
order in the test function and honours dependencies between fixtures. Autouse fixtures will be instantiated before
explicitly used fixtures.

Consider the code below:

```python
import pytest

# fixtures documentation order example
order= []

@pytest.fixture(scope="session")
def s1():
    order.append("s1")

@pytest.fixture(scope="module")
def m1():
    order.append("m1")

@pytest.fixture
def f1(f3):
    order.append("f1")

@pytest.fixture
def f3():
    order.append("f3")

@pytest.fixture(autouse=True)
def a1():
    order.append("a1")

@pytest.fixture
def f2():
    order.append("f2")

def test_order(f1, m1, f2, s1):
    assert order == ["s1", "m1", "a1", "f3", "f1", "f2"]
```

The fixtures requested by `test_order` will be instantiated in the following order:

1.  `s1`: is the highest-scoped fixture (`session`).
2.  `m1`: is the second highest-scoped fixture (`module`).
3.  `a1`: is a `function`-scoped `autouse` fixture: it will be instantiated before other fixtures within the same scope.
4.  `f3`: is a `function`-scoped fixture, required by `f1`: it needs to be instantiated at this point
5.  `f1`: is the first `function`-scoped fixture in `test_order` parameter list.
6.  `f2`: is the last `function`-scoped fixture in `test_order` parameter list.

## Using fixtures from classes, modules or projects[¶](https://docs.pytest.org/en/latest/fixture.html#using-fixtures-from-classes-modules-or-projects "Permalink to this headline")

Sometimes test functions do not directly need access to a fixture object. For example, tests may require to operate with
an empty directory as the current working directory but otherwise do not care for the concrete directory. Here is how
you can use the standard [tempfile](http://docs.python.org/library/tempfile.html) and pytest fixtures to achieve it. We
separate the creation of the fixture into a conftest.py file:

```python
# content of conftest.py

import pytest
import tempfile
import os

@pytest.fixture()
def cleandir():
    newpath = tempfile.mkdtemp()
    os.chdir(newpath)
```

and declare its use in a test module via a `usefixtures` marker:

```python
# content of test_setenv.py
import os
import pytest

@pytest.mark.usefixtures("cleandir")
class TestDirectoryInit:
    def test_cwd_starts_empty(self):
        assert os.listdir(os.getcwd()) == []
        with open("myfile", "w") as f:
            f.write("hello")

    def test_cwd_again_starts_empty(self):
        assert os.listdir(os.getcwd()) == []
```

Due to the `usefixtures` marker, the `cleandir` fixture will be required for the execution of each test method, just as
if you specified a “cleandir” function argument to each of them. Let’s run it to verify our fixture is activated and the
tests pass:

```sh
$ pytest -q
..                                                                   [100%]
2 passed in 0.01s
```

You can specify multiple fixtures like this:

```python
@pytest.mark.usefixtures("cleandir", "anotherfixture")
def test():
    ...
```

and you may specify fixture usage at the test module level, using a generic feature of the mark mechanism:

```python
pytestmark = pytest.mark.usefixtures("cleandir")
```

Note that the assigned variable _must_ be called `pytestmark`, assigning e.g. `foomark` will not activate the fixtures.

It is also possible to put fixtures required by all tests in your project into an ini-file:

```
# content of pytest.ini
[pytest]
usefixtures = cleandir
```

Warning

Note this mark has no effect in **fixture functions**. For example, this **will not work as expected**:

```python
@pytest.mark.usefixtures("my_other_fixture")
@pytest.fixture
def my_fixture_that_sadly_wont_use_my_other_fixture():
    ...
```

Currently this will not generate any error or warning, but this is intended to be handled by
[#3664](https://github.com/pytest-dev/pytest/issues/3664).

## Pytest request.params and getfixturevalue

Combine fixtures so that we can treat it as a single element rather have a for loop iterate over fixtures.

```python
import pytest

@pytest.fixture
def fixture1():
    return {1: 1}

@pytest.fixture
def fixture2():
    return {2: 2}

@pytest.fixture(params=["fixture1", "fixture2"])
def test_fixture(request):
    return request.getfixturevalue(request.param)

def test_case(test_fixture):
    print(test_fixture)
```

Output:

```sh
➜ pytest /Users/ztlevi/Developer/python-test/test.py
======================================= test session starts ========================================
platform darwin -- Python 3.7.7, pytest-5.4.1, py-1.8.1, pluggy-0.13.1
rootdir: /Users/ztlevi
collected 2 items
Developer/python-test/test.py ..                                                             [100%]
======================================== 2 passed in 0.01s =========================================
```

## Continue

https://docs.pytest.org/en/latest/fixture.html#fixture-finalization-executing-teardown-code
