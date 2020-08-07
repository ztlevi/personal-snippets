# Dataclasses

https://docs.python.org/3/library/dataclasses.html

## Module-level decorators, classes, and functions

```python
from dataclasses import dataclass, field, asdict
@dataclasses.dataclass(*, init=True, repr=True, eq=True, order=False, unsafe_hash=False, frozen=False)¶
```

## Field

```python
@dataclass
class C:
    mylist: List[int] = field(default_factory=list)
    myset: Set[int] = field(default_factory=lambda : set())

c = C()
c.mylist += [1, 2, 3]
c.myset.add(1)

@dataclass
class C:
    a: float
    b: float
    c: float = field(init=False)

    def __post_init__(self):
        self.c = self.a + self.b
c = C(a=1, b=2)
c.c = 4
```

```python
from dataclasses import asdict, dataclass, field


@dataclass
class C:
    x: int
    y: int = field(repr=False)
    z: int = field(repr=False, default=10)

a = C(x=1, y=2)
print(a)
print(asdict(a))

>> C(x=1)
>> {'x': 1, 'y': 2, 'z': 10}
```

## Asdict & astuple

```python
@dataclass
class Point:
     x: int
     y: int

@dataclass
class C:
     mylist: List[Point]

p = Point(10, 20)
assert asdict(p) == {'x': 10, 'y': 20}

c = C([Point(0, 0), Point(10, 4)])
assert asdict(c) == {'mylist': [{'x': 0, 'y': 0}, {'x': 10, 'y': 4}]}
```

## dataclasses.make_dataclass

```python
C = make_dataclass('C',
                   [('x', int),
                     'y',
                    ('z', int, field(default=5))],
                   namespace={'add_one': lambda self: self.x + 1})
```

is equivalent to

```python
@dataclass
class C:
    x: int
    y: 'typing.Any'
    z: int = 5

    def add_one(self):
        return self.x + 1
```

## Init-only variables[¶](https://docs.python.org/3/library/dataclasses.html#init-only-variables "Permalink to this headline")

The other place where
[`dataclass()`](https://docs.python.org/3/library/dataclasses.html#dataclasses.dataclass "dataclasses.dataclass")
inspects a type annotation is to determine if a field is an init-only variable. It does this by seeing if the type of a
field is of type `dataclasses.InitVar`. If a field is an `InitVar`, it is considered a pseudo-field called an init-only
field. As it is not a true field, it is not returned by the module-level
[`fields()`](https://docs.python.org/3/library/dataclasses.html#dataclasses.fields "dataclasses.fields") function.
Init-only fields are added as parameters to the generated
[`__init__()`](https://docs.python.org/3/reference/datamodel.html#object.__init__ "object.__init__") method, and are
passed to the optional `__post_init__()` method. They are not otherwise used by dataclasses.

For example, suppose a field will be initialized from a database, if a value is not provided when creating the class:

```python
@dataclassclass C:
    i: int
    j: int = None
    database: InitVar[DatabaseType] = None

    def __post_init__(self, database):
        if self.j is None and database is not None:
            self.j = database.lookup('j')

c = C(10, database=my_database)
```

In this case, [`fields()`](https://docs.python.org/3/library/dataclasses.html#dataclasses.fields "dataclasses.fields")
will return [`Field`](https://docs.python.org/3/library/dataclasses.html#dataclasses.Field "dataclasses.Field") objects
for `i` and `j`, but not for `database`.

## Frozen instances[¶](https://docs.python.org/3/library/dataclasses.html#frozen-instances "Permalink to this headline")

It is not possible to create truly immutable Python objects. However, by passing `frozen=True` to the
[`dataclass()`](https://docs.python.org/3/library/dataclasses.html#dataclasses.dataclass "dataclasses.dataclass")
decorator you can emulate immutability. In that case, dataclasses will add
[`__setattr__()`](https://docs.python.org/3/reference/datamodel.html#object.__setattr__ "object.__setattr__") and
[`__delattr__()`](https://docs.python.org/3/reference/datamodel.html#object.__delattr__ "object.__delattr__") methods to
the class. These methods will raise a
[`FrozenInstanceError`](https://docs.python.org/3/library/dataclasses.html#dataclasses.FrozenInstanceError "dataclasses.FrozenInstanceError")
when invoked.

There is a tiny performance penalty when using `frozen=True`:
[`__init__()`](https://docs.python.org/3/reference/datamodel.html#object.__init__ "object.__init__") cannot use simple
assignment to initialize fields, and must use
[`object.__setattr__()`](https://docs.python.org/3/reference/datamodel.html#object.__setattr__ "object.__setattr__").

## Default factory functions

```python
mylist: list = field(default_factory=list)
```

### Mutable default values

```python
class C:
    x = []
    def add(self, element):
        self.x.append(element)

o1 = C()
o2 = C()
o1.add(1)
o2.add(2)
assert o1.x == [1, 2]
assert o1.x is o2.x
```

Note that the two instances of class `C` share the same class variable `x`, as expected.

This has the same issue as the original example using class `C`. That is, two instances of class `D` that do not specify
a value for `x` when creating a class instance will share the same copy of `x`. Because dataclasses just use normal
Python class creation they also share this behavior. There is no general way for Data Classes to detect this condition.
Instead, dataclasses will raise a [`TypeError`](https://docs.python.org/3/library/exceptions.html#TypeError "TypeError")
if it detects a default parameter of type `list`, `dict`, or `set`. This is a partial solution, but it does protect
against many common errors.

Using default factory functions is a way to create new instances of mutable types as default values for fields:

```python
@dataclass
class D:
    x: list = field(default_factory=list)

assert D().x is not D().x
```
